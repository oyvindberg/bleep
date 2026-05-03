package com.monovore.decline

/** Walks decline's `Opts[A]` AST and emits a documentation-friendly model.
  *
  * Lives in `com.monovore.decline` (same package as `Completer.scala`) so it can pattern-match decline's package-private case classes (`Opts.Subcommand`,
  * `Opt.Regular`, etc.).
  */
object CliDocsWalker {

  /** A leaf or intermediate command node. */
  final case class CommandDoc(
      name: String,
      description: String,
      arguments: List[ArgumentDoc],
      flags: List[FlagDoc],
      subcommands: List[CommandDoc]
  )

  final case class ArgumentDoc(metavar: String, repeated: Boolean)

  final case class FlagDoc(
      longName: String,
      shortName: Option[String],
      metavar: Option[String], // None = boolean flag, Some = takes value
      description: String,
      repeated: Boolean,
      visibility: String // "Normal" or "Partial"
  )

  /** Walk a top-level Opts tree under a synthetic command name (e.g. "<root>"), returning every direct subcommand the tree exposes.
    */
  def topLevelSubcommands(opts: Opts[?]): List[CommandDoc] =
    walk(opts, depth = 0).subcommands

  /** Walk a single named command. The opts are the command's body, name is the command's CLI name, header is its description. Used when the caller has already
    * extracted these (e.g. from `Command[A]` directly).
    */
  def commandDoc(name: String, header: String, opts: Opts[?]): CommandDoc = {
    val collected = walk(opts, depth = 0)
    CommandDoc(name, header, collected.arguments, collected.flags, collected.subcommands)
  }

  /** Recursive collector. Returns args + flags + nested subcommands found in this Opts subtree. The `name` and `description` on the returned CommandDoc are
    * placeholders; the caller stitches them together.
    */
  private def walk(opts: Opts[?], depth: Int): CommandDoc = opts match {
    case Opts.Subcommand(cmd) =>
      val inner = walk(cmd.options, depth + 1)
      // Wrap so the discovered subcommand sits in the parent's `subcommands`
      // slot (Subcommand-shaped node IS a subcommand, not a sibling element).
      CommandDoc(
        name = "",
        description = "",
        arguments = Nil,
        flags = Nil,
        subcommands = List(
          CommandDoc(
            name = cmd.name,
            description = cmd.header,
            arguments = inner.arguments,
            flags = inner.flags,
            subcommands = inner.subcommands
          )
        )
      )

    case Opts.App(f, a) =>
      merge(walk(f, depth), walk(a, depth))

    case Opts.OrElse(a, b) =>
      merge(walk(a, depth), walk(b, depth))

    case Opts.Single(opt)        => fromOpt(opt, repeated = false)
    case Opts.Repeated(opt)      => fromOpt(opt, repeated = true)
    case Opts.Validate(value, _) => walk(value, depth)
    case Opts.Pure(_)            => empty
    case Opts.Missing            => empty
    case Opts.HelpFlag(_)        => empty
    case Opts.Env(_, _, _)       => empty
  }

  private def fromOpt(opt: Opt[?], repeated: Boolean): CommandDoc = opt match {
    case Opt.Regular(names, metavar, help, vis) =>
      CommandDoc("", "", Nil, List(flagFrom(names, Some(metavar), help, vis, repeated)), Nil)
    case Opt.Flag(names, help, vis) =>
      CommandDoc("", "", Nil, List(flagFrom(names, None, help, vis, repeated)), Nil)
    case Opt.Argument(metavar) =>
      CommandDoc("", "", List(ArgumentDoc(metavar, repeated)), Nil, Nil)
    case Opt.OptionalOptArg(names, metavar, help, vis) =>
      CommandDoc("", "", Nil, List(flagFrom(names, Some(metavar), help, vis, repeated)), Nil)
  }

  private def flagFrom(
      names: List[Opts.Name],
      metavar: Option[String],
      help: String,
      vis: Visibility,
      repeated: Boolean
  ): FlagDoc = {
    val longs = names.collect { case Opts.LongName(f) => f }
    val shorts = names.collect { case Opts.ShortName(f) => f }
    FlagDoc(
      longName = longs.headOption.getOrElse("?"),
      shortName = shorts.headOption.map(_.toString),
      metavar = metavar.filter(_.nonEmpty),
      description = help,
      repeated = repeated,
      visibility = vis match {
        case Visibility.Normal  => "Normal"
        case Visibility.Partial => "Partial"
      }
    )
  }

  private val empty = CommandDoc("", "", Nil, Nil, Nil)

  private def merge(a: CommandDoc, b: CommandDoc): CommandDoc =
    CommandDoc(
      name = "",
      description = "",
      arguments = a.arguments ++ b.arguments,
      flags = dedupFlags(a.flags ++ b.flags),
      subcommands = a.subcommands ++ b.subcommands
    )

  /** Dedup by long-name. Two pieces of the AST can mention the same flag (e.g. `--help` shows up via `Opts.HelpFlag` indirectly), keep the first.
    */
  private def dedupFlags(fs: List[FlagDoc]): List[FlagDoc] = {
    val seen = scala.collection.mutable.Set.empty[String]
    fs.filter { f =>
      if (seen(f.longName)) false
      else { seen += f.longName; true }
    }
  }

}
