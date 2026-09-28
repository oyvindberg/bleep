package bleep.model

import io.circe.{Decoder, Encoder, Json}

/** A description of javac/scalac options.
  *
  * Because some options have arguments we need to take some care when operating on them as sets
  *
  * Each option renders to the arguments the compiler gets, one string per argument, and an argument may contain spaces: `-Wconf:msg=unused value:s`, or
  * `-Xplugin:ErrorProne -Xep:NullAway:ERROR`, which javac hands to the plugin whole. The build file holds options as one string, where such an argument is
  * quoted: `"-Xplugin:ErrorProne -Xep:NullAway:ERROR"`. The quotes only mark where the argument ends, the compiler never sees them.
  */
case class Options(values: Set[Options.Opt]) extends SetLike[Options] {
  def render: List[String] = values.toList.sorted.flatMap(_.render)
  def isEmpty: Boolean = values.isEmpty

  override def union(other: Options) = new Options(values ++ other.values)
  override def intersect(other: Options) = new Options(values.intersect(other.values))
  override def removeAll(other: Options) = new Options(values -- other.values)
}

object Options {
  def fromIterable(opts: Iterable[Opt]): Options =
    new Options(opts.toSet)

  val empty = new Options(Set.empty)
  implicit val decodes: Decoder[Options] = Decoder[JsonList[String]].emap { list =>
    list.values
      .foldLeft[Either[String, List[String]]](Right(Nil)) { (acc, str) =>
        acc.flatMap(args => splitEither(str).map(args ++ _))
      }
      .map(args => fromArgs(args, maybeRelativize = None))
  }
  implicit val encodes: Encoder[Options] = Encoder.instance {
    case opts if opts.isEmpty => Json.Null
    case opts                 => Json.fromString(opts.render.map(quote).mkString(" "))
  }

  /** An argument as the build file writes it: quoted when it contains whitespace or a quote, with `\` and `"` escaped inside the quotes */
  def quote(arg: String): String =
    if (arg.nonEmpty && !arg.exists(c => c.isWhitespace || c == '"')) arg
    else "\"" + arg.replace("\\", "\\\\").replace("\"", "\\\"") + "\""

  /** Splits what the build file writes into arguments: at whitespace, but not inside double quotes. Inside quotes `\"` is a quote and `\\` a backslash, outside
    * them a backslash is just a backslash, so windows paths need no escaping
    */
  def split(str: String): List[String] =
    splitEither(str) match {
      case Right(args) => args
      case Left(err)   => throw new IllegalArgumentException(err)
    }

  def splitEither(str: String): Either[String, List[String]] = {
    val args = List.newBuilder[String]
    val current = new StringBuilder
    var inArg = false
    var inQuotes = false
    var i = 0
    while (i < str.length) {
      val c = str.charAt(i)
      if (inQuotes) {
        if (c == '\\' && i + 1 < str.length && (str.charAt(i + 1) == '"' || str.charAt(i + 1) == '\\')) {
          current += str.charAt(i + 1)
          i += 1
        } else if (c == '"') inQuotes = false
        else current += c
      } else if (c == '"') {
        inQuotes = true
        inArg = true
      } else if (c.isWhitespace) {
        if (inArg) args += current.result()
        current.clear()
        inArg = false
      } else {
        current += c
        inArg = true
      }
      i += 1
    }
    if (inQuotes) Left(s"Unterminated quote in options: $str")
    else {
      if (inArg) args += current.result()
      Right(args.result())
    }
  }

  /** Options as the build file writes them, or as they are written in a string like maven's `argLine`: split into arguments, see [[split]] */
  def parse(strings: List[String], maybeRelativize: Option[Replacements]): Options =
    fromArgs(strings.flatMap(split), maybeRelativize)

  /** Options from the arguments a compiler got, one per element, which are never split: sbt's and maven's compiler arguments */
  def fromArgs(args: List[String], maybeRelativize: Option[Replacements]): Options = {
    val relativeStrings = maybeRelativize match {
      case Some(relativize) => args.filter(_.trim.nonEmpty).map(relativize.templatize.string)
      case None             => args.filter(_.trim.nonEmpty)
    }

    val opts = relativeStrings.foldLeft(List.empty[Options.Opt]) {
      case (current :: rest, arg) if !arg.startsWith("-") => current.withArg(arg) :: rest
      case (acc, str) if str.startsWith("-")              => Opt.Flag(str) :: acc
      // revisit this
      case (_, nonMatching) => sys.error(s"unexpected ${nonMatching}")
    }

    fromIterable(opts)
  }

  sealed trait Opt {
    def render: List[String]
    def withArg(str: String): Opt.WithArgs
  }

  object Opt {
    implicit val ordering: Ordering[Opt] = Ordering.by(_.render.mkString(" "))

    case class Flag(name: String) extends Opt {
      override def render: List[String] = List(name)
      override def withArg(arg: String): WithArgs = WithArgs(name, List(arg))
    }

    case class WithArgs(name: String, args: List[String]) extends Opt {
      override def render: List[String] = name :: args
      override def withArg(arg: String): WithArgs = WithArgs(name, args :+ arg)
    }
  }
}
