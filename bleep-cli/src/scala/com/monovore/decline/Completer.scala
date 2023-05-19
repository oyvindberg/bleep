package com.monovore.decline

import com.monovore.decline.Completer.*

import scala.util.hashing.MurmurHash3

final class Completer(possibleCompletionsForMetavar: String => List[String]) {
  def completeOpts[A](args: List[String])(x: Opts[A]): Res =
    x match {
      case Opts.Subcommand(command) =>
        args match {
          case Nil =>
            Res.Found(List(Completion(command.name, nonEmpty(command.header))), 0)
          case command.name :: tail =>
            var remainingArgs = tail
            var consumed = 1
            var res: Res = Res.NoMatch
            while (remainingArgs.nonEmpty)
              completeOpts(remainingArgs)(command.options) match {
                case Res.NoMatch => return Res.NoMatch
                case matched: Res.Matched =>
                  res = matched
                  remainingArgs = remainingArgs.drop(matched.consumed)
                  consumed += matched.consumed
              }
            res.commitNonEmpty.withConsumed(consumed)
          case head :: rest if isStartOf(head, command.name) && rest.forall(_.isEmpty) =>
            Res.Found(List(Completion(command.name, nonEmpty(command.header))), 1).commitNonEmptyIf(head.nonEmpty)
          case _ => Res.NoMatch
        }

      case Opts.App(f, a) =>
        val ff = completeOpts(args)(f)
        val aa = completeOpts(args)(a)
        ff ++ aa
      case Opts.OrElse(a, b) =>
        val aa = completeOpts(args)(a)
        val bb = completeOpts(args)(b)
        aa ++ bb
      case Opts.Single(opt)        => completeOpt(args)(opt)
      case Opts.Repeated(opt)      => completeOpt(args)(opt)
      case Opts.Validate(value, _) => completeOpts(args)(value)
      case Opts.Pure(_)            => Res.NoMatch
      case Opts.Missing            => Res.NoMatch
      case Opts.HelpFlag(_)        => Res.NoMatch
      case Opts.Env(_, _, _)       => Res.NoMatch
    }

  def completeOpt[A](args: List[String])(x: Opt[A]): Res = {
    def pickName(names: List[Opts.Name]): String =
      names.collectFirst { case x: Opts.LongName => x }.getOrElse(names.head).toString

    x match {
      case Opt.Regular(names, metavar, help, _) =>
        args match {
          case Nil                                              => Res.Found(List(Completion(pickName(names), nonEmpty(help))), 0)
          case head :: Nil                                      => completeName(names.map(_.toString), head, nonEmpty(help)).commitNonEmptyIf(head.nonEmpty)
          case head :: tail if names.exists(_.toString == head) => completeMetaVar(tail, metavar).commitNonEmpty.increaseConsumedBy(1)
          case _                                                => Res.NoMatch
        }

      case Opt.Flag(names, help, _) =>
        args match {
          case Nil         => Res.Found(List(Completion(pickName(names), Some(help).filterNot(_.isEmpty))), 0)
          case head :: Nil => completeName(names.map(_.toString), head, nonEmpty(help)).commitNonEmptyIf(head.nonEmpty)
          case _           => Res.NoMatch
        }

      case Opt.Argument(metavar) =>
        completeMetaVar(args, metavar)

      case Opt.OptionalOptArg(_, _, _, _) =>
        Res.NoMatch // todo
    }
  }

  private def nonEmpty(help: String): Option[String] =
    Some(help).filterNot(_.isEmpty)

  def completeName(nameStrings: List[String], arg: String, description: Option[String]): Res =
    nameStrings.collect { case name if isStartOf(arg, name) => Completion(name, description) } match {
      case Nil      => Res.NoMatch
      case nonEmpty => Res.Found(nonEmpty, 1)
    }

  def isStartOf(arg: String, name: String): Boolean =
    name.startsWith(arg) && arg != name

  def completeMetaVar(args: List[String], metavar: String): Res = {
    val all = possibleCompletionsForMetavar(metavar)

    if (args.forall(_.trim.isEmpty)) Res.Found(all.map(one => Completion(one, nonEmpty(metavar))), 1)
    else if (args.sizeIs == 1)
      Res.Found(all.collect { case name if name.startsWith(args.head) && args.head != name => Completion(name, nonEmpty(metavar)) }, 1)
    else Res.Found(Nil, consumed = 1)
  }
}

object Completer {
  case class Completion(value: String, description: Option[String])

  def bashToArgs(compLine: String, compCword: Int, compPoint: Int): List[String] = {
    val words: List[String] =
      compLine.take(compPoint).split("\\s+").toList.slice(1, compCword + 1)

    if (compPoint > compLine.trim.length) words :+ "" else words
  }

  sealed trait Res {
    def value: List[Completion]

    def withConsumed(n: Int): Res =
      this match {
        case Res.Commit(value, _) => Res.Commit(value, n)
        case Res.Found(value, _)  => Res.Found(value, n)
        case Res.NoMatch          => Res.NoMatch
      }

    def increaseConsumedBy(n: Int): Res =
      this match {
        case Res.NoMatch          => Res.NoMatch
        case matched: Res.Matched => withConsumed(n + matched.consumed)
      }

    def commitNonEmpty: Res =
      this match {
        case Res.Found(value, remainingArgs) if value.nonEmpty => Res.Commit(value, remainingArgs)
        case res                                               => res
      }

    def commitNonEmptyIf(pred: Boolean): Res =
      if (pred) commitNonEmpty else this

    def ++(other: Res): Res =
      (this, other) match {
        case (Res.NoMatch, maybeMatch)        => maybeMatch
        case (maybeMatch, Res.NoMatch)        => maybeMatch
        case (c1: Res.Commit, c2: Res.Commit) => Res.Commit(c1.value ++ c2.value, math.max(c1.consumed, c2.consumed))
        case (Res.Found(_, _), c: Res.Commit) => c
        case (c: Res.Commit, Res.Found(_, _)) => c
        case (r1: Res.Found, r2: Res.Found)   => Res.Found(r1.value ++ r2.value, math.max(r1.consumed, r2.consumed))
      }
  }

  object Res {
    case object NoMatch extends Res {
      override def value: List[Completion] = Nil
    }

    sealed trait Matched extends Res {
      def consumed: Int
    }

    case class Commit(value: List[Completion], consumed: Int) extends Matched
    case class Found(value: List[Completion], consumed: Int) extends Matched
  }
}

object Zsh {
  def hash(content: Iterator[String]): String = {
    val hash = MurmurHash3.arrayHash(content.toArray)
    if (hash < 0) (hash * -1).toString
    else hash.toString
  }

  def escape(input: String): String =
    input
      .replace("'", "\\'")
      .replace("`", "\\`")
      .replace("|", "\\|")
      .linesIterator
      .take(1)
      .toList
      .headOption
      .getOrElse("")

  def defs(item: Completion): Seq[String] = {
    val (options, arguments) = List(item.value).partition(_.startsWith("-"))
    val optionsOutput =
      if (options.isEmpty) Nil
      else {
        val desc = item.description.map(desc => ":" + escape(desc)).getOrElse("")
        options.map(opt => "\"" + opt + desc + "\"")
      }
    val argumentsOutput =
      if (arguments.isEmpty) Nil
      else {
        val desc = item.description.map(desc => ":" + escape(desc)).getOrElse("")
        arguments.map("'" + _.replace(":", "\\:") + desc + "'")
      }
    optionsOutput ++ argumentsOutput
  }

  def render(commands: Seq[String]): String =
    if (commands.isEmpty) "_files" + System.lineSeparator()
    else {
      val id = hash(commands.iterator)
      s"""local -a args$id
         |args$id=(
         |${commands.mkString(System.lineSeparator())}
         |)
         |_describe command args$id
         |""".stripMargin
    }

  def print(items: Seq[Completion]): String =
    render(items.flatMap(defs(_)))
}
