package com.monovore.decline

import com.monovore.decline.Completer._

final class Completer(possibleCompletionsForMetavar: String => List[String]) extends App {
  def bash[A](compLine: String, compCword: Int, compPoint: Int)(x: Opts[A]): List[Completion] = {
    val args = bashToArgs(compLine, compCword, compPoint)
    completeOpts(args)(x).value
  }

  def completeOpts[A](args: List[String])(x: Opts[A]): Res =
    x match {
      case Opts.Subcommand(command) =>
        val found = Res.Found(List(Completion(command.name, nonEmpty(command.header))))
        if (args.isEmpty) found
        else if (args.head == command.name) completeOpts(args.tail)(command.options).commitNonEmpty
        else if (isStartOf(args.head, command.name)) found.commitNonEmptyIf(args.head.nonEmpty)
        else Res.Empty

      case Opts.App(f, a) =>
        val ff = completeOpts(args)(f)
        val aa = completeOpts(args)(a)
        ff ++ aa
      case Opts.OrElse(a, b) =>
        val aa = completeOpts(args)(a)
        val bb = completeOpts(args)(b)
        aa ++ bb
      case Opts.Single(opt)        => completeOpt(args)(opt)
      case Opts.Repeated(opt)      => completeOpt(args.takeRight(1))(opt)
      case Opts.Validate(value, _) => completeOpts(args)(value)
      case Opts.Pure(_)            => Res.Empty
      case Opts.Missing            => Res.Empty
      case Opts.HelpFlag(_)        => Res.Empty
      case Opts.Env(_, _, _)       => Res.Empty
    }

  def completeOpt[A](args: List[String])(x: Opt[A]): Res = {
    def pickName(names: List[Opts.Name]): String =
      names.collectFirst { case x: Opts.LongName => x }.getOrElse(names.head).toString

    x match {
      case Opt.Regular(names, metavar, help, _) =>
        if (args.isEmpty) Res.Found(List(Completion(pickName(names), nonEmpty(help))))
        else if (args.sizeIs == 1) completeName(names.map(_.toString), args.head, nonEmpty(help)).commitNonEmptyIf(args.head.nonEmpty)
        else if (names.exists(_.toString == args.head)) completeMetaVar(args.tail, metavar).commitNonEmpty
        else Res.Empty

      case Opt.Flag(names, help, _) =>
        if (args.isEmpty) Res.Found(List(Completion(pickName(names), Some(help).filterNot(_.isEmpty))))
        else if (args.sizeIs == 1) completeName(names.map(_.toString), args.head, nonEmpty(help)).commitNonEmptyIf(args.head.nonEmpty)
        else Res.Empty

      case Opt.Argument(metavar) =>
        completeMetaVar(args, metavar)
    }
  }

  private def nonEmpty(help: String): Option[String] =
    Some(help).filterNot(_.isEmpty)

  def completeName(nameStrings: List[String], arg: String, description: Option[String]): Res =
    Res.Found(nameStrings.collect { case name if isStartOf(arg, name) => Completion(name, description) })

  def isStartOf(arg: String, name: String): Boolean =
    name.startsWith(arg) && arg != name

  def completeMetaVar(args: List[String], metavar: String): Res = {
    val all = possibleCompletionsForMetavar(metavar)

    if (!args.exists(_.trim.nonEmpty)) Res.Found(all.map(one => Completion(one, nonEmpty(metavar))))
    else if (args.sizeIs == 1) Res.Found(all.collect { case name if name.startsWith(args.head) && args.head != name => Completion(name, nonEmpty(metavar)) })
    else Res.Empty
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
    def commitNonEmpty: Res = this match {
      case Res.Found(value) if value.nonEmpty => Res.Commit(value)
      case res                                => res
    }

    def commitNonEmptyIf(pred: Boolean): Res =
      if (pred) commitNonEmpty else this

    def ++(other: Res): Res =
      (this, other) match {
        case (c1: Res.Commit, c2: Res.Commit) => Res.Commit(c1.value ++ c2.value)
        case (Res.Found(_), c: Res.Commit)    => c
        case (c: Res.Commit, Res.Found(_))    => c
        case (Res.Found(v1), Res.Found(v2))   => Res.Found(v1 ++ v2)
      }
  }

  object Res {
    case class Commit(value: List[Completion]) extends Res
    case class Found(value: List[Completion]) extends Res
    val Empty: Res = Found(Nil)
  }
}
