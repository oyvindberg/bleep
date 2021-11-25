package com.monovore.decline

import cats.data.{NonEmptyList, Validated}
import cats.syntax.apply._
import cats.syntax.foldable._
import org.scalatest.funsuite.AnyFunSuite

class CompleterTest extends AnyFunSuite {
  case class Number(str: String)
  object Number {
    val metavar: String = "number"
    val valids = List("one", "two", "three")

    implicit val argument: Argument[Number] =
      Argument.fromMap(metavar, valids.map(x => (x, Number(x))).toMap)

    val many: Opts[Option[NonEmptyList[Number]]] =
      Opts.arguments[Number]().orNone
  }

  case class Project(str: String)
  object Project {
    val metavar: String =
      "project"

    implicit val argument: Argument[Project] =
      Argument.from(metavar)(str => Validated.Valid(Project(str)))

    val many: Opts[Option[NonEmptyList[Project]]] =
      Opts.arguments[Project]().orNone
  }

  val subCommands: List[Opts[Unit]] = List(
    Opts.subcommand("compile", "")(
      (
        Opts.flag("foobar", "the a", "f"),
        Opts.option[Number]("bar", "the bar", "b"),
        Project.many
      ).mapN { case (_, _, _) => () }
    ),
    Opts.subcommand("compose", "")(
      Opts(())
    ),
    Opts.subcommand("test", "test projects")(
      Project.many.map(_ => ())
    )
  )

  test("works") {
    val completer = new Completer({
      case Project.metavar => List("common", "core", "test")
      case Number.metavar  => Number.valids
      case metavar         => sys.error(s"specify how to complete metavar $metavar")
    })

    val opts = subCommands.foldK
    assert(completer.completeOpts(Nil)(opts).value === List("compile", "compose", "test"))
    assert(completer.completeOpts(List("co"))(opts).value === List("compile", "compose"))
    assert(completer.completeOpts(List("te"))(opts).value === List("test"))
    assert(completer.completeOpts(List("compile", "co"))(opts).value === List("common", "core"))
    assert(completer.completeOpts(List("compile", "core", "te"))(opts).value === List("test"))
    assert(completer.completeOpts(List("compile"))(opts).value === List("--foobar", "--bar", "common", "core", "test"))
    assert(completer.completeOpts(List("compile", "--bar", ""))(opts).value === List("one", "two", "three"))
    assert(completer.completeOpts(List("compile", "--bar", "t"))(opts).value === List("two", "three"))
  }

  test("bashToArgs") {
    assert(Completer.bashToArgs("bleep/target/bleep compile ble", 2, 30) === List("compile", "ble"))
    assert(Completer.bashToArgs("bleep/target/bleep compile ", 2, 26) === List("compile"))
    assert(Completer.bashToArgs("bleep/target/bleep compile ", 2, 27) === List("compile", ""))
  }
}
