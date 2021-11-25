package com.monovore.decline

import cats.data.{NonEmptyList, Validated}
import cats.syntax.apply._
import cats.syntax.foldable._
import com.monovore.decline.Completer.Completion
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.funsuite.AnyFunSuite

class CompleterTest extends AnyFunSuite with TypeCheckedTripleEquals {
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

  private val compileHelp = "compile projects"
  private val testHelp = "test projects"
  private val foobarHelp = "the a"
  private val barHelp = "the bar"

  val subCommands: List[Opts[Unit]] = List(
    Opts.subcommand("compile", compileHelp)(
      (Opts.flag("foobar", foobarHelp, "f"), Opts.option[Number]("bar", barHelp, "b"), Project.many).mapN { case (_, _, _) => () }
    ),
    Opts.subcommand("compose", "")(Opts(())),
    Opts.subcommand("test", testHelp)(Project.many.map(_ => ()))
  )

  test("works") {
    val completer = new Completer({
      case Project.metavar => List("common", "core", "test")
      case Number.metavar  => Number.valids
      case metavar         => sys.error(s"specify how to complete metavar $metavar")
    })

    val compile = Completion("compile", Some(compileHelp))
    val compose = Completion("compose", None)
    val testCmd = Completion("test", Some(testHelp))
    val testProj = Completion("test", Some(Project.metavar))
    val common = Completion("common", Some(Project.metavar))
    val core = Completion("core", Some(Project.metavar))
    val foobar = Completion("--foobar", Some(foobarHelp))
    val bar = Completion("--bar", Some(barHelp))
    val one = Completion("one", Some(Number.metavar))
    val two = Completion("two", Some(Number.metavar))
    val three = Completion("three", Some(Number.metavar))

    val opts = subCommands.foldK

    assert(completer.completeOpts(Nil)(opts).value === List(compile, compose, testCmd))
    assert(completer.completeOpts(List("co"))(opts).value === List(compile, compose))
    assert(completer.completeOpts(List("te"))(opts).value === List(testCmd))
    assert(completer.completeOpts(List("compile", "co"))(opts).value === List(common, core))
    assert(completer.completeOpts(List("compile", "core", "te"))(opts).value === List(testProj))
    assert(completer.completeOpts(List("compile"))(opts).value === List(foobar, bar, common, core, testProj))
    assert(completer.completeOpts(List("compile", "--bar", ""))(opts).value === List(one, two, three))
    assert(completer.completeOpts(List("compile", "--bar", "t"))(opts).value === List(two, three))
  }

  test("bashToArgs") {
    assert(Completer.bashToArgs("bleep/target/bleep compile ble", 2, 30) === List("compile", "ble"))
    assert(Completer.bashToArgs("bleep/target/bleep compile ", 2, 26) === List("compile"))
    assert(Completer.bashToArgs("bleep/target/bleep compile ", 2, 27) === List("compile", ""))
  }
}
