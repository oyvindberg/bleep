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
    val valids = Array("one", "two", "three")

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
      case Project.metavar => Array("common", "core", "test")
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
    val f = Completion("-f", Some(foobarHelp))
    val bar = Completion("--bar", Some(barHelp))
    val b = Completion("-b", Some(barHelp))
    val one = Completion("one", Some(Number.metavar))
    val two = Completion("two", Some(Number.metavar))
    val three = Completion("three", Some(Number.metavar))

    val opts = subCommands.foldK

    assert(completer.completeOpts(Nil)(opts).value === Array(compile, compose, testCmd))
    assert(completer.completeOpts(List("co"))(opts).value === Array(compile, compose))
    assert(completer.completeOpts(List("te"))(opts).value === Array(testCmd))
    assert(completer.completeOpts(List("compile", "co"))(opts).value === Array(common, core))
    assert(completer.completeOpts(List("compile", "core", "te"))(opts).value === Array(testProj))
    assert(completer.completeOpts(List("compile", ""))(opts).value === Array(foobar, f, bar, b, common, core, testProj))
    assert(completer.completeOpts(List("compile", "--bar", ""))(opts).value === Array(one, two, three))
    assert(completer.completeOpts(List("compile", "--bar", "t"))(opts).value === Array(two, three))
  }

  test("repeated options") {
    val platformMetavar = "platform name"
    val platformHelp = "specify wanted platform(s)"
    val platforms = List("jvm", "js", "native").map(x => (x, x)).toMap
    val fooMetavar = "foo name"
    val fooHelp = "specify wanted foo(s)"
    val foos = Map("one" -> "A", "two" -> "B")

    val opts =
      Opts.subcommand("cmd1", "")(
        Opts.subcommand("cmd2", "")(
          (
            Opts
              .options("platform", platformHelp, metavar = platformMetavar, short = "p")(Argument.fromMap(platformMetavar, platforms))
              .withDefault(NonEmptyList.of("jvm")),
            Opts
              .options("foo", fooHelp, metavar = fooMetavar, short = "f")(Argument.fromMap(fooMetavar, foos))
              .withDefault(NonEmptyList.of("one")),
            Opts.argument[String]("bar")
          ).mapN((_, _, _) => ())
        )
      )

    val completer = new Completer({
      case `platformMetavar` => platforms.keys.toArray
      case `fooMetavar`      => foos.keys.toArray
      case _                 => Array.empty
    })

    assert(completer.completeOpts(List("cmd1", "cmd2", "--p"))(opts).value === Array(Completion("--platform", Some(platformHelp))))
    assert(completer.completeOpts(List("cmd1", "cmd2", "-p"))(opts).value === Array.empty[Completion])
    assert(
      completer.completeOpts(List("cmd1", "cmd2", "-p", ""))(opts).value === Array(
        Completion("jvm", Some(platformMetavar)),
        Completion("js", Some(platformMetavar)),
        Completion("native", Some(platformMetavar))
      )
    )
    assert(
      completer.completeOpts(List("cmd1", "cmd2", "-p", "j"))(opts).value === Array(
        Completion("jvm", Some(platformMetavar)),
        Completion("js", Some(platformMetavar))
      )
    )
    assert(
      completer.completeOpts(List("cmd1", "cmd2", "-p", "jvm", "-p", "n"))(opts).value === Array(
        Completion("native", Some(platformMetavar))
      )
    )
    assert(
      completer.completeOpts(List("cmd1", "cmd2", "-p", "jvm", "-f", "o"))(opts).value === Array(
        Completion("one", Some(fooMetavar))
      )
    )
  }

  test("bashToArgs") {
    assert(Completer.bashToArgs("bleep/target/bleep compile ble", 2, 30) === List("compile", "ble"))
    assert(Completer.bashToArgs("bleep/target/bleep compile ", 2, 26) === List("compile"))
    assert(Completer.bashToArgs("bleep/target/bleep compile ", 2, 27) === List("compile", ""))
  }
}
