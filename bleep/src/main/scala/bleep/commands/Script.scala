package bleep.commands

import bleep.{cli, fixedClasspath, BleepCommand}
import bleep.model.ScriptName
import cats.data.{NonEmptyList, ValidatedNel}
import cats.effect.{ExitCode, IO}
import cats.syntax.traverse._
import cats.syntax.validated._
import com.monovore.decline.Argument

case class Script(name: String, args: List[String]) extends BleepCommand {
  override def run(): IO[ExitCode] =
    runWithEnv(started =>
      IO {
        val scriptDefs = started.build.scripts.get(ScriptName(name))
        val projects = scriptDefs.values.map(_.project.value).distinct.mkString(" ")
        cli(s"bloop compile $projects")(started.buildPaths.buildDir)
      }.flatMap { code =>
        val scriptDefs = started.build.scripts.get(ScriptName(name))
        scriptDefs.values
          .traverse { scriptDef =>
            IO {
              val bloopFile = started.bloopFiles(scriptDef.project).forceGet(scriptDef.project.value)
              val fullClassPath = fixedClasspath(bloopFile.project)
              cli(s"java -cp ${fullClassPath.mkString(":")} ${scriptDef.main} ${args.mkString(" ")}")(started.buildPaths.buildDir)
            }
          }
          .map(NonEmptyList(code, _))
      }.map(codes => ExitCode(codes.toList.max))
    )
}

object Script {
  implicit val argument: Argument[Script] = new Argument[Script] {

    def read(input: String): ValidatedNel[String, Script] =
      input.split(" ").toList match {
        case head :: tail => Script(head, tail).validNel
        case _            => s"Invalid script input '$input'".invalidNel
      }

    override def defaultMetavar: String = "script"
  }
}
