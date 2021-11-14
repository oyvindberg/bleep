package bleep

import bleep.bootstrap.Bootstrapped
import bleep.model.ScriptName
import cats.data.{NonEmptyList, ValidatedNel}
import cats.effect.{ExitCode, IO}
import cats.implicits.{catsSyntaxValidatedId, toTraverseOps}
import com.monovore.decline.Argument

import java.nio.file.{Path, Paths}

sealed trait BleepCommands {
  def run(): IO[ExitCode]
}

object BleepCommands {
  val cwd: Path = Paths.get(System.getProperty("user.dir"))

  def runWithEnv(runIo: (Bootstrapped.Ok) => IO[ExitCode]): IO[ExitCode] = bootstrap.from(cwd) match {
    case Bootstrapped.InvalidJson(e) =>
      IO(ExitCode.Error)
    case Bootstrapped.BuildNotFound =>
      IO {
        sys.error(s"Couldn't find a bleep build in (parent directories of) $cwd")
        ExitCode.Error
      }

    case bootstrap: Bootstrapped.Ok =>
      bootstrap.activeProject.foreach(p => println(s"active project: ${p.value}"))
      runIo(bootstrap)
  }

  case object Compile extends BleepCommands {
    override def run(): IO[ExitCode] =
      runWithEnv { bootstrap =>
        val projects = bootstrap.build.projects.keys.map(_.value).mkString(" ")
        IO(ExitCode(cli(s"bloop compile $projects")(bootstrap.buildDirPath)))
      }
  }

  case object Test extends BleepCommands {
    override def run(): IO[ExitCode] =
      runWithEnv { bootstrap =>
        val projects = bootstrap.build.projects.keys.map(_.value).mkString(" ")
        IO(ExitCode(cli(s"bloop test $projects")(bootstrap.buildDirPath)))
      }
  }

  case class Script(name: String, args: List[String]) extends BleepCommands {
    override def run(): IO[ExitCode] =
      runWithEnv(bootstrap =>
        IO {
          val scriptDefs = bootstrap.build.scripts.get(ScriptName(name))
          val projects = scriptDefs.values.map(_.project.value).distinct.mkString(" ")
          cli(s"bloop compile $projects")(bootstrap.buildDirPath)
        }.flatMap { code =>
          val scriptDefs = bootstrap.build.scripts.get(ScriptName(name))
          scriptDefs.values
            .traverse { scriptDef =>
              IO {
                val bloopFile = bootstrap.bloopFiles(scriptDef.project).forceGet(scriptDef.project.value)
                val fullClassPath = fixedClasspath(bloopFile.project)
                cli(s"java -cp ${fullClassPath.mkString(":")} ${scriptDef.main} ${args.mkString(" ")}")(bootstrap.buildDirPath)
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

}
