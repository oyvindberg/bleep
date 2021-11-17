package bleep

import bleep.bootstrap.Bootstrapped
import bleep.internal.{Directories, MyBloopRifleLogger, Os, ShortenJson}
import bleep.model.{ProjectName, ScriptName}
import cats.data.{NonEmptyList, ValidatedNel}
import cats.effect.{ExitCode, IO}
import cats.implicits.{catsSyntaxValidatedId, toTraverseOps}
import ch.epfl.scala.bsp4j
import com.monovore.decline.Argument
import io.circe.syntax._

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files
import scala.build.bloop.{BloopServer, BloopThreads}
import scala.jdk.CollectionConverters._

sealed trait BleepCommands {
  def run(): IO[ExitCode]
}

object BleepCommands {
  def runWithEnv(runIo: Bootstrapped.Ok => IO[ExitCode]): IO[ExitCode] =
    bootstrap.from(Os.cwd) match {
      case Bootstrapped.InvalidJson(e) =>
        IO(e.printStackTrace()).as(ExitCode.Error)
      case Bootstrapped.BuildNotFound =>
        IO(System.err.println(s"Couldn't find a bleep build in (parent directories of) ${Os.cwd}")).as(ExitCode.Error)

      case bootstrap: Bootstrapped.Ok =>
        bootstrap.activeProject.foreach(p => println(s"active project: ${p.value}"))
        runIo(bootstrap)
    }

  object bspClient extends bsp4j.BuildClient {
    override def onBuildShowMessage(params: bsp4j.ShowMessageParams): Unit = println(params)
    override def onBuildLogMessage(params: bsp4j.LogMessageParams): Unit = println(params)
    override def onBuildTaskStart(params: bsp4j.TaskStartParams): Unit = println(params)
    override def onBuildTaskProgress(params: bsp4j.TaskProgressParams): Unit = println(params)
    override def onBuildTaskFinish(params: bsp4j.TaskFinishParams): Unit = println(params)
    override def onBuildPublishDiagnostics(params: bsp4j.PublishDiagnosticsParams): Unit = println(params)
    override def onBuildTargetDidChange(params: bsp4j.DidChangeBuildTarget): Unit = println(params)
  }

  case object Compile extends BleepCommands {
    override def run(): IO[ExitCode] =
      runWithEnv { bootstrap =>
        val bloopSetup = new BloopSetup(
          "/usr/bin/java",
          Directories.default,
          bootstrap.build,
          bootstrap.resolver,
          bloopBspProtocol = None,
          bloopBspSocket = None,
        )

        IO {
          BloopServer.withBuildServer(
            bloopSetup.bloopRifleConfig,
            "bleep",
            Defaults.version,
            bootstrap.buildDirPath / ".bleep",
            bootstrap.buildDirPath / ".bleep" / "classes",
            bspClient,
            BloopThreads.create(),
            new MyBloopRifleLogger(bootstrap.logger, true, true)
          ) { server =>
            def targetId(name: ProjectName): bsp4j.BuildTargetIdentifier = {
              new bsp4j.BuildTargetIdentifier(bootstrap.buildDirPath.toFile.toURI.toASCIIString.stripSuffix("/") + "/?id=" + name.value)
            }
            val targets = bootstrap.activeProject match {
              case Some(value) => List(targetId(value)).asJava
              case None        => bootstrap.build.projects.keys.map(targetId).toList.asJava
            }
            println(server.server.workspaceBuildTargets().get().getTargets)
            println(server.server.buildTargetCompile(new bsp4j.CompileParams(targets)).get())
          }
        }.as(ExitCode.Success)
      }
  }

  case object Import extends BleepCommands {
    override def run(): IO[ExitCode] = IO{
      val build = deduplicateBuild(importBloopFilesFromSbt(Os.cwd))
      Files.writeString(
        Os.cwd / Defaults.BuildFileName,
        build.asJson.foldWith(ShortenJson).spaces2,
        UTF_8
      )
      ExitCode.Success
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
