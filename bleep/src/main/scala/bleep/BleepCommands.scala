package bleep

import bleep.bsp.BspImpl
import bleep.internal.{Argv0, MyBloopRifleLogger, Os, ShortenJson}
import bleep.model.{ProjectName, ScriptName}
import cats.data.{NonEmptyList, ValidatedNel}
import cats.effect.{ExitCode, IO}
import cats.implicits.{catsSyntaxValidatedId, toTraverseOps}
import ch.epfl.scala.bsp4j
import com.monovore.decline.Argument
import io.circe.Encoder
import io.circe.syntax._

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, StandardOpenOption}
import java.util
import scala.build.bloop.{BloopServer, BloopThreads}
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.jdk.CollectionConverters._

sealed trait BleepCommands {
  def run(): IO[ExitCode]
}

object BleepCommands {
  def runWithEnv(runIo: Started => IO[ExitCode]): IO[ExitCode] =
    bootstrap.fromCwd match {
      case Left(th) => IO.raiseError(th)
      case Right(started) =>
        started.activeProject.foreach(p => println(s"active project: ${p.value}"))
        runIo(started)
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
      runWithEnv { started: Started =>
        val bloopRifleConfig = new BloopSetup(
          "/usr/bin/java",
          started.directories,
          started.build,
          started.resolver,
          bloopBspProtocol = None,
          bloopBspSocket = None
        ).bloopRifleConfig

        IO {
          BloopServer.withBuildServer(
            bloopRifleConfig,
            "bleep",
            Defaults.version,
            started.buildPaths.dotBleepDir,
            started.buildPaths.dotBleepDir / "classes",
            bspClient,
            BloopThreads.create(),
            new MyBloopRifleLogger(started.logger, true, true)
          ) { server =>
            def targetId(name: ProjectName): bsp4j.BuildTargetIdentifier =
              new bsp4j.BuildTargetIdentifier(started.buildPaths.buildDir.toFile.toURI.toASCIIString.stripSuffix("/") + "/?id=" + name.value)

            val targets = started.activeProject match {
              case Some(value) => List(targetId(value)).asJava
              case None        => started.build.projects.keys.map(targetId).toList.asJava
            }

            println(server.server.workspaceBuildTargets().get().getTargets)
            println(server.server.buildTargetCompile(new bsp4j.CompileParams(targets)).get())
          }
        }.as(ExitCode.Success)
      }
  }

  case object Import extends BleepCommands {
    override def run(): IO[ExitCode] = IO {
      val buildPaths = BuildPaths(Os.cwd / Defaults.BuildFileName)

      val build = deduplicateBuild(importBloopFilesFromSbt(buildPaths))
      Files.writeString(
        buildPaths.bleepJsonFile,
        build.asJson.foldWith(ShortenJson).spaces2,
        UTF_8
      )
      ExitCode.Success
    }
  }

  case object Test extends BleepCommands {
    override def run(): IO[ExitCode] =
      runWithEnv { started =>
        val projects = started.build.projects.keys.map(_.value).mkString(" ")
        IO(ExitCode(cli(s"bloop test $projects")(started.buildPaths.buildDir)))
      }
  }

  case object SetupIde extends BleepCommands {
    implicit def encodesUtilList[T: Encoder]: Encoder[util.List[T]] = Encoder[List[T]].contramap(_.asScala.toList)
    implicit val encoder: Encoder[bsp4j.BspConnectionDetails] =
      Encoder.forProduct5[bsp4j.BspConnectionDetails, String, util.List[String], String, String, util.List[String]](
        "name",
        "argv",
        "version",
        "bspVersion",
        "languages"
      )(x => (x.getName, x.getArgv, x.getVersion, x.getBspVersion, x.getLanguages))

    override def run(): IO[ExitCode] =
      runWithEnv { started =>
        IO {
          val progName = (new Argv0).get("bleep")
          val details = new bsp4j.BspConnectionDetails(
            "bleep",
            util.List.of(progName, "bsp"),
            Defaults.version,
            scala.build.blooprifle.internal.Constants.bspVersion,
            List("scala", "java").asJava
          )
          Files.createDirectories(started.buildPaths.bspBleepJsonFile.getParent)
          Files.writeString(
            started.buildPaths.bspBleepJsonFile,
            details.asJson.spaces2,
            StandardOpenOption.WRITE,
            StandardOpenOption.TRUNCATE_EXISTING,
            StandardOpenOption.CREATE
          )
          ExitCode.Success
        }
      }
  }

  case object Bsp extends BleepCommands {
    override def run(): IO[ExitCode] =
      runWithEnv { started =>
        IO {
          val bloopRifleConfig = new BloopSetup(
            "/usr/bin/java",
            started.directories,
            started.build,
            started.resolver,
            bloopBspProtocol = None,
            bloopBspSocket = None
          ).bloopRifleConfig

          bsp.BspThreads.withThreads { threads =>
            val bsp = new BspImpl(
              started.logger,
              bloopRifleConfig,
              buildPath = started.buildPaths.dotBleepDir,
              threads,
              System.in,
              System.out,
              () => bootstrap.fromCwd
            )

            try {
              val doneFuture = bsp.run()
              Await.result(doneFuture, Duration.Inf)
            } finally bsp.shutdown()
          }
          ExitCode.Success
        }
      }
  }

  case class Script(name: String, args: List[String]) extends BleepCommands {
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
}
