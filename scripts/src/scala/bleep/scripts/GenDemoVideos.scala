package bleep
package scripts

import bleep.internal.{bleepLoggers, FileUtils}
import bleep.plugin.nativeimage.NativeImagePlugin
import ryddig.Logger

import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

object GenDemoVideos extends BleepScript("GenVideos") {
  val RecCmd = Path.of(getClass.getResource("/asciinema-rec_script").toURI)
  val Exec = PosixFilePermissions.fromString("rwxrwxr-x")

  override def run(started: Started, commands: Commands, args: List[String]): Unit = {
    val project = started.bloopProject(model.CrossProjectName(model.ProjectName("bleep-cli"), crossId = Some(model.CrossId("jvm213"))))
    val logger = started.logger.syncAccess(GenDemoVideos)
    val ni = new NativeImagePlugin(project, logger, started.jvmCommand)

    val nativeImage =
      List(args.headOption.map(Path.of(_)), Some(ni.nativeImageOutput)).flatten.find(FileUtils.exists) match {
        case Some(found) =>
          logger.info(s"Using native-image at $found")
          found
        case None =>
          sys.error(s"Expected a native-image built at ${ni.nativeImageOutput} or provided as parameter")
      }

    val env = sys.env
      .updated("BAT_PAGER", "")
      .removed(bleepLoggers.CallerProcessAcceptsJsonEvents)
      .updated(
        "PATH", {
          // this whole exercise is really to make "bleep-cli" look like "bleep" in the videos
          val tempDir = Files.createTempDirectory("bleep-videos")
          Files.createSymbolicLink(tempDir / "bleep", nativeImage)
          sys.env.get("PATH") match {
            case Some(existingPath) => s"$tempDir:$existingPath"
            case None               => tempDir.toString
          }
        }
      )

    implicit val ec: ExecutionContext = started.executionContext

    val generating: List[Future[Map[RelPath, String]]] =
      Demo.all.map { demo =>
        Future {
          val generated = genVideo(demo, env.toList, logger.withPath(demo.name))
          Map(
            RelPath.force(s"${demo.name}.cast") -> Some(generated.video),
            RelPath.force(s"${demo.name}.yaml") -> generated.yaml
          ).collect { case (k, Some(v)) => (k, v) }
        }
      }

    val generated: Map[RelPath, String] =
      Await.result(Future.sequence(generating), Duration.Inf).reduce(_ ++ _)

    FileSync
      .syncStrings(
        started.buildPaths.buildDir / "bleep-site" / "static" / "demos",
        generated,
        deleteUnknowns = FileSync.DeleteUnknowns.Yes(maxDepth = None),
        soft = true
      )
      .log(started.logger, "wrote videos")
  }

  case class Generated(video: String, yaml: Option[String])

  def genVideo(demo: Demo, env: List[(String, String)], logger: Logger): Generated = {
    val tempDir = Files.createTempDirectory(s"bleep-videos-${demo.name}")
    // nest one level to not include script and output file in file listings
    val workDir = tempDir / "work"
    Files.createDirectories(workDir)
    logger.withContext("tempDir", tempDir).debug("using temporary directory")

    val scriptFile = tempDir / "script"
    FileUtils.writeString(logger, None, scriptFile, demo.script(Path.of("bleep")))
    Files.setPosixFilePermissions(scriptFile, Exec)

    val outputFile = tempDir / "output"

    val cmd = List(
      RecCmd.toString,
      scriptFile.toString,
      "--title",
      demo.name,
      "--yes",
      "--rows",
      demo.rows.toString,
      "--col",
      demo.columns.toString,
      RelPath.relativeTo(workDir, outputFile).toString // this somehow needs to be relative
    )

    cli("asciinema-rec_script", workDir, cmd, logger = logger, out = cli.Out.ViaLogger(logger), env = env).discard()

    val video = Files.readString(outputFile).replace(tempDir.toString, "/folder")
    val maybeYaml = demo.expectedYaml.map(yamlRelPath => Files.readString(workDir / yamlRelPath))

    FileUtils.deleteDirectory(tempDir)

    Generated(video = video, yaml = maybeYaml)
  }
}
