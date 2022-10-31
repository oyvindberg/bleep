package bleep
package scripts

import bleep.internal.{jvmOrSystem, FileUtils}
import bleep.logging.{jsonEvents, Logger}
import bleep.tasks._

import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

object GenVideos extends BleepScript("GenVideos") {
  val RecCmd = Path.of(getClass.getResource("/asciinema-rec_script").toURI)
  val Exec = PosixFilePermissions.fromString("rwxrwxr-x")

  override def run(started: Started, commands: Commands, args: List[String]): Unit = {
    val project = started.bloopProjects(model.CrossProjectName(model.ProjectName("bleep-cli"), crossId = None))
    val ni = new NativeImagePlugin(project, started.logger, nativeImageJvm = jvmOrSystem(started))

    val nativeImage =
      List(args.headOption.map(Path.of(_)), Some(ni.nativeImageOutput)).flatten.find(FileUtils.exists) match {
        case Some(found) =>
          started.logger.info(s"Using native-image at $found")
          found
        case None =>
          sys.error(s"Expected a native-image built at ${ni.nativeImageOutput} or provided as parameter")
      }

    val env = sys.env
      .updated("BAT_PAGER", "")
      .removed(jsonEvents.CallerProcessAcceptsJsonEvents)
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

    val generating: List[Future[(RelPath, String)]] =
      Demo.all.map { demo =>
        Future {
          val relPath = RelPath.force(s"${demo.name}.cast")
          val content = genVideo(demo, env.toList, started.logger.withPath(demo.name))
          (relPath, content)
        }
      }

    val generated: Map[RelPath, String] =
      Await.result(Future.sequence(generating), Duration.Inf).toMap

    FileSync
      .syncStrings(
        started.buildPaths.buildDir / "docs" / "videos",
        generated,
        deleteUnknowns = FileSync.DeleteUnknowns.Yes(maxDepth = None),
        soft = true
      )
      .log(started.logger, "wrote videos")
  }

  def genVideo(demo: Demo, env: List[(String, String)], logger: Logger): String = {
    val tempDir = Files.createTempDirectory(s"bleep-videos-${demo.name}")
    // nest one level to not include script and output file in file listings
    val workDir = tempDir / "work"
    Files.createDirectories(workDir)
    logger.withContext(tempDir).debug("using temporary directory")

    val scriptFile = tempDir / "script"
    FileUtils.writeString(scriptFile, demo.script(Path.of("bleep")))
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

    cli("asciinema-rec_script", workDir, cmd, cliLogger = cli.CliLogger(logger), env = env)

    val content = Files.readString(outputFile)
    FileUtils.deleteDirectory(tempDir)

    content.replace(tempDir.toString, "/folder")
  }
}
