package bleep
package scripts

import bleep.internal.FileUtils
import bleep.logging.Logger
import bleep.tasks._

import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

object GenVideos extends BleepScript("GenVideos") {
  val RecCmd = Path.of(getClass.getResource("/asciinema-rec_script").toURI)
  val Exec = PosixFilePermissions.fromString("rwxrwxr-x")

  case class Video(name: String, rows: Int = 40)(val script: String)

  val videos = List(
    Video("run-native") {
      """
        |# create build
        |bleep build new -p native mycli
        |
        |# show files
        |find . -not -path '*/.*'
        |
        |# list projects
        |bleep projects
        |
        |# show build file
        |bat --paging=never bleep.yaml
        |
        |# show main
        |bat --paging=never mycli/src/scala/com/foo/App.scala
        |
        |# run
        |bleep run mycli
        |""".stripMargin
    },
    Video("run-cross-native-jvm") {
      """
        |# create build
        |bleep build new --platform native --platform jvm --scala 2.13 --scala 3 mycli
        |
        |# show files
        |find . -not -path '*/.*'
        |
        |# list projects
        |bleep projects
        |
        |# show build file
        |bat --paging=never bleep.yaml |head -n 30
        |
        |# show main
        |bat --paging=never mycli/shared/src/scala/com/foo/App.scala
        |
        |# run
        |bleep run mycli@native213
        |bleep run mycli@native3
        |bleep run mycli@jvm213
        |bleep run mycli@jvm3
        |""".stripMargin
    }
  )

  override def run(started: Started, commands: Commands, args: List[String]): Unit = {
    val project = started.bloopProjects(model.CrossProjectName(model.ProjectName("bleep-cli"), crossId = None))
    val ni = new NativeImagePlugin(project, started.logger, nativeImageJvm = started.build.jvm.getOrElse(model.Jvm.graalvm))

    val nativeImageBleep = if (FileUtils.exists(ni.nativeImageOutput)) {
      started.logger.info(s"Using native-image at ${ni.nativeImageOutput}")
      Some(ni.nativeImageOutput)
    } else None

    // this whole exercise is really to make "bleep-cli" look like "bleep" in the videos
    val env = nativeImageBleep
      .foldLeft(sys.env.updated("VERSION", "?")) { case (env, nativeImage) =>
        val tempDir = Files.createTempDirectory("bleep-videos")
        Files.createSymbolicLink(tempDir / "bleep", nativeImage)
        val newPath = sys.env.get("PATH") match {
          case Some(existingPath) => s"$tempDir:$existingPath"
          case None               => tempDir.toString
        }
        env.updated("PATH", newPath)
      }
      .toList

    implicit val ec: ExecutionContext = started.executionContext

    val generating: List[Future[(RelPath, Array[Byte])]] =
      videos.map { video =>
        Future {
          val relPath = RelPath.force(s"${video.name}.cast")
          val bytes = gen(video, env, started.logger.withContext(video.name))
          (relPath, bytes)
        }
      }

    val generated: Map[RelPath, Array[Byte]] =
      Await.result(Future.sequence(generating), Duration.Inf).toMap

    FileSync.syncBytes(
      started.buildPaths.buildDir / "docs" / "videos",
      generated,
      deleteUnknowns = FileSync.DeleteUnknowns.Yes(maxDepth = None),
      soft = true
    )
  }

  def gen(video: Video, env: List[(String, String)], logger: Logger): Array[Byte] = {
    val tempDir = Files.createTempDirectory(s"bleep-videos-${video.name}")
    logger.withContext(tempDir).debug("using temporary directory")

    val scriptFile = tempDir / "script"
    FileUtils.writeString(scriptFile, video.script)
    Files.setPosixFilePermissions(scriptFile, Exec)

    val outputFile = tempDir / "output"

    val cmd = List(
      RecCmd.toString,
      scriptFile.toString,
      "--title",
      video.name,
      "--yes",
      "--overwrite",
      "--rows",
      video.rows.toString,
      outputFile.getFileName.toString
    )

    cli("gen", tempDir, cmd = cmd, logger, env = env)

    val bytes = Files.readAllBytes(outputFile)
    FileUtils.deleteDirectory(tempDir)
    bytes
  }

}
