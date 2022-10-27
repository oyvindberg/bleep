package bleep
package scripts

import bleep.internal.{jvmOrSystem, FileUtils}
import bleep.logging.{jsonEvents, Logger}
import bleep.tasks._

import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path}
import java.util
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

object GenVideos extends BleepScript("GenVideos") {
  val RecCmd = Path.of(getClass.getResource("/asciinema-rec_script").toURI)
  val Exec = PosixFilePermissions.fromString("rwxrwxr-x")

  abstract class Video(val name: String, val rows: Int = 40, val columns: Int = 100) {
    def script: String
  }

  val videos = List(
    new Video("run-native") {
      override def script: String =
        """
        |# create build
        |bleep build new -p native mycli
        |
        |# show files
        |find . -type f
        |
        |# list projects
        |bleep projects
        |
        |# show build file
        |bat bleep.yaml
        |
        |# show generated main file
        |bat mycli/src/scala/com/foo/App.scala
        |
        |# run
        |bleep run mycli
        |""".stripMargin
    },
    new Video("run-cross-native-jvm") {
      override def script: String =
        s"""
        |# create build
        |bleep build new --platform native --platform jvm --scala 2.13 --scala 3 mycli
        |
        |# show files
        |find . -type f
        |
        |# list projects
        |bleep projects
        |
        |# show build file (part one)
        |bat --line-range :$rows bleep.yaml
        |# show build file (part two)
        |bat --line-range $rows: bleep.yaml
        |
        |# show generated main file
        |bat mycli/shared/src/scala/com/foo/App.scala
        |
        |# run
        |bleep run mycli@native213
        |bleep run mycli@native3
        |bleep run mycli@jvm213
        |bleep run mycli@jvm3
        |""".stripMargin
    },
    new Video("import") {
      override def script: String =
        s"""
        |# git clone an existing build
        |git clone git@github.com:scalameta/munit.git
        |
        |cd munit
        |
        |# import into bleep. note that this is a one-time, slow step
        |bleep import
        |
        |# list projects
        |bleep projects
        |
        |# show build file
        |bat --line-range :$rows bleep.yaml
        |
        |# generate resources
        |bleep generate-resources
        |
        |# run scala 3 jvm tests for all modules
        |bleep test jvm3
        |""".stripMargin
    }
  )

  override def run(started: Started, commands: Commands, args: List[String]): Unit = {
    val project = started.bloopProjects(model.CrossProjectName(model.ProjectName("bleep-cli"), crossId = None))
    val ni = new NativeImagePlugin(project, started.logger, nativeImageJvm = jvmOrSystem(started))

    val nativeImageBleep = if (FileUtils.exists(ni.nativeImageOutput)) {
      started.logger.info(s"Using native-image at ${ni.nativeImageOutput}")
      Some(ni.nativeImageOutput)
    } else None

    val env = nativeImageBleep.foldLeft(sys.env.updated("BAT_PAGER", "")) { case (env, nativeImage) =>
      // this whole exercise is really to make "bleep-cli" look like "bleep" in the videos
      val tempDir = Files.createTempDirectory("bleep-videos")
      Files.createSymbolicLink(tempDir / "bleep", nativeImage)
      val newPath = sys.env.get("PATH") match {
        case Some(existingPath) => s"$tempDir:$existingPath"
        case None               => tempDir.toString
      }
      env.updated("PATH", newPath)
    }

    implicit val ec: ExecutionContext = started.executionContext

    val generating: List[Future[(RelPath, Array[Byte])]] =
      videos.map { video =>
        Future {
          val relPath = RelPath.force(s"${video.name}.cast")
          val bytes = gen(video, env.toList, started.logger.withContext(video.name))
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
    // nest one level to not include script and output file in file listings
    val workDir = tempDir / "work"
    Files.createDirectories(workDir)
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
      "--rows",
      video.rows.toString,
      "--col",
      video.columns.toString,
      RelPath.relativeTo(workDir, outputFile).toString // this somehow needs to be relative
    )

    cli("asciinema-rec_script", workDir, cmd, cliLogger = cli.CliLogger(logger), env = env)

    val bytes = Files.readAllBytes(outputFile)
    FileUtils.deleteDirectory(workDir)

    replaceAll(
      bytes,
      tempDir.toString.getBytes,
      "/folder".getBytes
    )
  }

  def replaceAll(bytes: Array[Byte], from: Array[Byte], to: Array[Byte]): Array[Byte] = {
    val b = Array.newBuilder[Byte]
    var i = 0
    while (i < bytes.length) {
      val endFrom = i + from.length
      if (endFrom < bytes.length && util.Arrays.equals(bytes, i, endFrom, from, 0, from.length)) {
        b ++= to
        i = endFrom
      } else {
        b += bytes(i)
        i += 1
      }
    }
    b.result()
  }
}
