package bleep
package scripts

import bleep.internal.FileUtils
import bleep.plugin.nativeimage.NativeImagePlugin
import ryddig.Logger

import java.nio.file.{Files, Path}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.jdk.StreamConverters.StreamHasToScala

object It extends BleepScript("It") {
  override def run(started: Started, commands: Commands, args: List[String]): Unit = {
    val project = started.bloopProject(model.CrossProjectName(model.ProjectName("bleep-cli"), crossId = None))
    val ni = new NativeImagePlugin(project, started.logger, started.jvmCommand)

    val nativeImage = {
      val fromArgs = args.headOption.map { path =>
        RelPath.apply(path) match {
          case Left(_)        => Path.of(path)
          case Right(relPath) => started.buildPaths.cwd / relPath
        }
      }
      val possible = List(fromArgs, Some(ni.nativeImageOutput)).flatten
      possible.find(FileUtils.exists) match {
        case Some(found) =>
          started.logger.info(s"Using native-image at $found")
          found
        case None =>
          sys.error(
            s"Expected a native-image built at ${ni.nativeImageOutput} or provided as parameter (provided: $args. in directory: ${Files.list(started.buildPaths.cwd).toScala(List).mkString(", ")})"
          )
      }
    }

    val env = sys.env.updated("BAT_PAGER", "")
    implicit val ec: ExecutionContext = started.executionContext

    val bleepVersion = GenDemoVideos.detectBleepVersion(nativeImage, started.logger)

    Await
      .result(
        Future.sequence(
          Demo.all
            .filterNot(_.name == "import") // todo: iterate more on integration test concept. sbt import hangs in CI and would be slow anyway
            .filterNot(_.name == "diff") // needs bleep-test-runner published at the binary's version, which a freshly built native-image doesn't have
            .map(demo => Future(runDemo(nativeImage, bleepVersion, demo, env.toList, started.logger.withPath(demo.name))))
        ),
        Duration.Inf
      )
      .discard()
  }

  def runDemo(bleep: Path, bleepVersion: String, demo: Demo, env: List[(String, String)], logger: Logger): Unit = {
    val tempDir = Files.createTempDirectory(s"bleep-it-${demo.name}")
    var workDir = tempDir

    var action = Option.empty[String]

    demo.script(bleep, bleepVersion).lines().map(_.trim).forEach {
      case ""                                 => ()
      case comment if comment.startsWith("#") =>
        action = Some(comment.trim.dropWhile(c => c == '#' || c == ' ').takeWhile(c => c.isUnicodeIdentifierPart || c == ' '))
      case bat if bat.startsWith("bat") => // don't need to cat files here, and bat isn't necessarily installed everywhere
        ()
      case cd if cd.startsWith("cd") =>
        workDir = workDir / cd.drop("cd ".length)
      case silent if silent.startsWith(":") => // silently-run lines (see asciinema-rec_script) can use shell syntax, so run them through bash
        cli(action.getOrElse(silent), workDir, cmd = List("bash", "-c", silent.drop(1).trim), logger = logger, out = cli.Out.ViaLogger(logger), env = env)
          .discard()
      case line =>
        cli(action.getOrElse(line), workDir, cmd = line.split("\\s").toList, logger = logger, out = cli.Out.ViaLogger(logger), env = env).discard()
    }

    FileUtils.deleteDirectory(tempDir)
  }
}
