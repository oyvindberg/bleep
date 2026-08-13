package bleep
package scripts

import bleep.internal.{bleepLoggers, FileUtils}
import ryddig.Logger

import java.io.File
import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

/** Records the asciinema demo videos under `bleep-site/static/demos`.
  *
  * Requires asciinema 3.x (`brew install asciinema`); `bat` is optional but makes typed-out commands syntax-highlighted. Casts are recorded in asciicast v2
  * format at a fixed window size, so they play in the site's asciinema-player regardless of the terminal this script runs in.
  *
  * Each [[Demo]] can stage files and run warm-up commands *before* recording starts ([[Demo.files]] / [[Demo.prepareScript]]), so the camera opens on an
  * already-prepared workspace instead of showing setup.
  *
  * Usage:
  * {{{
  *   bleep generate-videos                        # record every demo, using the `bleep` binary on PATH
  *   bleep generate-videos diff                   # record only the `diff` demo
  *   bleep generate-videos diff /path/to/bleep    # record with an explicit bleep binary
  * }}}
  */
object GenDemoVideos extends BleepScript("GenVideos") {
  val RecCmd = Path.of(getClass.getResource("/asciinema-rec_script").toURI)
  val Exec = PosixFilePermissions.fromString("rwxrwxr-x")

  override def run(started: Started, commands: Commands, args: List[String]): Unit = {
    val logger = started.logger.syncAccess(GenDemoVideos)

    val (binaryArgs, demoNames) = args.partition(arg => FileUtils.exists(Path.of(arg)))

    val demos = demoNames match {
      case Nil   => Demo.all
      case names =>
        names.map { name =>
          Demo.all.find(_.name == name) match {
            case Some(demo) => demo
            case None       => sys.error(s"unknown demo '$name'. available: ${Demo.all.map(_.name).mkString(", ")}")
          }
        }
    }

    val bleepBinary = binaryArgs.headOption.map(Path.of(_)).orElse(findOnPath("bleep")) match {
      case Some(binary) =>
        logger.info(s"Using bleep binary at $binary")
        binary
      case None =>
        sys.error("Expected a `bleep` binary on PATH or provided as parameter")
    }

    // the demo workspaces must pin `$version` to exactly the recorded binary, or bleep relaunches/warns on camera
    val bleepVersion = {
      val helpOutput = cli(
        "bleep --help",
        FileUtils.TempDir,
        List(bleepBinary.toString, "--no-color", "--help"),
        logger = logger,
        out = cli.Out.ViaLogger(logger.withPath("version-probe")),
        env = sys.env.toList
      )
      val VersionRegex = """.*\(version ([^)]+)\).*""".r
      helpOutput.stdout.collectFirst { case VersionRegex(version) => version } match {
        case Some(version) =>
          logger.info(s"Recording with bleep version $version")
          version
        case None => sys.error(s"Could not determine version of $bleepBinary from its --help output")
      }
    }

    val env = sys.env
      .updated("BAT_PAGER", "")
      .updated("GIT_PAGER", "cat")
      .removed(bleepLoggers.CallerProcessAcceptsJsonEvents)
      // typing-simulation pacing for asciinema-rec_script
      .updated("PROMPT_PAUSE", "1")
      .updated("TYPING_PAUSE", "0.02")
      .updated(
        "PATH", {
          // this whole exercise is really to make any provided binary look like "bleep" in the videos
          val tempDir = Files.createTempDirectory("bleep-videos")
          Files.createSymbolicLink(tempDir / "bleep", bleepBinary)
          sys.env.get("PATH") match {
            case Some(existingPath) => s"$tempDir:$existingPath"
            case None               => tempDir.toString
          }
        }
      )

    implicit val ec: ExecutionContext = started.executionContext

    val generating: List[Future[Map[RelPath, String]]] =
      demos.map { demo =>
        Future {
          val generated = genVideo(demo, bleepVersion, env.toList, logger.withPath(demo.name))
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
        deleteUnknowns = FileSync.DeleteUnknowns.No, // we may only be regenerating a subset of the demos
        soft = true
      )
      .log(started.logger, "wrote videos")
  }

  def findOnPath(name: String): Option[Path] =
    sys.env
      .get("PATH")
      .iterator
      .flatMap(_.split(File.pathSeparator))
      .map(entry => Path.of(entry) / name)
      .find(Files.isExecutable)

  case class Generated(video: String, yaml: Option[String])

  def genVideo(demo: Demo, bleepVersion: String, env: List[(String, String)], logger: Logger): Generated = {
    val tempDir = Files.createTempDirectory(s"bleep-videos-${demo.name}")
    // nest one level to not include script and output file in file listings
    val workDir = tempDir / "demo"
    Files.createDirectories(workDir)
    logger.withContext("tempDir", tempDir).debug("using temporary directory")

    // stage files, then run off-camera setup so recording starts in a prepared workspace
    demo.files(bleepVersion).foreach { case (relPath, content) => FileUtils.writeString(logger, None, workDir / relPath, content) }
    demo.prepareScript(Path.of("bleep")).foreach { prepare =>
      val prepareFile = tempDir / "prepare"
      FileUtils.writeString(logger, None, prepareFile, prepare)
      Files.setPosixFilePermissions(prepareFile, Exec)
      cli("prepare", workDir, List("bash", prepareFile.toString), logger = logger, out = cli.Out.ViaLogger(logger), env = env).discard()
    }

    val scriptFile = tempDir / "script"
    FileUtils.writeString(logger, None, scriptFile, demo.script(Path.of("bleep")))
    Files.setPosixFilePermissions(scriptFile, Exec)

    val outputFile = tempDir / "output"

    val cmd = List(
      RecCmd.toString,
      scriptFile.toString,
      // asciinema 3.x flags. asciicast v2 is what asciinema-player embeds expect
      "--title",
      demo.name,
      "--headless",
      "--window-size",
      s"${demo.columns}x${demo.rows}",
      "--output-format",
      "asciicast-v2",
      "--idle-time-limit",
      "1",
      "--overwrite",
      RelPath.relativeTo(workDir, outputFile).toString // this somehow needs to be relative
    )

    cli("asciinema-rec_script", workDir, cmd, logger = logger, out = cli.Out.ViaLogger(logger), env = env).discard()

    // scrub machine-specific paths from the recording
    val scrubbed = Files
      .readString(outputFile)
      .replace(tempDir.toRealPath().toString, "~")
      .replace(tempDir.toString, "~")
      .replace(sys.props("user.home"), "~")

    // the asciicast header records the (temporary) command it ran. drop it, it's machine-specific noise
    val video = scrubbed.split("\n", 2) match {
      case Array(headerLine, rest) =>
        val header = io.circe.parser.parse(headerLine) match {
          case Right(json) => json.mapObject(_.remove("command")).noSpaces
          case Left(err)   => throw err
        }
        s"$header\n$rest"
      case other => sys.error(s"unexpected cast file with ${other.length} lines")
    }
    val maybeYaml = demo.expectedYaml.map(yamlRelPath => Files.readString(workDir / yamlRelPath))

    FileUtils.deleteDirectory(tempDir)

    Generated(video = video, yaml = maybeYaml)
  }
}
