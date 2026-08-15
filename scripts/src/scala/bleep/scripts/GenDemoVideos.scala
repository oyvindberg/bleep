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
    val bleepVersion = detectBleepVersion(bleepBinary, logger)

    // `bleep import` shells out to sbt, which needs a JVM: the machine may have no system JVM. older sbt releases
    // (scala 2.12 compiler) crash on very new JDKs, so fetch an LTS one instead of lending it the build's
    val javaHome = started.pre.fetchJvm(model.Jvm("temurin:17", None)).javaBin.getParent.getParent

    ensureSbtExportDependenciesInIvyLocal(logger)

    val env = sys.env
      .updated("BAT_PAGER", "")
      .updated("GIT_PAGER", "cat")
      .removed(bleepLoggers.CallerProcessAcceptsJsonEvents)
      // typing-simulation pacing for asciinema-rec_script
      .updated("PROMPT_PAUSE", "1")
      .updated("TYPING_PAUSE", "0.02")
      .updated("JAVA_HOME", javaHome.toString)
      .updated(
        "PATH", {
          // this whole exercise is really to make any provided binary look like "bleep" in the videos
          val tempDir = Files.createTempDirectory("bleep-videos")
          Files.createSymbolicLink(tempDir / "bleep", bleepBinary)
          val javaBin = javaHome / "bin"
          sys.env.get("PATH") match {
            case Some(existingPath) => s"$tempDir:$javaBin:$existingPath"
            case None               => s"$tempDir:$javaBin"
          }
        }
      )

    implicit val ec: ExecutionContext = started.executionContext

    val repoDir = started.buildPaths.buildDir

    val generating: List[Future[Map[RelPath, String]]] =
      demos.map { demo =>
        Future {
          val generated = genVideo(demo, bleepVersion, repoDir, env.toList, logger.withPath(demo.name))
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

  /** `bleep import` makes the imported sbt build resolve `sbt-export-dependencies`, which was published to Maven Central with maven-style artifact filenames
    * (`sbt-export-dependencies_2.12_1.0-0.4.0.jar`). sbt older than 1.9 only knows the legacy sbt-plugin filename convention and cannot resolve that — and the
    * import demo clones a build pinned to sbt 1.7. Seed the local ivy repository (which old sbt checks with its own layout) with the artifact, so the demo
    * records the same import experience users with modern sbt get. Idempotent: does nothing when the artifact is already there.
    */
  def ensureSbtExportDependenciesInIvyLocal(logger: Logger): Unit = {
    val version = "0.4.0"
    val base = Path.of(sys.props("user.home")) / ".ivy2" / "local" / "build.bleep" / "sbt-export-dependencies" / "scala_2.12" / "sbt_1.0" / version
    val jarFile = base / "jars" / "sbt-export-dependencies.jar"
    val ivyFile = base / "ivys" / "ivy.xml"

    if (!FileUtils.exists(jarFile)) {
      val uri = java.net.URI.create(
        s"https://repo1.maven.org/maven2/build/bleep/sbt-export-dependencies_2.12_1.0/$version/sbt-export-dependencies_2.12_1.0-$version.jar"
      )
      logger.info(s"seeding $jarFile from $uri so the import demo's old sbt can resolve it")
      Files.createDirectories(jarFile.getParent)
      val in = uri.toURL.openStream()
      try Files.copy(in, jarFile).discard()
      finally in.close()
    }

    if (!FileUtils.exists(ivyFile)) {
      val ivyXml =
        s"""<?xml version="1.0" encoding="UTF-8"?>
           |<ivy-module version="2.0" xmlns:e="http://ant.apache.org/ivy/extra">
           |	<info organisation="build.bleep" module="sbt-export-dependencies" revision="$version" status="release" e:scalaVersion="2.12" e:sbtVersion="1.0">
           |		<description>sbt-export-dependencies</description>
           |	</info>
           |	<configurations>
           |		<conf name="compile" visibility="public" description=""/>
           |		<conf name="runtime" visibility="public" description="" extends="compile"/>
           |		<conf name="test" visibility="public" description="" extends="runtime"/>
           |		<conf name="provided" visibility="public" description=""/>
           |		<conf name="optional" visibility="public" description=""/>
           |		<conf name="default" visibility="public" description="" extends="runtime"/>
           |	</configurations>
           |	<publications>
           |		<artifact name="sbt-export-dependencies" type="jar" ext="jar" conf="compile"/>
           |	</publications>
           |	<dependencies></dependencies>
           |</ivy-module>
           |""".stripMargin
      FileUtils.writeString(logger, None, ivyFile, ivyXml)
    }
  }

  /** the version a given bleep binary reports in its `--help` output */
  def detectBleepVersion(bleepBinary: Path, logger: Logger): String = {
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
        logger.info(s"bleep binary $bleepBinary has version $version")
        version
      case None => sys.error(s"Could not determine version of $bleepBinary from its --help output")
    }
  }

  def findOnPath(name: String): Option[Path] =
    sys.env
      .get("PATH")
      .iterator
      .flatMap(_.split(File.pathSeparator))
      .map(entry => Path.of(entry) / name)
      .find(Files.isExecutable)

  case class Generated(video: String, yaml: Option[String])

  def genVideo(demo: Demo, bleepVersion: String, repoDir: Path, env: List[(String, String)], logger: Logger): Generated = {
    val tempDir = Files.createTempDirectory(s"bleep-videos-${demo.name}")
    // nest one level to not include script and output file in file listings
    val workDir = tempDir / "demo"
    Files.createDirectories(workDir)
    logger.withContext("tempDir", tempDir).debug("using temporary directory")

    // stage files, then run off-camera setup so recording starts in a prepared workspace
    demo.files(bleepVersion).foreach { case (relPath, content) => FileUtils.writeString(logger, None, workDir / relPath, content) }
    demo.prepareScript(Path.of("bleep"), repoDir).foreach { prepare =>
      val prepareFile = tempDir / "prepare"
      FileUtils.writeString(logger, None, prepareFile, prepare)
      Files.setPosixFilePermissions(prepareFile, Exec)
      cli("prepare", workDir, List("bash", prepareFile.toString), logger = logger, out = cli.Out.ViaLogger(logger), env = env).discard()
    }

    val scriptFile = tempDir / "script"
    FileUtils.writeString(logger, None, scriptFile, demo.script(Path.of("bleep"), bleepVersion))
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

    // scrub machine-specific paths from the recording. the trailing regex catches temp paths the exact replaces
    // cannot: the TUI truncates long paths at the panel edge, leaving a prefix of the real path on screen
    val scrubbed = Files
      .readString(outputFile)
      .replace(tempDir.toRealPath().toString, "~")
      .replace(tempDir.toString, "~")
      .replace(sys.props("user.home"), "~")
      .replaceAll("(?:/private)?/var/folders/[A-Za-z0-9_./-]*", "~")

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

    // return anything prepare borrowed from the repo (e.g. a git worktree registration) before the temp dir vanishes
    demo.cleanupScript(Path.of("bleep"), repoDir).foreach { cleanup =>
      val cleanupFile = tempDir / "cleanup"
      FileUtils.writeString(logger, None, cleanupFile, cleanup)
      Files.setPosixFilePermissions(cleanupFile, Exec)
      cli("cleanup", workDir, List("bash", cleanupFile.toString), logger = logger, out = cli.Out.ViaLogger(logger), env = env).discard()
    }

    FileUtils.deleteDirectory(tempDir)

    Generated(video = video, yaml = maybeYaml)
  }
}
