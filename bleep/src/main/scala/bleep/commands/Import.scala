package bleep
package commands

import bleep.internal._
import bleep.logging.Logger
import bleep.rewrites.normalizeBuild
import bloop.config.Config
import cats.implicits.catsSyntaxTuple2Semigroupal
import com.monovore.decline.Opts
import io.circe.syntax._

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.stream.Collectors
import scala.jdk.CollectionConverters.ListHasAsScala

object Import {
  case class Options(
      ignoreWhenInferringTemplates: Set[model.ProjectName],
      skipSbt: Boolean
  )

  val skipSbt: Opts[Boolean] =
    Opts.flag("skip-sbt", "use if you already have generated bloop files and want to reimport from them").orFalse

  val ignoreWhenInferringTemplates: Opts[List[model.ProjectName]] = Opts
    .options[String](
      "ignore-when-templating",
      "some projects may differ much from the rest, for instance documentation and examples. considering these when computing templates may negatively affect the result",
      "i"
    )
    .orEmpty
    .map(_.map(model.ProjectName.apply))

  val opts: Opts[Options] =
    (skipSbt, ignoreWhenInferringTemplates).mapN { case (skipSbt, ignoreWhenInferringTemplates) =>
      Options(ignoreWhenInferringTemplates.toSet, skipSbt = skipSbt)
    }
}

// pardon the very imperative interface of the class with indirect flow through files. let's refactor later
case class Import(buildPaths: BuildPaths, logger: Logger, options: Import.Options) extends BleepCommand {
  override def run(): Either[BuildException, Unit] = {
    if (!options.skipSbt) {
      generateBloopFiles()
    }

    val bloopFiles = findGeneratedBloopFiles().map(readAndParseBloopFile)
    Right(generateBuildAndPersistFrom(bloopFiles))
  }

  def generateBloopFiles(): Unit = {
    val tempAddBloopPlugin = buildPaths.buildDir / "project" / "bleep-temp-add-bloop-plugin.sbt"

    FileUtils.writeString(
      tempAddBloopPlugin,
      s"""addSbtPlugin("ch.epfl.scala" % "sbt-bloop" % "1.4.13")"""
    )

    try {
      // only accepts relative paths
      val importDir = RelPath.relativeTo(buildPaths.buildDir, buildPaths.bleepImportDir)
      implicit val wd = buildPaths.buildDir
      cli(s"""sbt 'set Global / bloopConfigDir := baseDirectory.value / s"$importDir/bloop-$${scalaBinaryVersion.value}"' +bloopInstall""", logger)
    } finally Files.delete(tempAddBloopPlugin)
  }

  def findGeneratedBloopFiles(): Iterable[Path] =
    Files
      .list(buildPaths.bleepImportDir)
      .filter(Files.isDirectory(_))
      .flatMap(dir => Files.list(dir).filter(x => Files.isRegularFile(x) && x.getFileName.toString.endsWith(".json")))
      .collect(Collectors.toList[Path])
      .asScala

  def generateBuildAndPersistFrom(bloopFiles: Iterable[Config.File]): Unit = {
    val build0 = importBloopFilesFromSbt(logger, buildPaths, bloopFiles)
    val normalizedBuild = normalizeBuild(build0)
    val build = Templates(normalizedBuild, options.ignoreWhenInferringTemplates)

    // fail if we have done illegal rewrites during templating
    ExplodedBuild.diffProjects(Defaults.add(normalizedBuild), ExplodedBuild.of(build).dropTemplates) match {
      case empty if empty.isEmpty => ()
      case diffs =>
        logger.error("Project templating did illegal rewrites. Please report this as a bug")
        diffs.foreach { case (projectName, msg) => logger.withContext(projectName).error(msg) }
    }

    FileUtils.softWriteBytes(
      buildPaths.bleepJsonFile,
      build.asJson.foldWith(ShortenAndSortJson).spaces2.getBytes(StandardCharsets.UTF_8)
    )

    logger.info(s"Imported ${build0.projects.size} cross targets for ${build.projects.value.size} projects")
  }
}
