package bleep

import bleep.internal.{FileUtils, Replacements}

import java.nio.file.Path

trait BuildPaths {
  val cwd: Path
  val buildDir: Path

  lazy val bleepJsonFile: Path = buildDir / constants.BuildFileName
  lazy val bspBleepJsonFile: Path = buildDir / ".bsp" / "bleep.json"
  lazy val dotBleepDir: Path = buildDir / ".bleep"
  lazy val bleepImportDir: Path = dotBleepDir / "import"
  lazy val bleepImportBloopDir: Path = dotBleepDir / "import" / "bloop"
  lazy val bleepImportSbtExportDir: Path = dotBleepDir / "import" / "sbt-export"
  lazy val dotBleepBspModeDir: Path = dotBleepDir / "bsp"
  def dotBleepModeDir: Path
  lazy val bleepBloopDir: Path = dotBleepModeDir / ".bloop"
  lazy val digestFile: Path = bleepBloopDir / ".digest"
  lazy val logFile: Path = dotBleepModeDir / "last.log"
  lazy val bspProjectSelectionJsonFile: Path = dotBleepBspModeDir / "project-selection.json"

  final def from(crossName: model.CrossProjectName, p: model.Project): ProjectPaths = {
    val dir = buildDir / p.folder.getOrElse(RelPath.force(crossName.name.value))
    val scalaVersion: Option[Versions.Scala] = p.scala.flatMap(_.version)
    val maybePlatformId = p.platform.flatMap(_.name)
    val replacementsVersions = Replacements.versions(scalaVersion, maybePlatformId.map(_.value))

    def sourceLayout = p.`source-layout` match {
      case Some(sourceLayout) => sourceLayout
      case None               => if (scalaVersion.isDefined) SourceLayout.Normal else SourceLayout.Java
    }

    val sources: JsonSet[Path] = {
      val fromSourceLayout = sourceLayout.sources(scalaVersion, maybePlatformId, p.`sbt-scope`).values.map(dir / _)
      val fromJson = p.sources.values.map(relPath => dir / replacementsVersions.fill.relPath(relPath))
      val generated = generatedSourcesDir(crossName)
      JsonSet.fromIterable(fromSourceLayout ++ fromJson ++ List(generated))
    }

    val resources: JsonSet[Path] = {
      val fromSourceLayout = sourceLayout.resources(scalaVersion, maybePlatformId, p.`sbt-scope`).values.map(dir / _)
      val fromJson = p.resources.values.map(relPath => dir / replacementsVersions.fill.relPath(relPath))
      val generated = generatedResourcesDir(crossName)
      JsonSet.fromIterable(fromSourceLayout ++ fromJson + generated)
    }

    ProjectPaths(
      dir = dir,
      targetDir = bleepBloopDir / crossName.name.value / crossName.crossId.fold("")(_.value),
      sourcesDirs = sources,
      resourcesDirs = resources
    )
  }

  def generatedSourcesDir(crossName: model.CrossProjectName): Path = dotBleepDir / "generated-sources" / crossName.value
  def generatedResourcesDir(crossName: model.CrossProjectName): Path = dotBleepDir / "generated-resources" / crossName.value
}

object BuildPaths {
  sealed trait Mode
  object Mode {
    case object Normal extends Mode
    // put bloop files into a different directory for IDE use, since we add flags and select a subset of projects
    case object BSP extends Mode
  }

  def find(cwd: Path, mode: Mode): Either[BuildException.BuildNotFound, BuildPaths] = {
    // keep looking up until we find build file
    def in(dir: Path): Option[Path] = {
      val buildFile = dir / constants.BuildFileName
      if (FileUtils.exists(buildFile)) Some(buildFile)
      else Option(dir.getParent).flatMap(in)
    }

    in(cwd) match {
      case Some(bleepJsonPath) => Right(fromBuildDir(cwd, bleepJsonPath.getParent, mode))
      case None                => Left(new BuildException.BuildNotFound(cwd))
    }
  }

  def fromBuildDir(_cwd: Path, _buildDir: Path, mode: Mode): BuildPaths =
    new BuildPaths {
      override val cwd = _cwd
      override val buildDir = _buildDir

      override lazy val dotBleepModeDir: Path = mode match {
        case Mode.Normal => dotBleepDir
        case Mode.BSP    => dotBleepBspModeDir
      }
    }
}
