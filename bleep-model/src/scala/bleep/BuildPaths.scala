package bleep

import java.nio.file.Path

case class BuildPaths(cwd: Path, bleepYamlFile: Path, mode: BuildPaths.Mode, wantedBleepVersion: Option[model.BleepVersion]) {
  lazy val buildDir = bleepYamlFile.getParent
  lazy val bspBleepJsonFile: Path = buildDir / ".bsp" / "bleep.json"
  lazy val dotBleepDir: Path = buildDir / ".bleep"
  lazy val bleepImportDir: Path = dotBleepDir / "import"
  lazy val bleepImportBloopDir: Path = dotBleepDir / "import" / "bloop"
  lazy val bleepImportSbtExportDir: Path = dotBleepDir / "import" / "sbt-export"
  lazy val dotBleepBspModeDir: Path = dotBleepDir / "bsp"
  lazy val dotBleepModeDir: Path = mode match {
    case BuildPaths.Mode.Normal => dotBleepDir
    case BuildPaths.Mode.BSP    => dotBleepBspModeDir
  }

  lazy val bleepBloopDir: Path = dotBleepModeDir / ".bloop"
  lazy val digestFile: Path = bleepBloopDir / ".digest"
  lazy val logFile: Path = dotBleepModeDir / "last.log"
  lazy val bspProjectSelectionYamlFile: Path = dotBleepBspModeDir / "project-selection.yaml"

  final def project(crossName: model.CrossProjectName, p: model.Project): ProjectPaths = {
    val dir = buildDir / p.folder.getOrElse(RelPath.force(crossName.name.value))
    val scalaVersion: Option[model.VersionScala] = p.scala.flatMap(_.version)
    val maybePlatformId = p.platform.flatMap(_.name)
    val maybePlatformVersion = p.platform.flatMap(p => p.jsVersion.map(_.scalaJsVersion).orElse(p.nativeVersion.map(_.scalaNativeVersion)))

    val targetDir = bleepBloopDir / crossName.name.value / crossName.crossId.fold("")(_.value)
    val replacements =
      model.Replacements.paths(buildDir) ++
        model.Replacements.projectPaths(dir) ++
        model.Replacements.targetDir(targetDir) ++
        model.Replacements.scope(p.`sbt-scope`.getOrElse("")) ++
        model.Replacements.versions(wantedBleepVersion, scalaVersion, maybePlatformId, maybePlatformVersion, includeEpoch = true, includeBinVersion = true)

    def sourceLayout = p.`source-layout` match {
      case Some(sourceLayout) => sourceLayout
      case None               => if (scalaVersion.isDefined) model.SourceLayout.Normal else model.SourceLayout.Java
    }

    val sources = {
      val fromSourceLayout = sourceLayout.sources(scalaVersion, maybePlatformId, p.`sbt-scope`).values.map(dir / _)
      val fromJson = p.sources.values.map(relPath => (relPath, dir / replacements.fill.relPath(relPath))).toMap
      val generated = generatedSourcesDir(crossName)
      ProjectPaths.DirsByOrigin(fromSourceLayout, fromJson, generated)
    }

    val resources = {
      val fromSourceLayout = sourceLayout.resources(scalaVersion, maybePlatformId, p.`sbt-scope`).values.map(dir / _)
      val fromJson = p.resources.values.map(relPath => (relPath, dir / replacements.fill.relPath(relPath))).toMap
      val generated = generatedResourcesDir(crossName)
      ProjectPaths.DirsByOrigin(fromSourceLayout, fromJson, generated)
    }

    ProjectPaths(
      dir = dir,
      targetDir = targetDir,
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

  def apply(cwd: Path, buildLoader: BuildLoader, mode: BuildPaths.Mode): BuildPaths =
    BuildPaths(cwd, buildLoader.bleepYaml, mode, buildLoader.existing.toOption.flatMap(_.wantedVersion.forceGet.toOption))
}
