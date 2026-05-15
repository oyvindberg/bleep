package bleep

import java.nio.file.Path

/** Filesystem layout for everything bleep writes under `.bleep/` in a workspace.
  *
  * Two top-level categories:
  *
  *   - `.bleep/projects/<crossName>/...` — per cross-project state. Generated sources, generated resources, per-variant compile outputs (classes, analysis, KSP
  *     caches). One folder per cross-built variant (e.g. `mylib@jvm3`).
  *   - `.bleep/<workspace-level>/...` — state that spans projects: import scratch (`import/`), IDE preferences (`conf/`), per-variant build logs
  *     (`builds/<variant>/last.log`), publishing scratch (`sonatype-bundle/`), flamegraph artifacts (`traces/`).
  *
  * Within a cross-project folder, the shape is:
  *
  * {{{
  * .bleep/projects/<crossName>/
  * ├── generated-sources/         # SHARED across variants; source-like outputs
  * │   ├── annotations/           #   Java AP outputs
  * │   ├── <sourcegen-folder>/    #   sourcegen outputs (per ScriptDef.main)
  * │   └── ksp/{kotlin,java,resources}/
  * ├── generated-resources/       # SHARED across variants (sourcegen)
  * │   └── <sourcegen-folder>/
  * └── builds/<variant>/          # PER-VARIANT build state
  *     ├── classes/               #   kotlinc/javac/scalac output
  *     ├── test-classes/          #   only if isTestProject
  *     ├── analysis.zip           #   Zinc analysis
  *     ├── bloop.json             #   bleep-bsp compile config
  *     └── ksp/{caches,classes}/  #   KSP per-variant state
  * }}}
  *
  * The legacy `.bloop` segment is gone. Cross-project names use the `CrossProjectName.value` format (`myapp` for non-cross, `mylib@<crossId>` for cross
  * variants) as a single path segment.
  */
case class BuildPaths(cwd: Path, bleepYamlFile: Path, variant: model.BuildVariant, wantedBleepVersion: Option[model.BleepVersion]) {
  lazy val buildDir: Path = bleepYamlFile.getParent
  lazy val bspBleepJsonFile: Path = buildDir / ".bsp" / "bleep.json"
  lazy val dotBleepDir: Path = buildDir / ".bleep"

  // === workspace-level paths (project-agnostic) ===

  /** `<workspace>/.bleep/projects/`. Holds one subdirectory per cross-built project. */
  lazy val projectsDir: Path = dotBleepDir / "projects"

  /** `<workspace>/.bleep/builds/`. Holds per-variant workspace state (logs, etc.). */
  lazy val workspaceBuildsDir: Path = dotBleepDir / "builds"

  /** `<workspace>/.bleep/builds/<variant>/`. Per-variant scratch for this workspace, not tied to any single project. */
  lazy val workspaceVariantDir: Path = workspaceBuildsDir / variant.name

  /** `<workspace>/.bleep/builds/<variant>/last.log` — the build log from the most recent compile in this variant. */
  lazy val logFile: Path = workspaceVariantDir / "last.log"

  /** `<workspace>/.bleep/import/` — scratch for one-shot import commands (`bleep import`, `bleep import-maven`). */
  lazy val bleepImportDir: Path = dotBleepDir / "import"
  lazy val bleepImportBloopDir: Path = bleepImportDir / "bloop"
  lazy val bleepImportSbtExportDir: Path = bleepImportDir / "sbt-export"
  lazy val bleepImportMavenDir: Path = bleepImportDir / "maven"

  /** `<workspace>/.bleep/conf/` — IDE / user preferences scoped to this workspace. */
  lazy val localConfig: Path = dotBleepDir / "conf"
  lazy val bspProjectSelectionYaml: Path = localConfig / "bsp-project-selection.yaml"

  // === per cross-project paths ===

  /** `<workspace>/.bleep/projects/<crossName>/`. Everything related to one cross variant of one project lives here. */
  def crossProjectDir(crossName: model.CrossProjectName): Path =
    projectsDir / crossName.value

  /** `<workspace>/.bleep/projects/<crossName>/builds/<variant>/`. Per-variant compile state for one cross-project. */
  def variantBuildDir(crossName: model.CrossProjectName): Path =
    crossProjectDir(crossName) / "builds" / variant.name

  /** Source-like generated outputs, SHARED across variants. Located under the cross-project's `generated-sources/` directory. */
  def generatedSourcesDir(crossName: model.CrossProjectName, folderName: String): Path =
    crossProjectDir(crossName) / "generated-sources" / folderName

  /** Resource-like generated outputs, SHARED across variants. Located under the cross-project's `generated-resources/` directory. */
  def generatedResourcesDir(crossName: model.CrossProjectName, folderName: String): Path =
    crossProjectDir(crossName) / "generated-resources" / folderName

  // === project resolution ===

  final def project(crossName: model.CrossProjectName, p: model.Project): ProjectPaths = {
    val dir = buildDir / p.folder.getOrElse(RelPath.force(crossName.name.value))
    val scalaVersion: Option[model.VersionScala] = p.scala.flatMap(_.version)
    val maybePlatformId = p.platform.flatMap(_.name)
    val maybePlatformVersion = p.platform.flatMap(p => p.jsVersion.map(_.scalaJsVersion).orElse(p.nativeVersion.map(_.scalaNativeVersion)))

    val targetDir = variantBuildDir(crossName)
    val replacements =
      model.Replacements.paths(buildDir) ++
        model.Replacements.projectPaths(dir) ++
        model.Replacements.targetDir(targetDir) ++
        model.Replacements.scope(p.`sbt-scope`.getOrElse("")) ++
        model.Replacements.versions(
          wantedBleepVersion,
          scalaVersion,
          maybePlatformId,
          maybePlatformVersion,
          includeEpoch = true,
          includeBinVersion = true,
          buildDir = Some(buildDir)
        )

    def sourceLayout = p.`source-layout` match {
      case Some(sourceLayout) => sourceLayout
      case None               => if (scalaVersion.isDefined) model.SourceLayout.Normal else model.SourceLayout.Java
    }

    val sources = {
      val fromSourceLayout = sourceLayout.sources(scalaVersion, maybePlatformId, p.`sbt-scope`).values.map(dir / _)
      val fromJson = p.sources.values.map(relPath => (relPath, dir / replacements.fill.relPath(relPath))).toMap
      val generated = p.sourcegen.values.iterator.map(sourceGen => (sourceGen, generatedSourcesDir(crossName, sourceGen.folderName))).toMap
      val annotationProcessing =
        p.java
          .filter(j => j.scanForAnnotationProcessors.contains(true) || j.annotationProcessors.values.nonEmpty)
          .map(_ => generatedSourcesDir(crossName, "annotations"))
      val ksp: List[Path] =
        p.kotlin.filter(_.hasSymbolProcessing).toList.flatMap { _ =>
          val base = generatedSourcesDir(crossName, "ksp")
          List(base / "kotlin", base / "java")
        }
      ProjectPaths.DirsByOrigin(fromSourceLayout, fromJson, generated, annotationProcessing, ksp)
    }

    val resources = {
      val fromSourceLayout = sourceLayout.resources(scalaVersion, maybePlatformId, p.`sbt-scope`).values.map(dir / _)
      val fromJson = p.resources.values.iterator.map(relPath => (relPath, dir / replacements.fill.relPath(relPath))).toMap
      val generated = p.sourcegen.values.iterator.map(sourceGen => (sourceGen, generatedResourcesDir(crossName, sourceGen.folderName))).toMap
      // KSP resources land at .bleep/projects/<cross>/generated-sources/ksp/resources/; expose them so they're packaged like normal resources.
      val ksp: List[Path] =
        p.kotlin.filter(_.hasSymbolProcessing).toList.map(_ => generatedSourcesDir(crossName, "ksp") / "resources")
      ProjectPaths.DirsByOrigin(fromSourceLayout, fromJson, generated, None, ksp)
    }

    ProjectPaths(dir = dir, targetDir = targetDir, sourcesDirs = sources, resourcesDirs = resources, isTestProject = p.isTestProject.getOrElse(false))
  }
}

object BuildPaths {
  def apply(cwd: Path, buildLoader: BuildLoader, variant: model.BuildVariant): BuildPaths =
    BuildPaths(cwd, buildLoader.bleepYaml, variant, buildLoader.existing.toOption.flatMap(_.wantedVersion.forceGet.toOption))
}
