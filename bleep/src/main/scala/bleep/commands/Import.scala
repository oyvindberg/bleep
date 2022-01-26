package bleep
package commands

import bleep.internal.{Os, ShortenAndSortJson, Templates}
import bleep.logging.Logger
import io.circe.syntax._

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files

case class Import(logger: Logger, ignoreWhenInferringTemplates: Set[model.ProjectName]) extends BleepCommand {

  /** The `Dep.ScalaDependency` structure has three fields we can only correctly determine in context of a given scala version. We need to propagate those three
    * flags up to all projects with same scala version or platform. After that, the "combine by cross" functionality will work better
    */
  def unifyDeps(allDeps: Iterable[Dep]): Map[Dep, Dep] = {
    val javaDeps = allDeps.collect { case x: Dep.JavaDependency => x }
    val scalaDeps = allDeps.collect { case x: Dep.ScalaDependency => x }
    val rewrittenScalaDeps: Map[Dep, Dep] =
      scalaDeps
        .groupBy(x => x.copy(forceJvm = false, for3Use213 = false, for213Use3 = false))
        .flatMap { case (base, providedByBase) =>
          val combined = base.copy(
            forceJvm = providedByBase.exists(_.forceJvm),
            for3Use213 = providedByBase.exists(_.for3Use213),
            for213Use3 = providedByBase.exists(_.for213Use3)
          )
          providedByBase.map(provided => (provided, combined))
        }
    rewrittenScalaDeps ++ javaDeps.map(x => (x, x))
  }

  override def run(): Unit = {
    val buildPaths = BuildPaths(Os.cwd / Defaults.BuildFileName)

    cli("sbt 'set Global / bloopConfigDir := baseDirectory.value / s\".bleep/import/bloop-${scalaBinaryVersion.value}\"' +bloopInstall")(buildPaths.buildDir)

    val build0 = importBloopFilesFromSbt(logger, buildPaths)

    val unifiedDeps: Map[Dep, Dep] =
      unifyDeps(build0.projects.flatMap(_._2.dependencies.values))

    val build1 = build0.copy(projects = build0.projects.map { case (crossName, p) =>
      val newP = p.copy(
        dependencies = p.dependencies.map(unifiedDeps.apply),
        scala = p.scala.map(removeScalaDefaults),
        platform = p.platform.map(removePlatformDefaults),
        `source-layout` = p.`source-layout`.filterNot(_ == SourceLayout.Normal)
      )
      (crossName, newP)
    })

    val build = Templates(build1, ignoreWhenInferringTemplates)

    Files.writeString(
      buildPaths.bleepJsonFile,
      build.asJson.foldWith(ShortenAndSortJson).spaces2,
      UTF_8
    )

    logger.info(s"Imported ${build0.projects.size} cross targets for ${build.projects.value.size} projects")
  }

  def removeScalaDefaults(ret: model.Scala): model.Scala =
    ret.copy(setup = ret.setup.map(setup => setup.removeAll(Defaults.DefaultCompileSetup)))

  def removePlatformDefaults(x: model.Platform): model.Platform =
    x.removeAll(Defaults.Jvm)
}
