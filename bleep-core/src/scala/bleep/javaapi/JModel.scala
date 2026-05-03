package bleep.javaapi

import bleep.model
import bleep.packaging.ManifestCreator

import java.util
import java.util.Optional
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

/** Converters from bleep Scala model types to bleepscript Java records. */
object JModel {

  def crossProjectName(n: model.CrossProjectName): bleepscript.CrossProjectName =
    new bleepscript.CrossProjectName(
      n.name.value,
      n.crossId.map(_.value).toJava
    )

  def toCross(c: bleepscript.CrossProjectName): model.CrossProjectName =
    model.CrossProjectName(
      model.ProjectName(c.name),
      if (c.crossId.isPresent) Some(model.CrossId(c.crossId.get)) else None
    )

  def dep(d: model.Dep): bleepscript.Dep = d match {
    case j: model.Dep.JavaDependency =>
      new bleepscript.Dep.Java(
        j.organization.value,
        j.moduleName.value,
        j.version,
        j.transitive
      )
    case s: model.Dep.ScalaDependency =>
      new bleepscript.Dep.Scala(
        s.organization.value,
        s.baseModuleName.value,
        s.version,
        s.transitive,
        s.fullCrossVersion,
        s.forceJvm,
        s.for3Use213,
        s.for213Use3
      )
  }

  def relPath(r: bleep.RelPath): bleepscript.RelPath =
    new bleepscript.RelPath(r.segments.toList.asJava)

  def repository(r: model.Repository): bleepscript.Repository = r match {
    case m: model.Repository.Maven =>
      new bleepscript.Repository.Maven(m.name.map(_.value).toJava, m.uri)
    case mf: model.Repository.MavenFolder =>
      new bleepscript.Repository.MavenFolder(mf.name.map(_.value).toJava, mf.path)
    case i: model.Repository.Ivy =>
      new bleepscript.Repository.Ivy(i.name.map(_.value).toJava, i.uri)
  }

  def scriptDef(s: model.ScriptDef): bleepscript.ScriptDef = s match {
    case m: model.ScriptDef.Main =>
      new bleepscript.ScriptDef(
        crossProjectName(m.project),
        m.main,
        m.sourceGlobs.values.iterator.map(relPath).toSet.asJava
      )
  }

  def scalaConfig(s: model.Scala): bleepscript.ScalaConfig =
    new bleepscript.ScalaConfig(
      s.version.map(_.scalaVersion).toJava,
      s.options.render.asJava,
      s.compilerPlugins.values.iterator.map(dep).toSet.asJava,
      s.setup.map(compileSetup).toJava,
      s.strict.map(Boolean.box).toJava
    )

  def compileSetup(s: model.CompileSetup): bleepscript.CompileSetup =
    new bleepscript.CompileSetup(
      s.order.map(o => bleepscript.CompileSetup.CompileOrder.valueOf(o.toString)).toJava,
      s.addLibraryToBootClasspath.map(Boolean.box).toJava,
      s.addCompilerToClasspath.map(Boolean.box).toJava,
      s.addExtraJarsToClasspath.map(Boolean.box).toJava,
      s.manageBootClasspath.map(Boolean.box).toJava,
      s.filterLibraryFromClasspath.map(Boolean.box).toJava
    )

  def javaConfig(j: model.Java): bleepscript.JavaConfig =
    new bleepscript.JavaConfig(j.options.render.asJava)

  def kotlinConfig(k: model.Kotlin): bleepscript.KotlinConfig =
    new bleepscript.KotlinConfig(
      k.version.map(_.kotlinVersion).toJava,
      k.options.render.asJava
    )

  def platformConfig(p: model.Platform): bleepscript.PlatformConfig = p.name match {
    case Some(model.PlatformId.Jvm) | None =>
      new bleepscript.PlatformConfig.Jvm(
        p.mainClass.toJava,
        p.jvmOptions.render.asJava,
        p.jvmRuntimeOptions.render.asJava,
        p.jvmEnvironment.value.asJava
      )
    case Some(model.PlatformId.Js) =>
      new bleepscript.PlatformConfig.Js(
        p.mainClass.toJava,
        p.jsVersion.map(_.scalaJsVersion).toJava,
        p.jsNodeVersion.toJava,
        p.jsEmitSourceMaps.map(Boolean.box).toJava
      )
    case Some(model.PlatformId.Native) =>
      new bleepscript.PlatformConfig.Native(
        p.mainClass.toJava,
        p.nativeVersion.map(_.scalaNativeVersion).toJava,
        p.nativeGc.toJava
      )
  }

  def publishConfig(pc: model.PublishConfig): bleepscript.PublishConfig =
    new bleepscript.PublishConfig(
      pc.enabled.map(Boolean.box).toJava,
      pc.groupId.toJava,
      pc.description.toJava,
      pc.url.toJava,
      pc.organization.toJava,
      pc.developers.values.iterator
        .map(d => new bleepscript.PublishConfig.Developer(d.id, d.name, d.url))
        .toSet
        .asJava,
      pc.licenses.values.iterator
        .map(l =>
          new bleepscript.PublishConfig.License(
            l.name,
            l.url.toJava,
            l.distribution.toJava
          )
        )
        .toSet
        .asJava,
      pc.sonatypeProfileName.toJava,
      pc.sonatypeCredentialHost.toJava
    )

  def project(cross: model.CrossProjectName, p: model.Project): bleepscript.Project =
    new bleepscript.Project(
      crossProjectName(cross),
      p.dependencies.values.iterator.map(dep).toSet.asJava,
      p.dependsOn.values.iterator.map(_.value).toSet.asJava,
      p.sources.values.iterator.map(relPath).toSet.asJava,
      p.resources.values.iterator.map(relPath).toSet.asJava,
      p.scala.map(scalaConfig).toJava,
      p.java.map(javaConfig).toJava,
      p.kotlin.map(kotlinConfig).toJava,
      p.platform.map(platformConfig).toJava,
      p.isTestProject.getOrElse(false),
      p.testFrameworks.values.iterator.map(_.value).toSet.asJava,
      p.sourcegen.values.iterator.map(scriptDef).toSet.asJava,
      p.publish.map(publishConfig).toJava
    )

  def build(b: model.Build): bleepscript.Build = {
    val explodedJ = new util.LinkedHashMap[bleepscript.CrossProjectName, bleepscript.Project]()
    b.explodedProjects.foreach { case (cross, p) =>
      explodedJ.put(crossProjectName(cross), project(cross, p))
    }
    val resolversJ = b.resolvers.values.map(repository).asJava
    val scriptsJ = new util.LinkedHashMap[String, java.util.List[bleepscript.ScriptDef]]()
    b.scripts.foreach { case (name, defs) =>
      scriptsJ.put(name.value, defs.values.map(scriptDef).asJava)
    }
    new bleepscript.Build(
      new bleepscript.BleepVersion(b.$version.value),
      explodedJ,
      resolversJ,
      scriptsJ
    )
  }

  def buildPaths(bp: bleep.BuildPaths): bleepscript.BuildPaths =
    new bleepscript.BuildPaths(
      bp.cwd,
      bp.bleepYamlFile,
      bp.buildDir,
      bp.dotBleepDir,
      bp.buildsDir,
      bp.buildVariantDir,
      bp.bleepBloopDir,
      bp.logFile
    )

  def userPaths(up: bleep.UserPaths): bleepscript.UserPaths =
    new bleepscript.UserPaths(
      up.cacheDir,
      up.configDir,
      up.bspSocketDir,
      up.resolveCacheDir,
      up.resolveJvmCacheDir,
      up.configYaml
    )

  def projectPaths(pp: bleep.ProjectPaths): bleepscript.ProjectPaths =
    new bleepscript.ProjectPaths(
      pp.dir,
      pp.targetDir,
      pp.classes,
      pp.incrementalAnalysis,
      pp.sourcesDirs.all.toList.asJava,
      pp.resourcesDirs.all.toList.asJava,
      pp.isTestProject
    )

  def resolvedJvm(rj: bleep.ResolvedJvm): bleepscript.ResolvedJvm =
    new bleepscript.ResolvedJvm(
      rj.jvm.name,
      rj.jvm.index.toJava,
      rj.javaBin
    )

  def resolvedProject(rp: bleep.ResolvedProject): bleepscript.ResolvedProject = {
    val language: bleepscript.ResolvedProject.Language = rp.language match {
      case j: bleep.ResolvedProject.Language.Java =>
        new bleepscript.ResolvedProject.Language.Java(j.options.asJava)
      case s: bleep.ResolvedProject.Language.Scala =>
        new bleepscript.ResolvedProject.Language.Scala(
          s.organization,
          s.name,
          s.version,
          s.options.asJava,
          s.compilerJars.asJava,
          s.analysisFile.toJava,
          s.javaOptions.asJava
        )
      case k: bleep.ResolvedProject.Language.Kotlin =>
        new bleepscript.ResolvedProject.Language.Kotlin(
          k.version,
          k.options.asJava,
          k.compilerJars.asJava,
          k.javaOptions.asJava
        )
    }
    new bleepscript.ResolvedProject(
      rp.name,
      rp.directory,
      rp.workspaceDir,
      rp.sources.asJava,
      rp.classpath.asJava,
      rp.classesDir,
      rp.resources.map(_.asJava: java.util.List[java.nio.file.Path]).toJava,
      language,
      rp.isTestProject,
      rp.dependencies.asJava,
      rp.testFrameworks.asJava
    )
  }

  def defaultManifestCreator(): bleepscript.ManifestCreator =
    new JManifestCreator(ManifestCreator.default)
}
