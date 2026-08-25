package bleep.analysis

import bleep.bsp.{BspBuildData, BuildLoader}
import bleep.{model, BuildPaths, ResolvedProject}

import java.nio.file.Path

/** Lowers the test harness's `ProjectConfig` descriptions into the `BspBuildData.Payload` that every bleep client sends.
  *
  * The BSP server does not load a build. The BSP server compiles the build its client resolved. A test that drives the server uses the same protocol a
  * client uses rather than installing build state directly.
  *
  * [[BuildPaths]] determine every output location.
  */
object BspTestBuild {

  def payload(workspaceRoot: Path, configs: List[BspTestHarness.ProjectConfig]): BspBuildData.Payload = {
    val buildPaths = BuildPaths(
      cwd = workspaceRoot,
      bleepYamlFile = workspaceRoot.resolve(bleep.BuildLoader.BuildFileName),
      variant = model.BuildVariant.Normal,
      wantedBleepVersion = Option(model.BleepVersion.current)
    )

    val projects: Map[model.CrossProjectName, model.Project] =
      configs.map(cfg => crossName(cfg) -> project(cfg, configs)).toMap

    val build = model.Build.Exploded(
      // `BleepDevDeps` maps the `dev` version to the bleep-test-runner classes already on this JVM's classpath.
      $version = model.BleepVersion.dev,
      explodedProjects = projects,
      resolvers = model.JsonList.empty,
      // The server forces `Prebootstrapped.resolvedJvm`, which extracts this field. A None here
      // means `Jvm.system`. Coursier then reports "No system JVM found" in the test process. The
      // JVM this repository builds with already sits in the coursier cache.
      jvm = Option(model.Jvm.graalvm),
      scripts = Map.empty,
      remoteCache = None
    )

    BspBuildData.Payload.of(
      variantName = buildPaths.variant.name,
      build = build,
      resolvedProjects = configs.map(cfg => crossName(cfg) -> resolved(cfg, buildPaths)).toMap
    )
  }

  /** The directory for a project's .class files.*/
  def classesDirFor(workspaceRoot: Path, projectName: String, isTest: Boolean): Path =
    BuildPaths(
      cwd = workspaceRoot,
      bleepYamlFile = workspaceRoot.resolve(bleep.BuildLoader.BuildFileName),
      variant = model.BuildVariant.Normal,
      wantedBleepVersion = Option(model.BleepVersion.current)
    ).variantBuildDir(model.CrossProjectName(model.ProjectName(projectName), crossId = None))
      .resolve(if (isTest) "test-classes" else "classes")

  /** The directory for a project's .class files.*/
  def classesDir(workspaceRoot: Path, config: BspTestHarness.ProjectConfig): Path = {
    val buildPaths = BuildPaths(
      cwd = workspaceRoot,
      bleepYamlFile = workspaceRoot.resolve(bleep.BuildLoader.BuildFileName),
      variant = model.BuildVariant.Normal,
      wantedBleepVersion = Option(model.BleepVersion.current)
    )
    classesDirIn(buildPaths, config)
  }

  private def classesDirIn(buildPaths: BuildPaths, config: BspTestHarness.ProjectConfig): Path =
    buildPaths
      .variantBuildDir(crossName(config))
      .resolve(if (config.isTest) "test-classes" else "classes")

  private def crossName(config: BspTestHarness.ProjectConfig): model.CrossProjectName =
    model.CrossProjectName(model.ProjectName(config.name), crossId = None)

  private def project(config: BspTestHarness.ProjectConfig, all: List[BspTestHarness.ProjectConfig]): model.Project = {
    // The server extracts fields from a model project. `dependsOn` orders transitive
    // compiles and locks dependencies. `isTestProject` marks a test project. The platform's main
    // class serves `buildTarget/run`. The platform and the Scala version pick a test runner for
    // `buildTarget/test`.
    val dependsOn = model.JsonSet(config.dependsOn.map(model.ProjectName.apply).toSeq.sorted*)
    val _ = all
    model.Project.empty.copy(
      dependsOn = dependsOn,
      isTestProject = Option.when(config.isTest)(true),
      scala = modelScala(config),
      platform = modelPlatform(config)
    )
  }

  private def modelScala(config: BspTestHarness.ProjectConfig): Option[model.Scala] =
    config.languageConfig match {
      case sc: ScalaConfig =>
        Option(
          model.Scala(
            version = Option(model.VersionScala(sc.version)),
            options = model.Options.empty,
            setup = None,
            compilerPlugins = model.JsonSet.empty,
            strict = None
          )
        )
      case _ => None
    }

  /** The server matches this platform to pick a test runner. A Scala.js or Scala Native suite reaches its own runner through `platform.name`. That runner then
    * uses the platform's version to pick a toolchain.
    *
    * @throws KotlinPlatformNotModelledException
    *   for a Kotlin platform. A Kotlin project takes its toolchain from `kotlin.version` alongside the platform.
    */
  private def modelPlatform(config: BspTestHarness.ProjectConfig): Option[model.Platform] =
    config.platform match {
      case BuildLoader.Platform.Jvm                 => None
      case BuildLoader.Platform.ScalaJs(version, _) =>
        Option(
          model.Platform.Js(
            jsVersion = model.VersionScalaJs(version),
            jsKind = None,
            jsSplitStyle = None,
            jsEmitSourceMaps = None,
            jsJsdom = None,
            jsNodeVersion = None,
            jsMainClass = None
          )
        )
      case BuildLoader.Platform.ScalaNative(version, _) =>
        Option(
          model.Platform.Native(
            nativeVersion = model.VersionScalaNative(version),
            nativeGc = None,
            nativeMainClass = None,
            nativeBuildTarget = None,
            nativeLinkerReleaseMode = None,
            nativeLTO = None,
            nativeMultithreading = None,
            nativeOptimize = None,
            nativeEmbedResources = None,
            nativeUseIncrementalCompilation = None
          )
        )
      case kotlin @ (_: BuildLoader.Platform.KotlinJs | _: BuildLoader.Platform.KotlinNative) =>
        throw KotlinPlatformNotModelledException(kotlin.toString)
    }

  /** `BspTestBuild` builds no `model.Project` for a Kotlin platform.
    */
  case class KotlinPlatformNotModelledException(platform: String)
      extends IllegalArgumentException(
        s"BspTestBuild builds no model project for $platform. The server picks a Kotlin test runner from kotlin.version together with the platform. " +
          "Set a Kotlin version on the model project alongside the platform."
      )

  private def resolved(config: BspTestHarness.ProjectConfig, buildPaths: BuildPaths): ResolvedProject =
    ResolvedProject(
      name = config.name,
      directory = buildPaths.buildDir,
      workspaceDir = buildPaths.buildDir,
      sources = config.sources.toList,
      classpath = config.classpath,
      classesDir = classesDirIn(buildPaths, config),
      resources = None,
      language = language(config),
      platform = platform(config),
      isTestProject = config.isTest,
      dependencies = config.dependsOn.toList.sorted,
      testFrameworks = Nil,
      resolution = None
    )

  private def language(config: BspTestHarness.ProjectConfig): ResolvedProject.Language =
    config.languageConfig match {
      case jc: JavaConfig =>
        ResolvedProject.Language.Java(options = jc.release.map(r => List("--release", r.toString)).getOrElse(Nil) ++ jc.options)
      case sc: ScalaConfig =>
        ResolvedProject.Language.Scala(
          organization = "org.scala-lang",
          name = if (sc.version.startsWith("3")) "scala3-compiler_3" else "scala-compiler",
          version = sc.version,
          options = sc.options,
          compilerJars = Nil,
          analysisFile = None,
          setup = None,
          javaOptions = Nil
        )
      case kc: KotlinConfig =>
        ResolvedProject.Language.Kotlin(
          version = kc.version,
          options = kc.options,
          compilerJars = Nil,
          javaOptions = Nil
        )
    }

  private def platform(config: BspTestHarness.ProjectConfig): Option[ResolvedProject.Platform] =
    config.platform match {
      case BuildLoader.Platform.Jvm =>
        Option(ResolvedProject.Platform.Jvm(options = Nil, mainClass = None, runtimeOptions = Nil))
      case BuildLoader.Platform.ScalaJs(version, _) =>
        Option(
          ResolvedProject.Platform.Js(
            version = version,
            mode = "debug",
            kind = "application",
            emitSourceMaps = false,
            jsdom = None,
            nodePath = Option(Path.of(PlatformTestHelper.nodeBinary)),
            mainClass = None
          )
        )
      case BuildLoader.Platform.ScalaNative(version, _) =>
        Option(ResolvedProject.Platform.Native(version = version, mode = "debug", gc = "immix", mainClass = None))
      case BuildLoader.Platform.KotlinJs(version) =>
        Option(
          ResolvedProject.Platform.Js(
            version = version,
            mode = "debug",
            kind = "application",
            emitSourceMaps = false,
            jsdom = None,
            nodePath = Option(Path.of(PlatformTestHelper.nodeBinary)),
            mainClass = None
          )
        )
      case BuildLoader.Platform.KotlinNative(version) =>
        Option(ResolvedProject.Platform.Native(version = version, mode = "debug", gc = "immix", mainClass = None))
    }
}
