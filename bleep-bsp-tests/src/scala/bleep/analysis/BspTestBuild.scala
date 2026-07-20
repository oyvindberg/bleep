package bleep.analysis

import bleep.bsp.{BspBuildData, BuildLoader}
import bleep.{model, BuildPaths, ResolvedProject}

import java.nio.file.Path

/** Lowers the test harness's `ProjectConfig` descriptions into the `BspBuildData.Payload` that every real bleep client sends.
  *
  * The BSP server does not load builds — it compiles the one its client resolved — so a test that wants to drive it has to speak the same protocol rather than
  * reach inside and install build state. That is the whole point of retargeting these tests: they now exercise the server the way production does.
  *
  * Output locations are derived from [[BuildPaths]] rather than chosen per project. The server derives the Zinc analysis directory, the compile lock and the
  * test classpath the same way, so a test that named its own directory would compile into it and then find nothing there.
  */
object BspTestBuild {

  def payload(workspaceRoot: Path, configs: List[BspTestHarness.ProjectConfig]): BspBuildData.Payload = {
    val buildPaths = BuildPaths(
      cwd = workspaceRoot,
      bleepYamlFile = workspaceRoot.resolve(bleep.BuildLoader.BuildFileName),
      variant = model.BuildVariant.Normal,
      wantedBleepVersion = Some(model.BleepVersion.current)
    )

    val projects: Map[model.CrossProjectName, model.Project] =
      configs.map(cfg => crossName(cfg) -> project(cfg, configs)).toMap

    val build = model.Build.Exploded(
      $version = model.BleepVersion.current,
      explodedProjects = projects,
      resolvers = model.JsonList.empty,
      // The server forces `Prebootstrapped.resolvedJvm`, which reads through this. Leaving it None
      // means Jvm.system, and coursier reports "No system JVM found" in the test process. Naming the
      // JVM this repo builds with costs nothing — it is already in the coursier cache.
      jvm = Some(model.Jvm.graalvm),
      scripts = Map.empty,
      remoteCache = None
    )

    BspBuildData.Payload.of(
      variantName = buildPaths.variant.name,
      build = build,
      resolvedProjects = configs.map(cfg => crossName(cfg) -> resolved(cfg, buildPaths)).toMap
    )
  }

  /** Where a named project's classes land, matching what the server derives for it.
    *
    * Tests need this both to assert on compiler output and to put a dependency's classes on a dependent's classpath. Neither can hardcode a path any more: the
    * location is derived from BuildPaths, not chosen.
    */
  def classesDirFor(workspaceRoot: Path, projectName: String, isTest: Boolean): Path =
    BuildPaths(
      cwd = workspaceRoot,
      bleepYamlFile = workspaceRoot.resolve(bleep.BuildLoader.BuildFileName),
      variant = model.BuildVariant.Normal,
      wantedBleepVersion = Some(model.BleepVersion.current)
    ).variantBuildDir(model.CrossProjectName(model.ProjectName(projectName), crossId = None))
      .resolve(if (isTest) "test-classes" else "classes")

  /** Where a project's classes land, matching what the server derives for the same project. */
  def classesDir(workspaceRoot: Path, config: BspTestHarness.ProjectConfig): Path = {
    val buildPaths = BuildPaths(
      cwd = workspaceRoot,
      bleepYamlFile = workspaceRoot.resolve(bleep.BuildLoader.BuildFileName),
      variant = model.BuildVariant.Normal,
      wantedBleepVersion = Some(model.BleepVersion.current)
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
    // Only the fields the server actually reads off the model: dependsOn (for transitive ordering
    // and dependency locking), isTestProject, and the platform's main class for `buildTarget/run`.
    val dependsOn = model.JsonSet(config.dependsOn.map(model.ProjectName.apply).toSeq.sorted*)
    val _ = all
    model.Project.empty.copy(
      dependsOn = dependsOn,
      isTestProject = if (config.isTest) Some(true) else None
    )
  }

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
        // compilerJars stays empty: the server resolves the compiler from the version via its own
        // CompilerResolver cache, exactly as it did when these tests drove the old server.
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
        Some(ResolvedProject.Platform.Jvm(options = Nil, mainClass = None, runtimeOptions = Nil))
      case BuildLoader.Platform.ScalaJs(version, _) =>
        Some(
          ResolvedProject.Platform.Js(
            version = version,
            mode = "debug",
            kind = "application",
            emitSourceMaps = false,
            jsdom = None,
            nodePath = Some(Path.of(PlatformTestHelper.nodeBinary)),
            mainClass = None
          )
        )
      case BuildLoader.Platform.ScalaNative(version, _) =>
        Some(ResolvedProject.Platform.Native(version = version, mode = "debug", gc = "immix", mainClass = None))
      case BuildLoader.Platform.KotlinJs(version) =>
        Some(
          ResolvedProject.Platform.Js(
            version = version,
            mode = "debug",
            kind = "application",
            emitSourceMaps = false,
            jsdom = None,
            nodePath = Some(Path.of(PlatformTestHelper.nodeBinary)),
            mainClass = None
          )
        )
      case BuildLoader.Platform.KotlinNative(version) =>
        Some(ResolvedProject.Platform.Native(version = version, mode = "debug", gc = "immix", mainClass = None))
    }
}
