package bleep.bsp

import bleep.analysis.*
import ch.epfl.scala.bsp.*

import java.net.URI
import java.nio.file.{Files, Path}
import java.util.concurrent.atomic.AtomicReference

/** Loads and manages build configuration for the BSP server.
  *
  * This converts bleep build information into BSP data structures and maintains the mapping between BSP targets and actual compilation units.
  *
  * Thread safety: All state is stored in AtomicReference and updated atomically.
  */
class BuildLoader(workspaceRoot: Path) {

  /** Internal representation of a loaded project */
  case class ProjectInfo(
      targetId: BuildTargetIdentifier,
      name: String,
      sources: Set[Path],
      resources: Set[Path],
      classpath: List[Path],
      outputDir: Path,
      languageConfig: LanguageConfig,
      dependsOn: Set[BuildTargetIdentifier],
      isTest: Boolean,
      platform: BuildLoader.Platform
  )

  /** Immutable snapshot of build state */
  private case class BuildLoaderState(
      projects: Map[BuildTargetIdentifier, ProjectInfo],
      sourceToTarget: Map[Path, BuildTargetIdentifier]
  )

  private val state = new AtomicReference(BuildLoaderState(Map.empty, Map.empty))

  /** Load a minimal build configuration for testing/development.
    *
    * This creates a simple build with a single target. In production, this would parse bleep.yaml and load the full build.
    */
  def loadSimpleBuild(
      projectName: String,
      sources: Set[Path],
      classpath: List[Path],
      outputDir: Path,
      languageConfig: LanguageConfig,
      isTest: Boolean,
      platform: BuildLoader.Platform = BuildLoader.Platform.Jvm
  ): BuildState = {
    val targetUri = Uri(URI.create(s"${workspaceRoot.toUri}?id=$projectName"))
    val targetId = BuildTargetIdentifier(targetUri)

    val capabilities = BuildTargetCapabilities(
      canCompile = Some(true),
      canTest = Some(isTest),
      canRun = Some(!isTest),
      canDebug = Some(false)
    )

    val (languages, dataKind, data) = languageConfig match {
      case sc: ScalaConfig =>
        val scalaBuildTarget = ScalaBuildTarget(
          scalaOrganization = "org.scala-lang",
          scalaVersion = sc.version,
          scalaBinaryVersion = scalaBinaryVersion(sc.version),
          platform = ScalaPlatform.Jvm,
          jars = classpath.filter(_.toString.contains("scala")).map(p => Uri(p.toUri)),
          jvmBuildTarget = Some(
            JvmBuildTarget(
              javaHome = Some(Uri(new URI("file://" + System.getProperty("java.home")))),
              javaVersion = Some(System.getProperty("java.version"))
            )
          )
        )
        (List("scala", "java"), Some(BuildTargetDataKind.Scala), Some(scalaBuildTarget))

      case kc: KotlinConfig =>
        val kotlinBuildTarget = KotlinBuildTarget(
          kotlinVersion = kc.version,
          jvmTarget = kc.jvmTarget,
          kotlincOptions = kc.options,
          isK2 = isK2Kotlin(kc.version)
        )
        (List("kotlin", "java"), Some(KotlinBuildTargetDataKind.Kotlin), Some(kotlinBuildTarget))

      case _: JavaConfig =>
        val jvmBuildTarget = JvmBuildTarget(
          javaHome = Some(Uri(new URI("file://" + System.getProperty("java.home")))),
          javaVersion = Some(System.getProperty("java.version"))
        )
        (List("java"), Some(BuildTargetDataKind.Jvm), Some(jvmBuildTarget))
    }

    val buildTarget = BuildTarget(
      id = targetId,
      displayName = Some(projectName),
      baseDirectory = Some(Uri(workspaceRoot.toUri)),
      tags = List(if isTest then BuildTargetTag.Test else BuildTargetTag.Library),
      capabilities = capabilities,
      languageIds = languages,
      dependencies = List.empty,
      dataKind = dataKind,
      data = data.map(encodeData)
    )

    val projectInfo = ProjectInfo(
      targetId = targetId,
      name = projectName,
      sources = sources,
      resources = Set.empty,
      classpath = classpath,
      outputDir = outputDir,
      languageConfig = languageConfig,
      dependsOn = Set.empty,
      isTest = isTest,
      platform = platform
    )

    state.set(
      BuildLoaderState(
        projects = Map(targetId -> projectInfo),
        sourceToTarget = sources.map(p => p -> targetId).toMap
      )
    )

    BuildState(List(buildTarget))
  }

  /** Load a multi-project build configuration.
    *
    * @param projectConfigs
    *   list of (name, sources, classpath, outputDir, languageConfig, dependsOn, isTest)
    */
  def loadBuild(
      projectConfigs: List[(String, Set[Path], List[Path], Path, LanguageConfig, Set[String], Boolean, BuildLoader.Platform)]
  ): BuildState = {
    // First pass: create target IDs
    val nameToId: Map[String, BuildTargetIdentifier] = projectConfigs.map { case (name, _, _, _, _, _, _, _) =>
      val targetUri = Uri(URI.create(s"${workspaceRoot.toUri}?id=$name"))
      name -> BuildTargetIdentifier(targetUri)
    }.toMap

    // Second pass: create full project info and build targets
    val (infos, targets) = projectConfigs.map { case (name, sources, classpath, outputDir, languageConfig, dependsOnNames, isTest, platform) =>
      val targetId = nameToId(name)
      val dependsOnIds = dependsOnNames.flatMap(nameToId.get)

      val capabilities = BuildTargetCapabilities(
        canCompile = Some(true),
        canTest = Some(isTest),
        canRun = Some(!isTest),
        canDebug = Some(false)
      )

      val (languages, dataKind, data) = languageConfig match {
        case sc: ScalaConfig =>
          val scalaBuildTarget = ScalaBuildTarget(
            scalaOrganization = "org.scala-lang",
            scalaVersion = sc.version,
            scalaBinaryVersion = scalaBinaryVersion(sc.version),
            platform = ScalaPlatform.Jvm,
            jars = classpath.filter(_.toString.contains("scala")).map(p => Uri(p.toUri)),
            jvmBuildTarget = Some(
              JvmBuildTarget(
                javaHome = Some(Uri(new URI("file://" + System.getProperty("java.home")))),
                javaVersion = Some(System.getProperty("java.version"))
              )
            )
          )
          (List("scala", "java"), Some(BuildTargetDataKind.Scala), Some(scalaBuildTarget))

        case kc: KotlinConfig =>
          val kotlinBuildTarget = KotlinBuildTarget(
            kotlinVersion = kc.version,
            jvmTarget = kc.jvmTarget,
            kotlincOptions = kc.options,
            isK2 = isK2Kotlin(kc.version)
          )
          (List("kotlin", "java"), Some(KotlinBuildTargetDataKind.Kotlin), Some(kotlinBuildTarget))

        case _: JavaConfig =>
          val jvmBuildTarget = JvmBuildTarget(
            javaHome = Some(Uri(new URI("file://" + System.getProperty("java.home")))),
            javaVersion = Some(System.getProperty("java.version"))
          )
          (List("java"), Some(BuildTargetDataKind.Jvm), Some(jvmBuildTarget))
      }

      val buildTarget = BuildTarget(
        id = targetId,
        displayName = Some(name),
        baseDirectory = Some(Uri(workspaceRoot.toUri)),
        tags = List(if isTest then BuildTargetTag.Test else BuildTargetTag.Library),
        capabilities = capabilities,
        languageIds = languages,
        dependencies = dependsOnIds.toList,
        dataKind = dataKind,
        data = data.map(encodeData)
      )

      val projectInfo = ProjectInfo(
        targetId = targetId,
        name = name,
        sources = sources,
        resources = Set.empty,
        classpath = classpath,
        outputDir = outputDir,
        languageConfig = languageConfig,
        dependsOn = dependsOnIds,
        isTest = isTest,
        platform = platform
      )

      (targetId -> projectInfo, buildTarget)
    }.unzip

    val projectsMap = infos.toMap
    state.set(
      BuildLoaderState(
        projects = projectsMap,
        sourceToTarget = projectsMap.values.flatMap(p => p.sources.map(_ -> p.targetId)).toMap
      )
    )

    BuildState(targets)
  }

  /** Get project info for a build target */
  def getProject(targetId: BuildTargetIdentifier): Option[ProjectInfo] =
    state.get().projects.get(targetId)

  /** Get all project infos */
  def getAllProjects: Iterable[ProjectInfo] = state.get().projects.values

  /** Find which build target contains a source file */
  def findTargetForSource(sourcePath: Path): Option[BuildTargetIdentifier] = {
    val currentState = state.get()
    // First check exact path match
    currentState.sourceToTarget.get(sourcePath).orElse {
      // Then check if file is under any project's source directories
      val normalizedPath = sourcePath.toAbsolutePath.normalize()
      currentState.projects.values
        .find { p =>
          p.sources.exists { srcDir =>
            val normalizedSrcDir = srcDir.toAbsolutePath.normalize()
            normalizedPath.startsWith(normalizedSrcDir) || normalizedPath == normalizedSrcDir
          }
        }
        .map(_.targetId)
    }
  }

  /** Get sources for a build target */
  def getSources(targetId: BuildTargetIdentifier): List[SourceItem] =
    state.get().projects.get(targetId).toList.flatMap { project =>
      project.sources.toList.map { srcPath =>
        val kind = if Files.isDirectory(srcPath) then SourceItemKind.Directory else SourceItemKind.File
        SourceItem(
          uri = Uri(srcPath.toUri),
          kind = kind,
          generated = false,
          data = None,
          dataKind = None
        )
      }
    }

  /** Get resources for a build target */
  def getResources(targetId: BuildTargetIdentifier): List[Uri] =
    state.get().projects.get(targetId).toList.flatMap(_.resources.map(p => Uri(p.toUri)))

  /** Get classpath for a build target as Uri (for BSP responses that need Uri) */
  def getClasspath(targetId: BuildTargetIdentifier): List[Uri] =
    state.get().projects.get(targetId).toList.flatMap(_.classpath.map(p => Uri(p.toUri)))

  /** Get classpath for a build target as String (for JVM/Scalac/Javac options) */
  def getClasspathStrings(targetId: BuildTargetIdentifier): List[String] =
    state.get().projects.get(targetId).toList.flatMap(_.classpath.map(_.toUri.toString))

  /** Get output paths for a build target */
  def getOutputPaths(targetId: BuildTargetIdentifier): List[OutputPathItem] =
    state.get().projects.get(targetId).toList.map { project =>
      OutputPathItem(
        uri = Uri(project.outputDir.toUri),
        kind = OutputPathItemKind.Directory
      )
    }

  /** Create a compilation input for a build target */
  def createCompilationInput(
      targetId: BuildTargetIdentifier,
      sources: Seq[SourceFile]
  ): Option[CompilationInput] =
    state.get().projects.get(targetId).map { project =>
      CompilationInput(
        sources = sources,
        classpath = project.classpath,
        outputDir = project.outputDir,
        config = project.languageConfig
      )
    }

  // ============================================================================
  // Helper methods
  // ============================================================================

  private def scalaBinaryVersion(version: String): String =
    if version.startsWith("3.") then "3"
    else if version.startsWith("2.13.") then "2.13"
    else if version.startsWith("2.12.") then "2.12"
    else version

  private def isK2Kotlin(version: String): Boolean =
    // Kotlin 2.0+ uses K2 compiler
    version.split('.').headOption.flatMap(_.toIntOption).exists(_ >= 2)

  private def encodeData(data: Any): RawJson = {
    import com.github.plokhotnyuk.jsoniter_scala.core.*
    data match {
      case s: ScalaBuildTarget =>
        RawJson(writeToArray(s)(using ScalaBuildTarget.codec))
      case j: JvmBuildTarget =>
        RawJson(writeToArray(j)(using JvmBuildTarget.codec))
      case k: KotlinBuildTarget =>
        RawJson(writeToArray(k)(using KotlinBuildTarget.codec))
      case _ =>
        RawJson("{}".getBytes("UTF-8"))
    }
  }
}

object BuildLoader {

  /** Platform classification for a project */
  sealed trait Platform
  object Platform {
    case object Jvm extends Platform
    case class ScalaJs(sjsVersion: String, scalaVersion: String) extends Platform
    case class ScalaNative(snVersion: String, scalaVersion: String) extends Platform
    case class KotlinJs(kotlinVersion: String) extends Platform
    case class KotlinNative(kotlinVersion: String) extends Platform
  }

  /** Create a build loader for the given workspace */
  def forWorkspace(workspaceRoot: Path): BuildLoader =
    new BuildLoader(workspaceRoot)
}
