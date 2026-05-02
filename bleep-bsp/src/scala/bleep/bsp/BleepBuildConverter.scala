package bleep.bsp

import bleep.*
import bleep.analysis.{CompilerResolver, ProjectConfig, ProjectLanguage}
import bleep.model
import bleep.model.{CrossProjectName, Java, Kotlin}

import java.nio.file.Paths

/** Converts bleep's build model to the compilation infrastructure types.
  *
  * This bridges bleep-core's `Started` to bleep-bsp's `ProjectDag` and `ProjectConfig` for compilation, using the bleep-native ResolvedProject type.
  */
object BleepBuildConverter {

  /** Convert a ResolvedProject to ProjectConfig for compilation.
    *
    * @param crossName
    *   the cross-built project name
    * @param resolved
    *   the resolved project configuration
    * @param started
    *   the build context (for paths and model access)
    * @return
    *   ProjectConfig for compilation
    */
  def toProjectConfig(
      crossName: CrossProjectName,
      resolved: ResolvedProject,
      started: Started,
      additionalJavaOptions: List[String]
  ): ProjectConfig = {
    val sources = resolved.sources.map(p => Paths.get(p.toString)).toSet
    val classpath = resolved.classpath.map(p => Paths.get(p.toString)).toSeq
    val outputDir = Paths.get(resolved.classesDir.toString)

    // Get explicit configs from bleep model (if defined)
    val bleepProject = started.build.explodedProjects.get(crossName)
    val kotlinConfig = bleepProject.flatMap(_.kotlin)
    val javaConfig = bleepProject.flatMap(_.java)
    val platform = bleepProject.flatMap(_.platform)
    val isTest = bleepProject.exists(_.isTestProject.getOrElse(false))
    val language0 = detectLanguage(resolved, kotlinConfig, javaConfig, platform, isTest, additionalJavaOptions)

    // For Kotlin test projects, add -Xfriend-paths so tests can access `internal` members
    val language = language0 match {
      case kt: ProjectLanguage.Kotlin if isTest =>
        val friendPaths = computeFriendPaths(crossName, started)
        if (friendPaths.nonEmpty) {
          kt.copy(kotlinOptions = kt.kotlinOptions :+ s"-Xfriend-paths=${friendPaths.mkString(",")}")
        } else kt
      case other => other
    }

    // Use the same path structure as BuildPaths.targetDir: name/crossId
    val targetDir = started.buildPaths.bleepBloopDir.resolve(crossName.name.value).resolve(crossName.crossId.fold("")(_.value))
    val analysisDir = Some(targetDir.resolve(".zinc"))

    ProjectConfig(
      name = crossName.value,
      sources = sources,
      classpath = classpath,
      outputDir = outputDir,
      language = language,
      analysisDir = analysisDir,
      buildDir = started.buildPaths.buildDir
    )
  }

  /** Detect the language mode for a project based on explicit config.
    *
    * Priority:
    *   1. If project has explicit kotlin config with version:
    *      - If platform is JS -> KotlinJs
    *      - If platform is Native -> KotlinNative
    *      - Otherwise -> Kotlin (JVM)
    *   2. If project has Scala config -> ScalaJava
    *   3. Otherwise -> JavaOnly
    *
    * @param resolved
    *   the resolved project configuration
    * @param kotlinConfig
    *   explicit Kotlin configuration from bleep model (if any)
    * @param javaConfig
    *   explicit Java configuration from bleep model (if any)
    * @param platform
    *   explicit platform configuration from bleep model (if any)
    * @param isTest
    *   whether this is a test project
    */
  private def detectLanguage(
      resolved: ResolvedProject,
      kotlinConfig: Option[Kotlin],
      javaConfig: Option[Java],
      platform: Option[model.Platform],
      isTest: Boolean,
      additionalJavaOptions: List[String]
  ): ProjectLanguage =
    // Check for explicit Kotlin configuration first
    kotlinConfig.flatMap(_.version) match {
      case Some(kotlinVersion) =>
        val options = kotlinConfig.map(_.options.render).getOrElse(Nil)

        // Check platform to determine Kotlin target
        platform.flatMap(_.name) match {
          case Some(model.PlatformId.Js) =>
            ProjectLanguage.KotlinJs(
              kotlinVersion = kotlinVersion.kotlinVersion,
              kotlinOptions = options,
              isTest = isTest
            )

          case Some(model.PlatformId.Native) =>
            ProjectLanguage.KotlinNative(
              kotlinVersion = kotlinVersion.kotlinVersion,
              kotlinOptions = options,
              isTest = isTest
            )

          case _ =>
            // JVM platform (default)
            val jvmTarget = kotlinConfig.flatMap(_.jvmTarget).getOrElse("11")
            val pluginOptions = resolveCompilerPlugins(kotlinVersion, kotlinConfig.map(_.compilerPlugins.values.toList).getOrElse(Nil))
            // Extract Java release from java options (--release X, -source X, -target X)
            // Options may be rendered as separate strings ("--release", "21") or combined ("--release=21")
            val javaRelease = javaConfig.flatMap { java =>
              val opts = java.options.render
              val releaseIdx = opts.indexOf("--release")
              val sourceIdx = opts.indexOf("-source")
              val targetIdx = opts.indexOf("-target")
              if (releaseIdx >= 0 && releaseIdx + 1 < opts.size) opts(releaseIdx + 1).toIntOption
              else if (sourceIdx >= 0 && sourceIdx + 1 < opts.size) opts(sourceIdx + 1).toIntOption
              else if (targetIdx >= 0 && targetIdx + 1 < opts.size) opts(targetIdx + 1).toIntOption
              else
                opts.collectFirst {
                  case opt if opt.startsWith("--release=") => opt.stripPrefix("--release=").toIntOption
                }.flatten
            }
            ProjectLanguage.Kotlin(
              kotlinVersion = kotlinVersion.kotlinVersion,
              jvmTarget = jvmTarget,
              kotlinOptions = options ++ pluginOptions,
              javaRelease = javaRelease
            )
        }

      case None =>
        // Scala/Java project - check resolved config using the Language ADT
        resolved.language match {
          case scalaLang: ResolvedProject.Language.Scala =>
            val javaRelease = scalaLang.javaOptions.collectFirst {
              case opt if opt.startsWith("--release") =>
                opt.stripPrefix("--release").trim.toIntOption
              case opt if opt == "-release" =>
                None
            }.flatten
            ProjectLanguage.ScalaJava(
              scalaVersion = scalaLang.version,
              scalaOptions = scalaLang.options,
              javaRelease = javaRelease,
              javaOptions = scalaLang.javaOptions ++ additionalJavaOptions
            )

          case javaLang: ResolvedProject.Language.Java =>
            // Java only
            val javaRelease = javaLang.options.collectFirst {
              case opt if opt.startsWith("--release=") =>
                opt.stripPrefix("--release=").toIntOption
              case opt if opt.startsWith("-target") =>
                opt.stripPrefix("-target").trim.toIntOption
            }.flatten
            ProjectLanguage.JavaOnly(
              release = javaRelease,
              javaOptions = javaLang.options ++ additionalJavaOptions,
              ecjVersion = javaConfig.flatMap(_.ecjVersion).map(_.version)
            )

          case kotlinLang: ResolvedProject.Language.Kotlin =>
            // Kotlin from resolved language (should not normally happen if we checked kotlinConfig first)
            ProjectLanguage.Kotlin(
              kotlinVersion = kotlinLang.version,
              jvmTarget = "11",
              kotlinOptions = kotlinLang.options,
              javaRelease = None
            )
        }
    }

  /** Compute -Xfriend-paths for Kotlin test projects.
    *
    * In Maven/Gradle, test sources are part of the same Kotlin module as main sources, so they can access `internal` members. In bleep, test projects are
    * separate projects. The Kotlin compiler's -Xfriend-paths flag restores this access for the dependency's output directory.
    */
  private def computeFriendPaths(crossName: CrossProjectName, started: Started): List[String] =
    started.build.explodedProjects.get(crossName) match {
      case Some(project) =>
        project.dependsOn.values.flatMap { depName =>
          val depCrossName = CrossProjectName(depName, crossName.crossId)
          val resolved =
            if (started.build.explodedProjects.contains(depCrossName)) Some(depCrossName) else started.build.explodedProjects.keys.find(_.name == depName)
          resolved.flatMap { cn =>
            started.resolvedProjects.get(cn).flatMap { lazyResolved =>
              scala.util.Try(lazyResolved.forceGet).toOption.map(_.classesDir.toString)
            }
          }
        }.toList
      case None =>
        Nil
    }

  /** Resolve Kotlin compiler plugin JARs and generate compiler options.
    *
    * Maps plugin IDs (spring, jpa, allopen, noarg, etc.) to their Maven artifacts, resolves the JARs via coursier, and generates `-Xplugin=<path>` and
    * `-P plugin:<id>:preset=<preset>` options.
    */
  private def resolveCompilerPlugins(kotlinVersion: model.VersionKotlin, pluginIds: List[String]): List[String] = {
    if (pluginIds.isEmpty) return Nil

    val pluginJarPaths = pluginIds.map { pluginId =>
      CompilerResolver.resolveKotlinPlugin(pluginId, kotlinVersion)
    }

    val xpluginOpt = List(s"-Xplugin=${pluginJarPaths.map(_.toString).mkString(",")}")

    val presetOpts = pluginIds.flatMap {
      case "spring" => List("-P", "plugin:org.jetbrains.kotlin.allopen:preset=spring")
      case "jpa"    => List("-P", "plugin:org.jetbrains.kotlin.noarg:preset=jpa")
      case _        => Nil
    }

    xpluginOpt ++ presetOpts
  }

  /** Get transitive dependencies for a set of projects. */
  def transitiveDependencies(
      projects: Set[CrossProjectName],
      started: Started
  ): Set[CrossProjectName] = {
    def go(remaining: Set[CrossProjectName], visited: Set[CrossProjectName]): Set[CrossProjectName] =
      if (remaining.isEmpty) visited
      else {
        val next = remaining.head
        val rest = remaining.tail
        if (visited.contains(next)) {
          go(rest, visited)
        } else {
          val deps = started.build.resolvedDependsOn.get(next) match {
            case Some(resolvedDeps) => resolvedDeps.toSet
            case None               => Set.empty[CrossProjectName]
          }
          go(rest ++ deps, visited + next)
        }
      }

    go(projects, Set.empty)
  }
}
