package bleep
package mavenimport

import coursier.core.{Configuration, ModuleName, Organization}
import ryddig.Logger

import java.net.URI
import java.nio.file.{Files, Path}

object buildFromMavenPom {

  // Scala library artifacts that bleep provides automatically
  private val providedScalaArtifacts: Set[String] = Set(
    "scala-library",
    "scala-reflect",
    "scala-compiler",
    "scala3-library_3",
    "scala3-compiler_3",
    "scala3-interfaces"
  )

  // Well-known test framework detection by artifact name (direct dependency)
  private val testFrameworksByArtifact: Map[String, String] = Map(
    "junit-jupiter-api" -> "com.github.sbt.junit.JupiterFramework",
    "junit-jupiter-engine" -> "com.github.sbt.junit.JupiterFramework",
    "junit-jupiter" -> "com.github.sbt.junit.JupiterFramework",
    "junit" -> "com.novocode.junit.JUnitFramework",
    "kotlin-test-junit" -> "com.novocode.junit.JUnitFramework",
    "scalatest" -> "org.scalatest.tools.Framework",
    "specs2-core" -> "org.specs2.runner.Specs2Framework",
    "munit" -> "munit.Framework",
    "utest" -> "utest.runner.Framework",
    "zio-test" -> "zio.test.sbt.ZTestFramework",
    "weaver-cats" -> "weaver.framework.CatsEffect"
  )

  // Artifact names that indicate a test framework when found in the transitive dependency closure
  private val transitiveTestFrameworkArtifacts: Map[String, String] = Map(
    "junit-jupiter-api" -> "com.github.sbt.junit.JupiterFramework",
    "junit-jupiter-engine" -> "com.github.sbt.junit.JupiterFramework"
  )

  // Adapters needed by bleep's test runner (implements sbt test-interface)
  // These are injected automatically when the corresponding test framework is detected.
  // Scala test libs (scalatest, munit, etc.) already implement sbt test-interface.
  // JUnit Jupiter requires an external adapter; JUnit 4 needs junit-interface.
  private val testAdapterDeps: Map[String, model.Dep] = Map(
    "com.github.sbt.junit.JupiterFramework" -> model.Dep.Java("net.aichler", "jupiter-interface", "0.11.1"),
    "com.novocode.junit.JUnitFramework" -> model.Dep.Java("com.github.sbt", "junit-interface", "0.13.3")
  )

  // Repos that are default and shouldn't be included
  private val defaultRepoUrls: Set[String] = Set(
    "https://repo.maven.apache.org/maven2",
    "https://repo1.maven.org/maven2",
    "https://repo.maven.apache.org/maven2/"
  )

  def apply(
      logger: Logger,
      destinationPaths: BuildPaths,
      mavenProjects: List[MavenProject],
      bleepVersion: model.BleepVersion
  ): model.Build.Exploded = {

    // Build a set of reactor module GAVs for inter-module dependency detection
    val reactorModules: Map[(String, String), MavenProject] =
      mavenProjects.map(p => (p.groupId, p.artifactId) -> p).toMap

    val allProjects = mavenProjects.flatMap { mavenProject =>
      // Skip parent/pom-only modules that have no source directories
      if (mavenProject.packaging == "pom") {
        logger.info(s"Skipping pom-only module: ${mavenProject.artifactId}")
        Nil
      } else {
        convertModule(logger, destinationPaths, mavenProject, reactorModules)
      }
    }

    val buildResolvers = extractRepositories(mavenProjects)

    // Detect Java version from maven-compiler-plugin <release> across all modules
    val javaRelease = mavenProjects.flatMap(detectJavaRelease).maxOption
    val jvm = javaRelease.map(v => model.Jvm(s"temurin:$v", None))

    model.Build.Exploded(
      bleepVersion.latestRelease,
      explodedProjects = allProjects.toMap,
      resolvers = buildResolvers,
      jvm = jvm,
      scripts = Map.empty
    )
  }

  private def convertModule(
      logger: Logger,
      destinationPaths: BuildPaths,
      mavenProject: MavenProject,
      reactorModules: Map[(String, String), MavenProject]
  ): List[(model.CrossProjectName, model.Project)] = {

    val projectName = model.ProjectName(sanitizeProjectName(mavenProject.artifactId))
    val testProjectName = model.ProjectName(sanitizeProjectName(mavenProject.artifactId) + "-test")

    val scalaVersion = detectScalaVersion(mavenProject)

    val mainSourceLayout = inferSourceLayout(mavenProject, scalaVersion)
    val testSourceLayout = mainSourceLayout

    // Resolve symlinks to avoid path type mismatches (e.g. macOS /tmp -> /private/tmp)
    val buildDir =
      if (destinationPaths.buildDir.toFile.exists()) destinationPaths.buildDir.toRealPath()
      else destinationPaths.buildDir.toAbsolutePath.normalize()

    val folder: Option[RelPath] =
      RelPath.relativeTo(buildDir, mavenProject.directory) match {
        case RelPath(Array(projectName.value)) => None
        case relPath                           => Some(relPath)
      }

    val testFolder: Option[RelPath] =
      RelPath.relativeTo(buildDir, mavenProject.directory) match {
        case RelPath(Array(testProjectName.value)) => None
        case relPath                               => Some(relPath)
      }

    // Separate main vs test dependencies
    val (mainDeps, testDeps) = partitionDependencies(logger, mavenProject, reactorModules)

    // Inter-module dependsOn (main project)
    val mainDependsOn = detectInterModuleDeps(mavenProject, reactorModules, isTest = false)
    val testDependsOn = model.JsonSet(projectName) ++ detectInterModuleDeps(mavenProject, reactorModules, isTest = true)

    val configuredScala: Option[model.Scala] = scalaVersion.map { sv =>
      val compilerArgs = extractScalaCompilerArgs(mavenProject)
      model.Scala(
        version = Some(sv),
        options = model.Options.parse(compilerArgs, None),
        setup = None,
        compilerPlugins = model.JsonSet.empty,
        strict = None
      )
    }

    val configuredJava: Option[model.Java] = {
      val javaArgs = extractJavaCompilerArgs(mavenProject)
      if (javaArgs.isEmpty) None
      else Some(model.Java(options = model.Options.parse(javaArgs, None)))
    }

    val configuredKotlin: Option[model.Kotlin] = detectKotlinVersion(mavenProject).map { kv =>
      val kotlinArgs = extractKotlinCompilerArgs(mavenProject)
      val pluginOptionArgs = extractKotlinPluginOptions(mavenProject)
      val jvmTarget = extractKotlinJvmTarget(mavenProject)
      val plugins = extractKotlinCompilerPlugins(mavenProject)
      model.Kotlin(
        version = Some(kv),
        options = model.Options.parse(kotlinArgs ++ pluginOptionArgs, None),
        jvmTarget = jvmTarget,
        compilerPlugins = model.JsonSet.fromIterable(plugins),
        js = None,
        native = None
      )
    }

    val platform = model.Platform.Jvm(model.Options.empty, detectMainClass(logger, mavenProject), model.Options.empty)

    val testFrameworks = detectTestFrameworks(mavenProject, logger)

    // Inject test interface adapter deps needed by bleep's test runner
    val adapterDeps = testFrameworks.values.iterator
      .flatMap { fw =>
        testAdapterDeps.get(fw.value)
      }
      .toList
      .distinct
    val allTestDeps = testDeps ++ adapterDeps

    val testHasSources = hasSourceFiles(mavenProject.testSourceDirectory)

    // Additional source dirs (from build-helper-maven-plugin) that are NOT under target/
    // Dirs under target/ are generated sources handled separately via sourcegen.
    val targetDir = mavenProject.directory.resolve("target")
    val extraMainSources = mavenProject.additionalSources
      .filter(p => Files.isDirectory(p) && !p.startsWith(targetDir))
      .map(p => RelPath.relativeTo(mavenProject.directory, p))
    val extraTestSources = mavenProject.additionalTestSources
      .filter(p => Files.isDirectory(p) && !p.startsWith(targetDir))
      .map(p => RelPath.relativeTo(mavenProject.directory, p))

    // If Maven's sourceDirectory is non-standard (doesn't match inferred source layout), add it explicitly.
    // E.g. connector-openapi uses <sourceDirectory>src/main/generated-kotlin</sourceDirectory>
    val layoutMainDirs = mainSourceLayout.sources(scalaVersion, None, "main").values.map(mavenProject.directory / _).toSet
    val customMainSource =
      if (
        Files.isDirectory(mavenProject.sourceDirectory) && !layoutMainDirs
          .contains(mavenProject.sourceDirectory) && !mavenProject.sourceDirectory.startsWith(targetDir)
      )
        List(RelPath.relativeTo(mavenProject.directory, mavenProject.sourceDirectory))
      else Nil

    val layoutTestDirs = testSourceLayout.sources(scalaVersion, None, "test").values.map(mavenProject.directory / _).toSet
    val customTestSource =
      if (
        Files.isDirectory(mavenProject.testSourceDirectory) && !layoutTestDirs.contains(mavenProject.testSourceDirectory) && !mavenProject.testSourceDirectory
          .startsWith(targetDir)
      )
        List(RelPath.relativeTo(mavenProject.directory, mavenProject.testSourceDirectory))
      else Nil

    val allExtraMainSources = extraMainSources ++ customMainSource
    val allExtraTestSources = extraTestSources ++ customTestSource

    val result = List.newBuilder[(model.CrossProjectName, model.Project)]

    // Main project - always create it (test project depends on it)
    val mainCrossName = model.CrossProjectName(projectName, None)
    val mainProject = model.Project(
      `extends` = model.JsonSet.empty[model.TemplateId],
      cross = model.JsonMap.empty,
      folder = folder,
      dependsOn = mainDependsOn,
      `source-layout` = Some(mainSourceLayout),
      `sbt-scope` = Some("main"),
      sources = model.JsonSet.fromIterable(allExtraMainSources),
      resources = model.JsonSet.empty[RelPath],
      dependencies = model.JsonSet.fromIterable(mainDeps),
      java = configuredJava,
      scala = configuredScala,
      kotlin = configuredKotlin,
      platform = Some(platform),
      isTestProject = None,
      testFrameworks = model.JsonSet.empty[model.TestFrameworkName],
      sourcegen = model.JsonSet.empty[model.ScriptDef],
      libraryVersionSchemes = model.JsonSet.empty[model.LibraryVersionScheme],
      ignoreEvictionErrors = None,
      publish = None
    )
    result += (mainCrossName -> mainProject)

    // Test project - only if test sources exist
    if (testHasSources) {
      val testCrossName = model.CrossProjectName(testProjectName, None)

      // Extract surefire argLine: JVM options and javaagent coordinates
      val (surefireJvmArgs, surefireAgents) = extractSurefireConfig(mavenProject)
      val testJvmOptions = if (surefireJvmArgs.nonEmpty) model.Options.parse(surefireJvmArgs, None) else model.Options.empty

      val testPlatform = model.Platform
        .Jvm(testJvmOptions, None, model.Options.empty)
        .copy(
          jvmAgents = model.JsonSet.fromIterable(surefireAgents)
        )

      val testProject = model.Project(
        `extends` = model.JsonSet.empty[model.TemplateId],
        cross = model.JsonMap.empty,
        folder = testFolder,
        dependsOn = testDependsOn,
        `source-layout` = Some(testSourceLayout),
        `sbt-scope` = Some("test"),
        sources = model.JsonSet.fromIterable(allExtraTestSources),
        resources = model.JsonSet.empty[RelPath],
        dependencies = model.JsonSet.fromIterable(allTestDeps),
        java = configuredJava,
        scala = configuredScala,
        kotlin = configuredKotlin,
        platform = Some(testPlatform),
        isTestProject = Some(true),
        testFrameworks = testFrameworks,
        sourcegen = model.JsonSet.empty[model.ScriptDef],
        libraryVersionSchemes = model.JsonSet.empty[model.LibraryVersionScheme],
        ignoreEvictionErrors = None,
        publish = None
      )
      result += (testCrossName -> testProject)
    }

    result.result()
  }

  private def detectScalaVersion(mavenProject: MavenProject): Option[model.VersionScala] = {
    // Check for scala-library dependency
    val fromDeps = mavenProject.dependencies.collectFirst {
      case dep if dep.groupId == "org.scala-lang" && dep.artifactId == "scala-library" && dep.version.nonEmpty =>
        model.VersionScala(dep.version)
      case dep if dep.groupId == "org.scala-lang" && dep.artifactId == "scala3-library_3" && dep.version.nonEmpty =>
        model.VersionScala(dep.version)
    }

    // Also check for scala-maven-plugin version
    val fromPlugin = mavenProject.plugins.collectFirst {
      case plugin if plugin.artifactId == "scala-maven-plugin" || plugin.artifactId == "scala3-maven-plugin" =>
        val scalaVersion = (plugin.configuration \ "scalaVersion").headOption.map(_.text.trim)
        scalaVersion.filter(_.nonEmpty).map(model.VersionScala.apply)
    }.flatten

    fromDeps.orElse(fromPlugin)
  }

  private def detectKotlinVersion(mavenProject: MavenProject): Option[model.VersionKotlin] =
    // Primary: get version from kotlin-maven-plugin (effective POM has all interpolation resolved)
    mavenProject.plugins
      .collectFirst {
        case plugin if plugin.artifactId == "kotlin-maven-plugin" && plugin.groupId == "org.jetbrains.kotlin" && plugin.version.nonEmpty =>
          model.VersionKotlin(plugin.version)
      }
      .orElse {
        // Fallback: check for kotlin-stdlib or kotlin-stdlib-jdk8 in dependencies
        val kotlinArtifacts = Set("kotlin-stdlib", "kotlin-stdlib-jdk8", "kotlin-stdlib-jdk7")
        mavenProject.dependencies.collectFirst {
          case dep if dep.groupId == "org.jetbrains.kotlin" && kotlinArtifacts.contains(dep.artifactId) && dep.version.nonEmpty =>
            model.VersionKotlin(dep.version)
        }
      }

  private def extractKotlinCompilerArgs(mavenProject: MavenProject): List[String] =
    mavenProject.plugins.flatMap {
      case plugin if plugin.artifactId == "kotlin-maven-plugin" =>
        val args = plugin.configuration \ "args" \ "arg"
        args.map(_.text.trim).toList
      case _ => Nil
    }

  private def extractKotlinJvmTarget(mavenProject: MavenProject): Option[String] =
    mavenProject.plugins.collectFirst {
      case plugin if plugin.artifactId == "kotlin-maven-plugin" =>
        (plugin.configuration \ "jvmTarget").headOption.map(_.text.trim)
    }.flatten

  /** Extract Kotlin compiler plugin IDs from kotlin-maven-plugin configuration.
    *
    * Maven POM format:
    * {{{
    * <configuration>
    *   <compilerPlugins>
    *     <plugin>spring</plugin>
    *     <plugin>jpa</plugin>
    *   </compilerPlugins>
    * </configuration>
    * }}}
    */
  private def extractKotlinCompilerPlugins(mavenProject: MavenProject): List[String] =
    mavenProject.plugins.flatMap {
      case plugin if plugin.artifactId == "kotlin-maven-plugin" =>
        val plugins = plugin.configuration \ "compilerPlugins" \ "plugin"
        plugins.map(_.text.trim).toList
      case _ => Nil
    }

  /** Extract Kotlin plugin options from kotlin-maven-plugin configuration.
    *
    * Maven POM format:
    * {{{
    * <configuration>
    *   <pluginOptions>
    *     <option>all-open:annotation=jakarta.ws.rs.Path</option>
    *   </pluginOptions>
    * </configuration>
    * }}}
    *
    * These map to `-P plugin:<pluginId>:<key>=<value>` kotlinc flags. The plugin ID mappings:
    *   - `all-open:` -> `plugin:org.jetbrains.kotlin.allopen:`
    *   - `no-arg:` -> `plugin:org.jetbrains.kotlin.noarg:`
    *   - `sam-with-receiver:` -> `plugin:org.jetbrains.kotlin.samWithReceiver:`
    */
  private def extractKotlinPluginOptions(mavenProject: MavenProject): List[String] = {
    val pluginIdToFqn = Map(
      "all-open" -> "org.jetbrains.kotlin.allopen",
      "no-arg" -> "org.jetbrains.kotlin.noarg",
      "sam-with-receiver" -> "org.jetbrains.kotlin.samWithReceiver"
    )

    mavenProject.plugins.flatMap {
      case plugin if plugin.artifactId == "kotlin-maven-plugin" =>
        val options = plugin.configuration \ "pluginOptions" \ "option"
        options.flatMap { opt =>
          val text = opt.text.trim
          // Format: "pluginShortName:key=value" → "-P plugin:fqn:key=value"
          val colonIdx = text.indexOf(':')
          if (colonIdx > 0) {
            val shortName = text.substring(0, colonIdx)
            val rest = text.substring(colonIdx + 1)
            pluginIdToFqn.get(shortName).map { fqn =>
              s"-P plugin:$fqn:$rest"
            }
          } else None
        }.toList
      case _ => Nil
    }
  }

  /** Extract surefire/failsafe configuration for test execution.
    *
    * Returns (jvmOptions, jvmAgents):
    *   - jvmOptions: plain JVM flags like `-XX:+EnableDynamicAgentLoading`
    *   - jvmAgents: Maven coordinates extracted from `-javaagent:` references
    *
    * `-javaagent:${settings.localRepository}/org/group/artifact/version/artifact-version.jar` is parsed back into the Maven coordinate
    * `org.group:artifact:version`.
    */
  private def extractSurefireConfig(mavenProject: MavenProject): (List[String], List[model.Dep]) = {
    val surefirePlugins = Set("maven-surefire-plugin", "maven-failsafe-plugin")
    // Pattern: -javaagent:<repo-path>/org/mockito/mockito-core/5.20.0/mockito-core-5.20.0.jar
    val agentPattern = """-javaagent:.*?/([^/]+(?:/[^/]+)*)/([^/]+)/([^/]+)/\2-\3\.jar""".r

    val jvmOptions = List.newBuilder[String]
    val jvmAgents = List.newBuilder[model.Dep]

    mavenProject.plugins
      .filter(p => surefirePlugins.contains(p.artifactId))
      .foreach { plugin =>
        // Extract argLine
        val argLine = (plugin.configuration \ "argLine").headOption.map(_.text.trim).getOrElse("")
        argLine.split("\\s+").filter(_.nonEmpty).foreach {
          case arg @ agentPattern(groupPath, artifactId, version) =>
            val groupId = groupPath.replace('/', '.')
            jvmAgents += model.Dep.Java(groupId, artifactId, version)
          case arg if arg.startsWith("-javaagent:") =>
            () // skip unrecognizable agent paths (e.g. with unresolvable Maven properties)
          case arg if !arg.contains("${") =>
            jvmOptions += arg
          case _ =>
            () // skip args with unresolved Maven properties
        }

        // Extract systemPropertyVariables as -D flags
        val sysPropVars = plugin.configuration \ "systemPropertyVariables"
        val mavenSpecificProps = Set("maven.home", "maven.repo.local", "basedir", "project.build.directory")
        sysPropVars.foreach { parent =>
          parent.child.collect { case elem: scala.xml.Elem => elem }.foreach { elem =>
            val key = elem.label
            val value = elem.text.trim
            if (value.nonEmpty && !value.contains("${") && !mavenSpecificProps.contains(key)) {
              jvmOptions += s"-D$key=$value"
            }
          }
        }
      }

    (jvmOptions.result(), jvmAgents.result().distinct)
  }

  /** Infer source layout from project configuration (plugins, dependencies), not directory existence.
    *
    * A project with kotlin-maven-plugin is Kotlin regardless of whether src/main/kotlin exists on disk.
    */
  private def inferSourceLayout(
      mavenProject: MavenProject,
      scalaVersion: Option[model.VersionScala]
  ): model.SourceLayout = {
    val hasKotlinPlugin = mavenProject.plugins.exists(_.artifactId == "kotlin-maven-plugin")
    val hasKotlinDep = mavenProject.dependencies.exists(d => d.groupId == "org.jetbrains.kotlin" && d.artifactId.startsWith("kotlin-stdlib"))

    val hasScalaPlugin = mavenProject.plugins.exists(p => p.artifactId == "scala-maven-plugin" || p.artifactId == "scala3-maven-plugin")

    if (hasKotlinPlugin || hasKotlinDep) model.SourceLayout.Kotlin
    else if (hasScalaPlugin || scalaVersion.isDefined) model.SourceLayout.Normal
    else model.SourceLayout.Java
  }

  /** Partition Maven dependencies into main and test deps, converting to bleep model.Dep */
  private def partitionDependencies(
      logger: Logger,
      mavenProject: MavenProject,
      reactorModules: Map[(String, String), MavenProject]
  ): (List[model.Dep], List[model.Dep]) = {
    val mainDeps = List.newBuilder[model.Dep]
    val testDeps = List.newBuilder[model.Dep]

    mavenProject.dependencies.foreach { dep =>
      // Skip inter-module dependencies (handled via dependsOn)
      if (reactorModules.contains((dep.groupId, dep.artifactId))) {
        // skip - handled as dependsOn
      }
      // Skip provided Scala artifacts
      else if (isProvidedScalaArtifact(dep)) {
        // skip - bleep provides these
      } else {
        convertDependency(logger, dep) match {
          case Some(bleepDep) =>
            dep.scope match {
              case "test" =>
                testDeps += bleepDep
              case "provided" =>
                mainDeps += bleepDep.withConfiguration(Configuration.provided)
              case _ =>
                // compile, runtime, system -> main deps
                mainDeps += bleepDep
            }
          case None =>
            // Already logged in convertDependency
            ()
        }
      }
    }

    (mainDeps.result(), testDeps.result())
  }

  private def isProvidedScalaArtifact(dep: MavenDependency): Boolean = {
    val stripped = stripScalaSuffix(dep.artifactId)._1
    dep.groupId == "org.scala-lang" && providedScalaArtifacts.contains(stripped)
  }

  /** Convert a MavenDependency to a bleep model.Dep */
  private def convertDependency(
      logger: Logger,
      dep: MavenDependency
  ): Option[model.Dep] = {
    if (dep.version.isEmpty) {
      logger.warn(s"Skipping dependency with no version: ${dep.groupId}:${dep.artifactId}")
      return None
    }

    val exclusions = convertExclusions(dep.exclusions)

    val (baseName, crossInfo) = stripScalaSuffix(dep.artifactId)

    val result = crossInfo match {
      case Some(CrossInfo(fullCrossVersion)) =>
        model.Dep.ScalaDependency(
          organization = Organization(dep.groupId),
          baseModuleName = ModuleName(baseName),
          version = dep.version,
          fullCrossVersion = fullCrossVersion,
          exclusions = exclusions
        )
      case None =>
        model.Dep.JavaDependency(
          organization = Organization(dep.groupId),
          moduleName = ModuleName(dep.artifactId),
          version = dep.version,
          exclusions = exclusions
        )
    }

    Some(result)
  }

  private case class CrossInfo(fullCrossVersion: Boolean)

  /** Strip Scala version suffix from artifact name to detect cross-built Scala dependencies.
    *
    * Returns (baseName, Some(CrossInfo)) for Scala deps, (originalName, None) for Java deps.
    */
  private def stripScalaSuffix(artifactId: String): (String, Option[CrossInfo]) = {
    // Full cross version: _2.13.12, _3.3.3
    val fullCrossPattern = """^(.+)_(\d+\.\d+\.\d+)$""".r
    // Binary cross Scala 2.x: _2.13, _2.12
    val binaryCross2Pattern = """^(.+)_(\d+\.\d+)$""".r
    // Binary cross Scala 3: _3
    val binaryCross3Pattern = """^(.+)_(\d+)$""".r

    artifactId match {
      case fullCrossPattern(base, _) =>
        (base, Some(CrossInfo(fullCrossVersion = true)))
      case binaryCross2Pattern(base, _) =>
        (base, Some(CrossInfo(fullCrossVersion = false)))
      case binaryCross3Pattern(base, _) =>
        (base, Some(CrossInfo(fullCrossVersion = false)))
      case _ =>
        (artifactId, None)
    }
  }

  private def convertExclusions(exclusions: List[MavenExclusion]): model.JsonMap[Organization, model.JsonSet[ModuleName]] =
    if (exclusions.isEmpty) model.JsonMap.empty
    else {
      model.JsonMap {
        exclusions
          .groupBy(e => Organization(e.groupId))
          .map { case (org, excs) => (org, model.JsonSet.fromIterable(excs.map(e => ModuleName(e.artifactId)))) }
      }
    }

  private def detectInterModuleDeps(
      mavenProject: MavenProject,
      reactorModules: Map[(String, String), MavenProject],
      isTest: Boolean
  ): model.JsonSet[model.ProjectName] = {
    val deps = mavenProject.dependencies.flatMap { dep =>
      reactorModules.get((dep.groupId, dep.artifactId)) match {
        case Some(_) =>
          val depScope = dep.scope
          // For main project: only compile/runtime/provided scope inter-module deps
          // For test project: also include test scope inter-module deps
          if (isTest || (depScope != "test")) {
            Some(model.ProjectName(sanitizeProjectName(dep.artifactId)))
          } else None
        case None => None
      }
    }
    model.JsonSet.fromIterable(deps)
  }

  private def extractScalaCompilerArgs(mavenProject: MavenProject): List[String] = {
    val fromScalaMaven = mavenProject.plugins.flatMap {
      case plugin if plugin.artifactId == "scala-maven-plugin" || plugin.artifactId == "scala3-maven-plugin" =>
        val args = plugin.configuration \ "args" \ "arg"
        args.map(_.text.trim).toList
      case _ => Nil
    }
    fromScalaMaven
  }

  /** Detect the Java release version from maven-compiler-plugin configuration. */
  private def detectJavaRelease(mavenProject: MavenProject): Option[Int] =
    mavenProject.plugins.collectFirst {
      case plugin if plugin.artifactId == "maven-compiler-plugin" =>
        (plugin.configuration \ "release").headOption
          .map(_.text.trim.toInt)
          .orElse((plugin.configuration \ "target").headOption.map(_.text.trim.toInt))
    }.flatten

  private def extractJavaCompilerArgs(mavenProject: MavenProject): List[String] =
    mavenProject.plugins.flatMap {
      case plugin if plugin.artifactId == "maven-compiler-plugin" =>
        val compilerArgs = plugin.configuration \ "compilerArgs" \ "arg"
        val source = (plugin.configuration \ "source").headOption.map(n => List("-source", n.text.trim)).getOrElse(Nil)
        val target = (plugin.configuration \ "target").headOption.map(n => List("-target", n.text.trim)).getOrElse(Nil)
        val release = (plugin.configuration \ "release").headOption.map(n => List("--release", n.text.trim)).getOrElse(Nil)
        // prefer release over source/target
        if (release.nonEmpty) release ++ compilerArgs.map(_.text.trim).toList
        else source ++ target ++ compilerArgs.map(_.text.trim).toList
      case _ => Nil
    }

  private def detectMainClass(logger: Logger, mavenProject: MavenProject): Option[String] = {
    val raw = mavenProject.plugins.collectFirst {
      case plugin if plugin.artifactId == "maven-jar-plugin" =>
        (plugin.configuration \ "archive" \ "manifest" \ "mainClass").headOption.map(_.text.trim)
      case plugin if plugin.artifactId == "exec-maven-plugin" =>
        (plugin.configuration \ "mainClass").headOption.map(_.text.trim)
    }.flatten

    raw match {
      case Some(value) if value.contains("${") =>
        logger.warn(s"Skipping unresolved mainClass '$value' for ${mavenProject.artifactId} — set it manually in bleep.yaml")
        None
      case other => other
    }
  }

  private def detectTestFrameworks(mavenProject: MavenProject, logger: Logger): model.JsonSet[model.TestFrameworkName] = {
    // Phase 1: detect from direct test dependencies
    val fromDeps = mavenProject.dependencies.flatMap { dep =>
      if (dep.scope == "test") {
        val stripped = stripScalaSuffix(dep.artifactId)._1
        testFrameworksByArtifact.get(stripped).map(model.TestFrameworkName.apply)
      } else None
    }.distinct

    // Phase 2: resolve transitive test dependencies to detect frameworks not directly listed
    val fromTransitives = detectTestFrameworksFromTransitives(mavenProject, logger)

    val frameworks = (fromDeps ++ fromTransitives).distinct

    // When both JUnit 4 and JUnit 5 are detected, prefer JUnit 5 (Jupiter)
    val jupiterFramework = model.TestFrameworkName("com.github.sbt.junit.JupiterFramework")
    val junit4Framework = model.TestFrameworkName("com.novocode.junit.JUnitFramework")
    val resolved =
      if (frameworks.contains(jupiterFramework) && frameworks.contains(junit4Framework))
        frameworks.filter(_ != junit4Framework)
      else frameworks

    model.JsonSet.fromIterable(resolved)
  }

  /** Resolve test dependencies transitively and check for known test framework artifacts. */
  private def detectTestFrameworksFromTransitives(mavenProject: MavenProject, logger: Logger): List[model.TestFrameworkName] = {
    val testDeps = mavenProject.dependencies.filter(_.scope == "test")
    if (testDeps.isEmpty) return Nil

    val coursierDeps = testDeps.map { dep =>
      coursier.Dependency(
        coursier.Module(Organization(dep.groupId), ModuleName(dep.artifactId)),
        dep.version
      )
    }

    val repos = CoursierResolver.coursierRepos(
      mavenProject.repositories.map(r => model.Repository.Maven(Some(r.id).filter(_.nonEmpty).map(model.ResolverName.apply), URI.create(r.url))),
      None,
      new CredentialProvider(logger, None),
      logger
    )

    val resolution =
      try coursier.Resolve().addDependencies(coursierDeps*).withRepositories(repos).run()
      catch { case _: Exception => return Nil }

    val resolvedModuleNames = resolution.dependencies.map(_.module.name.value)

    resolvedModuleNames.flatMap(transitiveTestFrameworkArtifacts.get).map(model.TestFrameworkName.apply).toList
  }

  private def extractRepositories(mavenProjects: List[MavenProject]): model.JsonList[model.Repository] = {
    val repos = mavenProjects
      .flatMap(_.repositories)
      .distinctBy(_.url)
      .filterNot(repo => defaultRepoUrls.contains(repo.url) || defaultRepoUrls.contains(repo.url + "/"))
      .filterNot(_.id == "central")
      .map { repo =>
        model.Repository.Maven(Some(repo.id).filter(_.nonEmpty).map(model.ResolverName.apply), URI.create(repo.url)): model.Repository
      }
    model.JsonList(repos)
  }

  private def hasSourceFiles(dir: Path): Boolean =
    Files.isDirectory(dir) && {
      val stream = Files.walk(dir)
      try
        stream.anyMatch { p =>
          val name = p.getFileName.toString
          name.endsWith(".scala") || name.endsWith(".java") || name.endsWith(".kt")
        }
      finally stream.close()
    }

  /** Discover generated source files under target/generated-sources/ for each Maven module.
    *
    * Maven code generators (openapi, wsdl2java, avro, jaxb, etc.) all output to `target/generated-sources/<generator-name>/`. We walk these directories after
    * `mvn compile` has populated them, read the file contents, and return them keyed by bleep project name.
    *
    * Generated test sources under `target/generated-test-sources/` are associated with the corresponding `-test` project.
    */
  def discoverGeneratedFiles(
      logger: Logger,
      mavenProjects: List[MavenProject]
  ): Map[model.CrossProjectName, Vector[internal.GeneratedFile]] = {
    val result = Map.newBuilder[model.CrossProjectName, Vector[internal.GeneratedFile]]

    mavenProjects.foreach { mavenProject =>
      if (mavenProject.packaging == "pom") ()
      else {
        val projectName = model.ProjectName(sanitizeProjectName(mavenProject.artifactId))
        val testProjectName = model.ProjectName(sanitizeProjectName(mavenProject.artifactId) + "-test")

        val mainGenDir = mavenProject.directory.resolve("target/generated-sources")
        val testGenDir = mavenProject.directory.resolve("target/generated-test-sources")

        val mainFiles = collectGeneratedFiles(logger, mainGenDir, isResource = false)
        val testFiles = collectGeneratedFiles(logger, testGenDir, isResource = false)

        if (mainFiles.nonEmpty) {
          val crossName = model.CrossProjectName(projectName, None)
          result += (crossName -> mainFiles)
        }
        if (testFiles.nonEmpty) {
          val crossName = model.CrossProjectName(testProjectName, None)
          result += (crossName -> testFiles)
        }
      }
    }

    result.result()
  }

  private def collectGeneratedFiles(logger: Logger, parentDir: Path, isResource: Boolean): Vector[internal.GeneratedFile] =
    if (!Files.isDirectory(parentDir)) Vector.empty
    else {
      import scala.jdk.CollectionConverters.*

      val subDirs = Files.list(parentDir)
      try
        subDirs
          .iterator()
          .asScala
          .filter(Files.isDirectory(_))
          // Skip "annotations" dirs — these are annotation processor outputs (empty marker dirs)
          .filter(dir => dir.getFileName.toString != "annotations")
          .flatMap { genDir =>
            val fileStream = Files.walk(genDir)
            try
              fileStream
                .iterator()
                .asScala
                .filter(Files.isRegularFile(_))
                .filter { p =>
                  val name = p.getFileName.toString
                  name.endsWith(".scala") || name.endsWith(".java") || name.endsWith(".kt")
                }
                .flatMap { file =>
                  val content =
                    try Some(Files.readString(file))
                    catch {
                      case e: Exception =>
                        logger.warn(s"Failed to read generated file $file: $e")
                        None
                    }
                  content.map(c => internal.GeneratedFile(isResource, c, RelPath.relativeTo(genDir, file)))
                }
                .toVector
            finally fileStream.close()
          }
          .toVector
      finally subDirs.close()
    }

  private def sanitizeProjectName(artifactId: String): String =
    artifactId.replace('.', '-')
}
