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

    model.Build.Exploded(
      bleepVersion.latestRelease,
      explodedProjects = allProjects.toMap,
      resolvers = buildResolvers,
      jvm = None,
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

    val mainSourceLayout = inferSourceLayout(mavenProject.directory, "main", scalaVersion)
    val testSourceLayout = inferSourceLayout(mavenProject.directory, "test", scalaVersion)

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
      val jvmTarget = extractKotlinJvmTarget(mavenProject)
      val plugins = extractKotlinCompilerPlugins(mavenProject)
      model.Kotlin(
        version = Some(kv),
        options = model.Options.parse(kotlinArgs, None),
        jvmTarget = jvmTarget,
        compilerPlugins = model.JsonSet.fromIterable(plugins),
        js = None,
        native = None
      )
    }

    val platform = model.Platform.Jvm(model.Options.empty, detectMainClass(logger, mavenProject), model.Options.empty)

    val testFrameworks = detectTestFrameworks(mavenProject)

    // Inject test interface adapter deps needed by bleep's test runner
    val adapterDeps = testFrameworks.values.iterator.flatMap { fw =>
      testAdapterDeps.get(fw.value)
    }.toList.distinct
    val allTestDeps = testDeps ++ adapterDeps

    val testHasSources = hasSourceFiles(mavenProject.testSourceDirectory)

    val result = List.newBuilder[(model.CrossProjectName, model.Project)]

    // Main project - always create it (test project depends on it)
    val mainCrossName = model.CrossProjectName(projectName, None)
    val mainProject = model.Project(
      `extends` = model.JsonSet.empty,
      cross = model.JsonMap.empty,
      folder = folder,
      dependsOn = mainDependsOn,
      `source-layout` = Some(mainSourceLayout),
      `sbt-scope` = Some("main"),
      sources = model.JsonSet.empty,
      resources = model.JsonSet.empty,
      dependencies = model.JsonSet.fromIterable(mainDeps),
      java = configuredJava,
      scala = configuredScala,
      kotlin = configuredKotlin,
      platform = Some(platform),
      isTestProject = None,
      testFrameworks = model.JsonSet.empty,
      sourcegen = model.JsonSet.empty,
      libraryVersionSchemes = model.JsonSet.empty,
      ignoreEvictionErrors = None
    )
    result += (mainCrossName -> mainProject)

    // Test project - only if test sources exist
    if (testHasSources) {
      val testCrossName = model.CrossProjectName(testProjectName, None)
      val testProject = model.Project(
        `extends` = model.JsonSet.empty,
        cross = model.JsonMap.empty,
        folder = testFolder,
        dependsOn = testDependsOn,
        `source-layout` = Some(testSourceLayout),
        `sbt-scope` = Some("test"),
        sources = model.JsonSet.empty,
        resources = model.JsonSet.empty,
        dependencies = model.JsonSet.fromIterable(allTestDeps),
        java = configuredJava,
        scala = configuredScala,
        kotlin = configuredKotlin,
        platform = Some(platform),
        isTestProject = Some(true),
        testFrameworks = testFrameworks,
        sourcegen = model.JsonSet.empty,
        libraryVersionSchemes = model.JsonSet.empty,
        ignoreEvictionErrors = None
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
    mavenProject.plugins.collectFirst {
      case plugin if plugin.artifactId == "kotlin-maven-plugin" && plugin.groupId == "org.jetbrains.kotlin" && plugin.version.nonEmpty =>
        model.VersionKotlin(plugin.version)
    }.orElse {
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

  private def inferSourceLayout(
      projectDir: Path,
      scope: String,
      scalaVersion: Option[model.VersionScala]
  ): model.SourceLayout = {
    val hasScala = Files.isDirectory(projectDir.resolve(s"src/$scope/scala"))
    val hasKotlin = Files.isDirectory(projectDir.resolve(s"src/$scope/kotlin"))

    if (hasKotlin) model.SourceLayout.Kotlin
    else if (hasScala || scalaVersion.isDefined) model.SourceLayout.Normal
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
      }
      else {
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

  private def detectTestFrameworks(mavenProject: MavenProject): model.JsonSet[model.TestFrameworkName] = {
    // Phase 1: detect from direct test dependencies
    val fromDeps = mavenProject.dependencies.flatMap { dep =>
      if (dep.scope == "test") {
        val stripped = stripScalaSuffix(dep.artifactId)._1
        testFrameworksByArtifact.get(stripped).map(model.TestFrameworkName.apply)
      } else None
    }.distinct

    // Phase 2: resolve transitive test dependencies to detect frameworks not directly listed
    val fromTransitives = detectTestFrameworksFromTransitives(mavenProject)

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
  private def detectTestFrameworksFromTransitives(mavenProject: MavenProject): List[model.TestFrameworkName] = {
    val testDeps = mavenProject.dependencies.filter(_.scope == "test")
    if (testDeps.isEmpty) return Nil

    val coursierDeps = testDeps.map { dep =>
      coursier.Dependency(
        coursier.Module(Organization(dep.groupId), ModuleName(dep.artifactId)),
        dep.version
      )
    }

    val repos = CoursierResolver.coursierRepos(
      mavenProject.repositories.map(r => model.Repository.Maven(Some(r.id).filter(_.nonEmpty), URI.create(r.url))),
      None
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
      .map(repo => model.Repository.Maven(Some(repo.id).filter(_.nonEmpty), URI.create(repo.url)): model.Repository)
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

  private def sanitizeProjectName(artifactId: String): String =
    artifactId.replace('.', '-')
}
