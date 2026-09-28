package bleep
package mavenimport

import coursier.core.{Classifier, Configuration, Extension, ModuleName, Organization, Publication, Type}
import ryddig.Logger

import java.net.URI
import java.nio.file.Path

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

  /** Well-known `sbt.testing.Framework` implementations, by the artifact that provides them.
    *
    * No junit here, deliberately. junit has no `Framework` of its own — the sbt adapters are third-party bridges, and bleep does not use them: it drives the
    * JUnit Platform's `Launcher` directly and finds junit suites by scanning for their annotations. Naming an adapter class in `testFrameworks:` would point
    * bleep at a bridge it never loads, and pull in a dependency the build does not need.
    */
  private val testFrameworksByArtifact: Map[String, String] = Map(
    "scalatest" -> "org.scalatest.tools.Framework",
    "specs2-core" -> "org.specs2.runner.Specs2Framework",
    "munit" -> "munit.Framework",
    "utest" -> "utest.runner.Framework",
    "zio-test" -> "zio.test.sbt.ZTestFramework",
    "weaver-cats" -> "weaver.framework.CatsEffect"
  )

  // Repos that are default and shouldn't be included
  private val defaultRepoUrls: Set[String] = Set(
    "https://repo.maven.apache.org/maven2",
    "https://repo1.maven.org/maven2",
    "https://repo.maven.apache.org/maven2/"
  )

  def apply(
      logger: Logger,
      fs: MavenFs,
      destinationPaths: BuildPaths,
      mavenProjects: List[MavenProject],
      /** what `mvn dependency:list` printed, see [[runMaven]] */
      dependencyList: Path,
      bleepVersion: model.BleepVersion,
      /** `--build-jvm`, see [[internal.importJvm]] */
      buildJvm: Option[Int]
  ): model.Build.Exploded = {

    // Build a set of reactor module GAVs for inter-module dependency detection
    val reactorModules: Map[(String, String), MavenProject] =
      mavenProjects.map(p => (p.groupId, p.artifactId) -> p).toMap

    // only read when the build manages versions itself
    lazy val resolved = parseDependencyList(fs.readString(dependencyList))
    val management = new Management(fs, mavenProjects, () => resolved)

    val allProjects = mavenProjects.flatMap { mavenProject =>
      // parents, aggregators and BOM modules. what they manage reaches the modules through `Management`
      if (mavenProject.packaging == "pom") {
        logger.info(s"Skipping pom-only module: ${mavenProject.artifactId}")
        Nil
      } else {
        convertModule(logger, fs, destinationPaths, mavenProject, reactorModules, management)
      }
    }

    val buildResolvers = extractRepositories(mavenProjects)

    // the newest java version any module compiles its code or its tests for
    val javaRelease = mavenProjects.flatMap(m => List(JavaCompile.main(m), JavaCompile.test(m))).flatMap(_.javaVersion).maxOption
    val jvm = Some(internal.importJvm(buildJvm, javaRelease))

    model.Build.Exploded(
      bleepVersion.latestRelease,
      explodedProjects = allProjects.toMap,
      resolvers = buildResolvers,
      jvm = jvm,
      scripts = Map.empty,
      remoteCache = None
    )
  }

  private def publishAs(mavenProject: MavenProject): model.PublishConfig =
    model.PublishConfig(
      enabled = None,
      groupId = Some(mavenProject.groupId),
      description = None,
      url = None,
      organization = None,
      developers = model.JsonSet.empty,
      licenses = model.JsonSet.empty,
      sonatypeProfileName = None,
      sonatypeCredentialHost = None
    )

  private def folderOf(destinationPaths: BuildPaths, mavenProject: MavenProject, projectName: model.ProjectName): Option[RelPath] = {
    // Resolve symlinks to avoid path type mismatches (e.g. macOS /tmp -> /private/tmp)
    val buildDir =
      if (destinationPaths.buildDir.toFile.exists()) destinationPaths.buildDir.toRealPath()
      else destinationPaths.buildDir.toAbsolutePath.normalize()
    RelPath.relativeTo(buildDir, mavenProject.directory) match {
      case RelPath(Array(projectName.value)) => None
      case relPath                           => Some(relPath)
    }
  }

  private def convertModule(
      logger: Logger,
      fs: MavenFs,
      destinationPaths: BuildPaths,
      mavenProject: MavenProject,
      reactorModules: Map[(String, String), MavenProject],
      management: Management
  ): List[(model.CrossProjectName, model.Project)] = {

    val projectName = model.ProjectName(sanitizeProjectName(mavenProject.artifactId))
    val testProjectName = model.ProjectName(sanitizeProjectName(mavenProject.artifactId) + "-test")

    val scalaVersion = detectScalaVersion(mavenProject)

    val mainSourceLayout = inferSourceLayout(mavenProject, scalaVersion)
    val testSourceLayout = mainSourceLayout

    val folder: Option[RelPath] = folderOf(destinationPaths, mavenProject, projectName)
    val testFolder: Option[RelPath] = folderOf(destinationPaths, mavenProject, testProjectName)

    // BOMs the module imports, and the coordinates whose version those (or any dependencyManagement) supply — a `<dependency>` written without a `<version>`
    // in the raw pom. `mvn help:effective-pom` fills those versions in, but the author wrote them BOM-managed; with a `boms:` block to supply the version, bleep
    // preserves that and emits the dependency version-less, so the imported build reads like the Maven one instead of freezing a version the BOM should own.
    val boms = management.boms(mavenProject)

    // Separate main vs test dependencies
    val (declaredMainDeps, declaredTestDeps) = partitionDependencies(logger, mavenProject, reactorModules)

    // what the build manages itself, where the module uses it
    val (usedManagedMain, usedManagedTest) = {
      val deps = management.usedManaged(mavenProject).map { r =>
        val exclusions = mavenProject.dependencyManagement.find(m => m.groupId == r.groupId && m.artifactId == r.artifactId).toList.flatMap(_.exclusions)
        val dep = convertDependency(logger, MavenDependency(r.groupId, r.artifactId, r.version, r.scope, optional = false, exclusions, r.tpe, r.classifier))
          .getOrElse(throw new BleepException.Text(s"${mavenProject.artifactId}: could not convert ${r.groupId}:${r.artifactId}:${r.version}"))
        (r.scope, dep)
      }
      (
        deps.collect {
          case ("provided", dep)                                      => dep.withConfiguration(Configuration.provided)
          case ("runtime", dep)                                       => dep.withConfiguration(Configuration.runtime)
          case (scope, dep) if scope != "test" && scope != "provided" => dep
        },
        deps.collect { case ("test", dep) => dep }
      )
    }
    val mainDeps = declaredMainDeps ++ usedManagedMain
    val testDeps = declaredTestDeps ++ usedManagedTest

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
        strict = None,
        skipStdlib = None,
        compilerProject = None
      )
    }

    val mainJava = JavaCompile.main(mavenProject).toJava
    val testJava = JavaCompile.test(mavenProject).toJava

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
        kspVersion = None,
        scanForSymbolProcessors = None,
        symbolProcessors = model.JsonSet.empty,
        symbolProcessorOptions = model.SymbolProcessorOptions.empty,
        js = None,
        native = None
      )
    }

    // Maven's working-directory semantics: surefire (and exec) run in `${basedir}`, the module
    // directory. bleep's global default emulates sbt instead (user.dir = build root), so imported
    // projects state maven's behavior explicitly; stating it also keeps the sbt-flavored default
    // from being added (see rewrites.Defaults).
    val mavenUserDir = model.Options(Set(model.Options.Opt.Flag(s"-Duser.dir=${model.Replacements.known.ProjectDir}")))

    val platform = model.Platform.Jvm(mavenUserDir, detectMainClass(logger, mavenProject), model.Options.empty)

    val testFrameworks = detectTestFrameworks(mavenProject)

    // No adapter injection. Scala frameworks implement sbt test-interface themselves, and junit needs no bridge because bleep runs the JUnit Platform
    // Launcher directly — whatever junit the pom already declares is what runs.
    //
    // In maven the tests are part of the module, and see its `provided` and `optional` dependencies, which the test project does not inherit
    val allTestDeps = testDeps ++ mainDeps.filter(ResolveProjects.providedOrOptional)

    val testHasSources = hasSourceFiles(fs, mavenProject.testSourceDirectory)

    // Additional source dirs (from build-helper-maven-plugin) that are NOT under target/
    // Dirs under target/ are generated sources handled separately via sourcegen.
    val targetDir = mavenProject.directory.resolve("target")
    val extraMainSources = mavenProject.additionalSources
      .filter(p => fs.isDirectory(p) && !p.startsWith(targetDir))
      .map(p => RelPath.relativeTo(mavenProject.directory, p))
    val extraTestSources = mavenProject.additionalTestSources
      .filter(p => fs.isDirectory(p) && !p.startsWith(targetDir))
      .map(p => RelPath.relativeTo(mavenProject.directory, p))

    // If Maven's sourceDirectory is non-standard (doesn't match inferred source layout), add it explicitly.
    // E.g. connector-openapi uses <sourceDirectory>src/main/generated-kotlin</sourceDirectory>
    val layoutMainDirs = mainSourceLayout.sources(scalaVersion, None, Set.empty, "main").values.map(mavenProject.directory / _).toSet
    val customMainSource =
      if (
        fs.isDirectory(mavenProject.sourceDirectory) && !layoutMainDirs
          .contains(mavenProject.sourceDirectory) && !mavenProject.sourceDirectory.startsWith(targetDir)
      )
        List(RelPath.relativeTo(mavenProject.directory, mavenProject.sourceDirectory))
      else Nil

    val layoutTestDirs = testSourceLayout.sources(scalaVersion, None, Set.empty, "test").values.map(mavenProject.directory / _).toSet
    val customTestSource =
      if (
        fs.isDirectory(mavenProject.testSourceDirectory) && !layoutTestDirs.contains(mavenProject.testSourceDirectory) && !mavenProject.testSourceDirectory
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
      dependsOn = mainDependsOn.map(model.ProjectRef(_)),
      `source-layout` = Some(mainSourceLayout),
      `sbt-scope` = Some("main"),
      sources = model.JsonSet.fromIterable(allExtraMainSources),
      resources = model.JsonSet.empty[RelPath],
      dependencies = model.JsonSet.fromIterable(mainDeps),
      boms = model.JsonSet.fromIterable(boms),
      jars = model.JsonSet.empty,
      java = mainJava,
      scala = configuredScala,
      kotlin = configuredKotlin,
      platform = Some(platform),
      isTestProject = None,
      testFrameworks = model.JsonSet.empty[model.TestFrameworkName],
      testTags = model.JsonMap.empty,
      testExclude = model.JsonSet.empty,
      maxConcurrentSuites = None,
      testFork = None,
      sourcegen = model.JsonSet.empty[model.ScriptDef],
      stamp = model.JsonSet.empty[model.StampKind],
      libraryVersionSchemes = model.JsonSet.empty[model.LibraryVersionScheme],
      ignoreEvictionErrors = None,
      // the module's coordinates. a library may depend on the published version of a module this build makes itself, and the module replaces it on the
      // classpath, as in the maven reactor
      publish = Some(publishAs(mavenProject)),
      postCompile = None
    )
    result += (mainCrossName -> mainProject)

    // Test project - only if test sources exist
    if (testHasSources) {
      val testCrossName = model.CrossProjectName(testProjectName, None)

      // Extract surefire argLine: JVM options and javaagent coordinates
      val (surefireJvmArgs, surefireAgents) = extractSurefireConfig(mavenProject)
      val testJvmOptions = if (surefireJvmArgs.nonEmpty) model.Options.parse(surefireJvmArgs, None) else model.Options.empty

      val testPlatform = model.Platform
        .Jvm(testJvmOptions.union(mavenUserDir), None, model.Options.empty)
        .copy(
          jvmAgents = model.JsonSet.fromIterable(surefireAgents)
        )

      val testProject = model.Project(
        `extends` = model.JsonSet.empty[model.TemplateId],
        cross = model.JsonMap.empty,
        folder = testFolder,
        dependsOn = testDependsOn.map(model.ProjectRef(_)),
        `source-layout` = Some(testSourceLayout),
        `sbt-scope` = Some("test"),
        sources = model.JsonSet.fromIterable(allExtraTestSources),
        resources = model.JsonSet.empty[RelPath],
        dependencies = model.JsonSet.fromIterable(allTestDeps),
        boms = model.JsonSet.empty,
        jars = model.JsonSet.empty,
        java = testJava,
        scala = configuredScala,
        kotlin = configuredKotlin,
        platform = Some(testPlatform),
        isTestProject = Some(true),
        testFrameworks = testFrameworks,
        testTags = model.JsonMap.empty,
        testExclude = model.JsonSet.empty,
        maxConcurrentSuites = None,
        testFork = None,
        sourcegen = model.JsonSet.empty[model.ScriptDef],
        stamp = model.JsonSet.empty[model.StampKind],
        libraryVersionSchemes = model.JsonSet.empty[model.LibraryVersionScheme],
        ignoreEvictionErrors = None,
        publish = None,
        postCompile = None
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
        // `<javaParameters>true</javaParameters>` maps to kotlinc's `-java-parameters`. Dropping it
        // breaks runtime reflection over constructor parameter names — notably Jackson, which then
        // cannot deserialize into Kotlin data classes ("no Creators, like default constructor, exist").
        val javaParameters = (plugin.configuration \ "javaParameters").headOption.filter(_.text.trim == "true").map(_ => "-java-parameters")
        args.map(_.text.trim).toList ++ javaParameters
      case _ => Nil
    }

  /** From the plugin's `compile` execution, its configuration, or the `kotlin.compiler.jvmTarget` property it reads as its default: javalin sets only that */
  private def extractKotlinJvmTarget(mavenProject: MavenProject): Option[String] = {
    val fromPlugin = mavenProject.plugins.find(_.artifactId == "kotlin-maven-plugin").flatMap { plugin =>
      val configurations = plugin.executions.filter(e => e.isEnabled && e.goals.contains("compile")).map(_.configuration) :+ plugin.configuration
      configurations.iterator.flatMap(c => (c \ "jvmTarget").headOption).map(_.text.trim).find(_.nonEmpty)
    }
    fromPlugin.orElse(mavenProject.properties.get("kotlin.compiler.jvmTarget"))
  }

  /** Extract Kotlin compiler plugin IDs from kotlin-maven-plugin configuration.
    *
    * Maven POM format: {{ <configuration> <compilerPlugins> <plugin>spring</plugin> <plugin>jpa</plugin> </compilerPlugins> </configuration> }}
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
    * Maven POM format: {{ <configuration> <pluginOptions> <option>all-open:annotation=jakarta.ws.rs.Path</option> </pluginOptions> </configuration> }}
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
          case arg if !arg.contains("${") && !arg.contains("@{") =>
            jvmOptions += arg
          case _ =>
            () // skip args with unresolved Maven properties, incl. surefire's late-evaluated @{...}
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
        // the effective pom has the version of every dependency, managed or not
        convertDependency(logger, dep) match {
          case Some(bleepDep) =>
            dep.scope match {
              case "test" =>
                testDeps += bleepDep
              case "provided" =>
                mainDeps += bleepDep.withConfiguration(Configuration.provided)
              // what the module needs, but not its consumers. also when it is only needed at runtime: bleep has one configuration per dependency, and
              // leaking a dependency to every consumer is the worse of the two
              case _ if dep.optional =>
                mainDeps += bleepDep.withConfiguration(Configuration.optional)
              case "runtime" =>
                mainDeps += bleepDep.withConfiguration(Configuration.runtime)
              case _ =>
                // compile, system -> main deps
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

    // which artifact of the module. `test-jar` is published as classifier `tests`
    val publication: Publication =
      if (dep.tpe == "jar" && dep.classifier.isEmpty) Publication.empty
      else {
        val (tpe, ext, classifier) =
          if (dep.tpe == "test-jar") (Type.jar, Extension.jar, Classifier.tests)
          else (Type(dep.tpe), Extension(if (dep.tpe == "pom") "pom" else "jar"), Classifier(dep.classifier))
        Publication(dep.artifactId, tpe, ext, classifier)
      }

    val (baseName, crossInfo) = stripScalaSuffix(dep.artifactId)

    val result = crossInfo match {
      case Some(CrossInfo(fullCrossVersion)) =>
        model.Dep.ScalaDependency(
          organization = Organization(dep.groupId),
          baseModuleName = ModuleName(baseName),
          version = dep.version,
          fullCrossVersion = fullCrossVersion,
          exclusions = exclusions,
          publication = publication
        )
      case None =>
        model.Dep.JavaDependency(
          organization = Organization(dep.groupId),
          moduleName = ModuleName(dep.artifactId),
          version = dep.version,
          exclusions = exclusions,
          publication = publication
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
            // `test-jar` of a module is its tests, which are a project of their own
            val name = sanitizeProjectName(dep.artifactId)
            Some(model.ProjectName(if (dep.isTests) s"$name-test" else name))
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

  /** How maven-compiler-plugin compiles a module's code (`compile`) or its tests (`testCompile`). A parameter comes from the execution which runs the goal,
    * then from the plugin's configuration, then from the property the plugin reads as its default. Tests have parameters of their own, `testRelease` wins over
    * `release`: feign compiles its code for java 8 and its tests for java 25
    */
  private class JavaCompile(mavenProject: MavenProject, goal: String) {
    private val isTest = goal == "testCompile"
    private val plugin = mavenProject.plugins.find(_.artifactId == "maven-compiler-plugin")
    // a build may turn the default execution off and run the goal in one of its own
    private val configurations: List[scala.xml.NodeSeq] =
      plugin.toList.flatMap(_.executions).filter(e => e.isEnabled && e.goals.contains(goal)).map(_.configuration) ++ plugin.map(_.configuration).toList

    private def element(name: String): Option[scala.xml.Node] =
      configurations.iterator.flatMap(c => (c \ name).headOption).nextOption()

    private def param(name: String): Option[String] =
      element(name).map(_.text.trim).filter(_.nonEmpty).orElse(mavenProject.properties.get(s"maven.compiler.$name"))

    private def versionParam(name: String): Option[String] =
      (if (isTest) param(s"test${name.capitalize}") else None).orElse(param(name))

    val release: Option[String] = versionParam("release")
    val source: Option[String] = versionParam("source")
    val target: Option[String] = versionParam("target")
    val compilerArgs: List[String] = element("compilerArgs").toList.flatMap(args => (args \ "arg").map(_.text.trim))

    def javaVersion: Option[Int] = release.orElse(target).map(parseJavaVersion(mavenProject, _))

    /** `<annotationProcessorPaths>`: with them javac runs these processors and no others, without them it runs every processor it finds on the classpath */
    val annotationProcessors: List[model.Dep] =
      element("annotationProcessorPaths").toList.flatMap(_ \ "path").map { path =>
        val groupId = (path \ "groupId").text.trim
        val artifactId = (path \ "artifactId").text.trim
        val version = Some((path \ "version").text.trim).filter(_.nonEmpty).getOrElse {
          mavenProject.dependencyManagement
            .find(m => m.groupId == groupId && m.artifactId == artifactId)
            .map(_.version)
            .getOrElse(throw new BleepException.Text(s"${mavenProject.artifactId}: no version for annotation processor $groupId:$artifactId"))
        }
        model.Dep.Java(groupId, artifactId, version)
      }

    private val processingOff: Boolean = param("proc").contains("none") || compilerArgs.contains("-proc:none")

    def toJava: Option[model.Java] = {
      val versionArgs = release match {
        case Some(release) => List("--release", release)
        case None          => source.toList.flatMap(s => List("-source", s)) ++ target.toList.flatMap(t => List("-target", t))
      }
      // without `<annotationProcessorPaths>` javac runs whatever processors are on the classpath, lombok from `provided` say, and finding none is fine
      val scan: Option[model.ScanForAnnotationProcessors] =
        if (annotationProcessors.isEmpty && !processingOff) Some(model.ScanForAnnotationProcessors.IfPresent) else None
      // `-proc:full` asks javac to run processors, which bleep now wires itself. it refuses `-proc:` options next to its own: feign's tests
      val args = if (scan.isDefined || annotationProcessors.nonEmpty) compilerArgs.filterNot(_ == "-proc:full") else compilerArgs
      val java = model.Java(
        options = model.Options.parse(versionArgs ++ args, None),
        scanForAnnotationProcessors = scan,
        annotationProcessors = model.JsonSet.fromIterable(annotationProcessors),
        annotationProcessorOptions = model.AnnotationProcessorOptions.empty,
        ecjVersion = None
      )
      if (java.isEmpty) None else Some(java)
    }
  }

  private object JavaCompile {
    def main(mavenProject: MavenProject): JavaCompile = new JavaCompile(mavenProject, "compile")
    def test(mavenProject: MavenProject): JavaCompile = new JavaCompile(mavenProject, "testCompile")
  }

  /** `17`, and the old spelling `1.8` for java 8 */
  private def parseJavaVersion(mavenProject: MavenProject, version: String): Int =
    version.stripPrefix("1.").toIntOption.getOrElse {
      throw new BleepException.Text(s"${mavenProject.artifactId}: could not understand java version '$version' of maven-compiler-plugin")
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

  /** Which `sbt.testing.Framework` implementations the imported project brings, for `testFrameworks:`.
    *
    * Direct test dependencies only. There used to be a second phase that resolved the whole transitive closure through coursier looking for junit artifacts —
    * an entire dependency resolution per module, swallowing any exception it hit, to detect something bleep does not need told: junit suites are found by
    * scanning for their annotations, and the JUnit Platform Launcher runs them. Nothing else was ever detected transitively.
    */
  private def detectTestFrameworks(mavenProject: MavenProject): model.JsonSet[model.TestFrameworkName] = {
    val fromDeps = mavenProject.dependencies.flatMap { dep =>
      if (dep.scope == "test") {
        val stripped = stripScalaSuffix(dep.artifactId)._1
        testFrameworksByArtifact.get(stripped).map(model.TestFrameworkName.apply)
      } else None
    }.distinct

    model.JsonSet.fromIterable(fromDeps)
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

  private def hasSourceFiles(fs: MavenFs, dir: Path): Boolean =
    fs.isDirectory(dir) && fs.walk(dir).exists { p =>
      val name = p.getFileName.toString
      name.endsWith(".scala") || name.endsWith(".java") || name.endsWith(".kt")
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
      fs: MavenFs,
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

        val mainFiles = collectGeneratedFiles(logger, fs, mainGenDir, isResource = false)
        val testFiles = collectGeneratedFiles(logger, fs, testGenDir, isResource = false)

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

  private def collectGeneratedFiles(logger: Logger, fs: MavenFs, parentDir: Path, isResource: Boolean): Vector[internal.GeneratedFile] =
    if (!fs.isDirectory(parentDir)) Vector.empty
    else
      fs.list(parentDir)
        .filter(fs.isDirectory)
        // what annotation processors wrote (maven-compiler-plugin's default `generatedSourcesDirectory` and `generatedTestSourcesDirectory`). bleep runs the
        // processors itself, and javac refuses to write a file which is already a source: javalin's jmh benchmarks
        .filter(dir => dir.getFileName.toString != "annotations" && dir.getFileName.toString != "test-annotations")
        .flatMap { genDir =>
          fs.walk(genDir)
            .filter(fs.isRegularFile)
            .filter { p =>
              val name = p.getFileName.toString
              name.endsWith(".scala") || name.endsWith(".java") || name.endsWith(".kt")
            }
            .flatMap { file =>
              val content =
                try Some(fs.readString(file))
                catch {
                  case e: Exception =>
                    logger.warn(s"Failed to read generated file $file: $e")
                    None
                }
              content.map(c => internal.GeneratedFile(isResource, c, RelPath.relativeTo(genDir, file)))
            }
        }
        .toVector

  /** Maven's `<dependencyManagement>`, which bleep has no equivalent of. It is expressed in two ways instead.
    *
    * BOMs published to a repository stay BOMs: `boms:` of every module lists the BOMs imported anywhere up its parent chain, those imported by the BOM modules
    * of the build it imports in turn (dropwizard-dependencies), and the parent where the chain leaves the repository (spring-boot-starter-parent) - maven reads
    * that one from a repository, and it manages versions exactly like an imported BOM does.
    *
    * Versions the build manages in its own poms become dependencies, but only where they are used: a managed library maven resolved for a module (directly or
    * transitively, see `mvn dependency:list`) is declared on the module, at the version and scope maven resolved it at. The classpath gets nothing it did not
    * have, and a transitive request for an older version no longer wins. One for a newer version still does - coursier takes the highest version asked for.
    *
    * Imports come from the RAW poms: `mvn help:effective-pom` resolves them away. Everything else comes from the effective pom, where maven has interpolated
    * it.
    */
  private class Management(fs: MavenFs, mavenProjects: List[MavenProject], resolved: () => Map[String, List[MavenResolvedDependency]]) {
    private val reactorModules: Map[(String, String), MavenProject] = mavenProjects.map(p => (p.groupId, p.artifactId) -> p).toMap
    private val chains = scala.collection.mutable.Map.empty[Path, List[(Path, scala.xml.Elem)]]

    /** The module's raw pom and its parent chain in the repository, innermost first */
    private def chain(module: MavenProject): List[(Path, scala.xml.Elem)] =
      chains.getOrElseUpdate(module.directory, rawPomChain(fs, module.directory.resolve("pom.xml").normalize()))

    private def interpolator(module: MavenProject): String => Option[String] = {
      // parent properties first so a module can override its parent, matching Maven's rules
      val props: Map[String, String] =
        chain(module).reverse.flatMap { case (_, pom) =>
          (pom \ "properties").flatMap(_.child).collect { case e: scala.xml.Elem => e.label -> e.text.trim }
        }.toMap ++ Map("project.version" -> module.version, "project.groupId" -> module.groupId)
      val PropRef = "\\$\\{([^}]+)}".r
      value => {
        val out = PropRef.replaceAllIn(value, m => java.util.regex.Matcher.quoteReplacement(props.getOrElse(m.group(1), m.matched)))
        if (out.contains("${")) None else Some(out)
      }
    }

    private def coordinates(module: MavenProject, entry: scala.xml.Node, what: String): (String, String, String) = {
      val interpolate = interpolator(module)
      val raw = ((entry \ "groupId").text.trim, (entry \ "artifactId").text.trim, (entry \ "version").text.trim)
      (interpolate(raw._1), interpolate(raw._2), interpolate(raw._3)) match {
        case (Some(g), Some(a), Some(v)) => (g, a, v)
        case _ => throw new BleepException.Text(s"${module.artifactId}: could not resolve the Maven property references in $what $raw from the raw pom chain")
      }
    }

    private def managementEntries(module: MavenProject): List[scala.xml.Node] =
      chain(module).flatMap { case (_, pom) => pom \ "dependencyManagement" \ "dependencies" \ "dependency" }

    private def isImport(entry: scala.xml.Node): Boolean = (entry \ "scope").text.trim == "import"

    /** BOM modules of the build the module imports, directly or up its chain */
    private def importedBomModules(module: MavenProject): List[MavenProject] =
      managementEntries(module).filter(isImport).flatMap { e =>
        val (g, a, _) = coordinates(module, e, "BOM import")
        reactorModules.get((g, a))
      }

    /** The published BOMs whose management applies to the module */
    def boms(module: MavenProject): List[model.Dep] = {
      def go(m: MavenProject, visited: Set[Path]): List[model.Dep] =
        if (visited(m.directory)) Nil
        else {
          val imported = managementEntries(m).filter(isImport).flatMap { e =>
            val (g, a, v) = coordinates(m, e, "BOM import")
            reactorModules.get((g, a)) match {
              case Some(bomModule) => go(bomModule, visited + m.directory)
              case None            => List(model.Dep.Java(g, a, v))
            }
          }
          val externalParent = chain(m).lastOption.flatMap { case (_, pom) => (pom \ "parent").headOption }.map { parent =>
            val (g, a, v) = coordinates(m, parent, "parent")
            model.Dep.Java(g, a, v)
          }
          imported ++ externalParent.toList
        }
      go(module, Set.empty).distinct
    }

    /** (groupId, artifactId) the build's own poms manage for the module: up its chain, and in the BOM modules it imports */
    private def managedByBuild(module: MavenProject): Set[(String, String)] = {
      def go(m: MavenProject, visited: Set[Path]): Set[(String, String)] =
        if (visited(m.directory)) Set.empty
        else {
          val own = managementEntries(m)
            .filterNot(isImport)
            .map { e =>
              val (g, a, _) = coordinates(m, e, "managed dependency")
              (g, a)
            }
            .toSet
          own ++ importedBomModules(m).flatMap(bom => go(bom, visited + m.directory))
        }
      go(module, Set.empty)
    }

    /** Libraries the build's own poms manage, which maven resolved for the module and the module does not declare itself */
    def usedManaged(module: MavenProject): List[MavenResolvedDependency] = {
      val managed = managedByBuild(module)
      if (managed.isEmpty) Nil
      else {
        val declared = module.dependencies.map(d => (d.groupId, d.artifactId)).toSet
        val forModule = resolved().getOrElse(module.artifactId, throw new BleepException.Text(s"mvn dependency:list printed nothing for ${module.artifactId}"))
        forModule.filter { r =>
          val key = (r.groupId, r.artifactId)
          managed(key) && !declared(key) && !reactorModules.contains(key)
        }
      }
    }
  }

  /** The module's raw pom plus its `<parent>` chain, innermost first, bounded to files that exist on disk (a parent outside the repository resolves from a
    * repository instead and is not read).
    */
  private def rawPomChain(fs: MavenFs, start: Path): List[(Path, scala.xml.Elem)] = {
    def go(pomFile: Path, acc: List[(Path, scala.xml.Elem)]): List[(Path, scala.xml.Elem)] =
      if (!fs.isRegularFile(pomFile) || acc.length > 10) acc.reverse
      else {
        val pom = scala.xml.XML.loadString(fs.readString(pomFile))
        val parentPom = (pom \ "parent").headOption.map { parent =>
          val relativePath = (parent \ "relativePath").text.trim match {
            case ""   => "../pom.xml"
            case path => path
          }
          val resolved = pomFile.getParent.resolve(relativePath).normalize()
          if (fs.isDirectory(resolved)) resolved.resolve("pom.xml") else resolved
        }
        parentPom match {
          case Some(next) => go(next, (pomFile, pom) :: acc)
          case None       => ((pomFile, pom) :: acc).reverse
        }
      }
    go(start, Nil)
  }

  private def sanitizeProjectName(artifactId: String): String =
    artifactId.replace('.', '-')
}
