package bleep
package scripts

import bleep.model.VersionCombo

import java.nio.file.{Files, Path, Paths}

/** Proof-of-concept Maven exporter: walks the exploded build model and writes a buildable Maven layout.
  *
  * This exists to make the claim in `docs/guides/exit-strategy.mdx` literally true: bleep's build model is data, so exporting to Maven POMs is a mechanical
  * walk. Usage: `bleep export-maven <output-directory> [--skip-tests <test-project-name>]...` — the output directory is required, there is no default.
  * `--skip-tests` marks a named test project's suite execution skipped in its generated pom (with a loud comment; the suites still compile): use it for test
  * projects whose suites drive bleep's own toolchain end-to-end and therefore cannot run under stock Maven. For bleep's own build that is `bleep-bsp-tests`
  * (in-process BSP server, zinc, platform linkers); `bleep-tests`' unit suites run green under Maven and stay on.
  *
  * What it writes:
  *   - an aggregator `pom.xml` in the output directory listing every exported project as a `<module>`
  *   - per project a `<artifactId>/pom.xml` with coordinates, dependencies, source/resource dirs (via `build-helper-maven-plugin`, since bleep's layout differs
  *     from Maven's default), and compiler setup (`scala-maven-plugin` for Scala, `kotlin-maven-plugin` for Kotlin, `maven-compiler-plugin` for pure-Java)
  *   - for test projects (`isTestProject`): sources are wired as Maven *test* sources, and `scalatest-maven-plugin` runs the suites during `mvn test`/`install`
  *     (the default surefire execution is skipped — ScalaTest suites are not JUnit tests). The forked-test JVM options bleep would use
  *     (`platform.jvmOptions`/`jvmRuntimeOptions`) are translated to the plugin's `argLine`.
  *
  * Decisions documented for the PoC:
  *   - version: every exported module gets the fixed version [[ExportVersion]] (`0.1.0-SNAPSHOT`). A real exporter would derive it from dynver the way the
  *     publish scripts do; version schemes are not the point being proven here.
  *   - groupId: the project's `publish.groupId` if configured, else [[DefaultGroupId]].
  *   - artifactId: the cross project name (`name` or `name@crossId`) with every character outside `[A-Za-z0-9._-]` replaced by `-`. Note this means exported
  *     Scala artifacts do not carry the `_3`/`_2.13` suffix bleep's real publishing would add — coordinates are only consumed inside the exported reactor.
  *   - external dependencies are concretized through the model's own machinery (`Dep.asJava` with the project's `VersionCombo`, versions and options filled
  *     through `model.Replacements` exactly like `CoursierResolver` does), so Scala `::`/`:::` deps get the exact `_2.13`/`_3` suffix bleep itself resolves —
  *     no string-concatenation reimplementation. Deps that cannot be rendered fail the export.
  *   - Scala/Kotlin projects additionally get the standard-library deps the model prescribes for their `VersionCombo` (`combo.libraries`), so the modules
  *     actually compile under Maven.
  *   - `dependsOn` edges become `<dependency>` entries with the sibling module's exported coordinates, resolved cross-aware via `build.resolvedDependsOn`.
  *   - both generated-source layouts can exist on disk simultaneously (legacy `.bleep/generated-sources/<name>/<folder>` written by older bleep versions and
  *     the current `.bleep/projects/<name>/generated-sources/<folder>`), and the released bleep-core this script runs against reports both. Compiling both
  *     under Maven duplicates every generated class, so the exporter deterministically keeps the layout the current bleep writes and drops the legacy dir
  *     whenever its current-layout twin is present in the same set. No content-based dedupe — purely the documented layout preference.
  *   - test suite discovery under Maven is narrowed to class names ending in `Test` (ScalaTest Runner `-q`): bleep's `*IT` suites are end-to-end integration
  *     tests that drive bleep's own toolchain (in-process BSP server, dev-deps shim resolution of in-tree fixtures, real build imports) and cannot sensibly run
  *     under stock Maven. Each generated test pom carries a loud comment saying so. The `*Test` suites compile AND execute for real via
  *     `scalatest-maven-plugin`, with bleep's forked-test JVM options translated to `argLine` and bleep's fork cwd (the project directory) translated to
  *     `workingDirectory`.
  *
  * Known limitations beyond the above: sourcegen is not executed by Maven (generated source dirs are referenced but only populated if bleep ran first),
  * unmanaged `jars` are ignored, Scala `compilerPlugins` are not translated, KSP symbol processing is not wired (fail-hard if a module needs it at build time),
  * and publish/assembly configuration is not emitted.
  */
object ExportMaven extends BleepScript("ExportMaven") {

  /** Every exported module shares this fixed version. See scaladoc above for why the PoC does not derive it from dynver. */
  val ExportVersion = "0.1.0-SNAPSHOT"

  /** groupId for projects without a `publish.groupId` in the build. */
  val DefaultGroupId = "build.bleep.exported"

  val BuildHelperPluginVersion = "3.6.0"
  val MavenCompilerPluginVersion = "3.13.0"
  val ScalaMavenPluginVersion = "4.9.2"
  val SurefirePluginVersion = "3.2.5"
  val ScalaTestMavenPluginVersion = "2.2.0"

  private val MavenScopes = Set("compile", "provided", "runtime", "test")

  // a tiny XML tree with proper escaping. scala-xml is only transitively on the scripts classpath (via published
  // bleep-core -> bleep-nosbt), and Scala 3 XML literals are on their way out, so we keep the writer local: element
  // content only, no mixed content, deterministic two-space indentation.
  private sealed trait Xml
  private final case class El(tag: String, children: List[Xml]) extends Xml
  private final case class Txt(value: String) extends Xml
  private final case class Comment(text: String) extends Xml

  private def el(tag: String)(children: Xml*): El = El(tag, children.toList)
  private def leaf(tag: String, value: String): El = El(tag, List(Txt(value)))

  private def xmlEscape(s: String): String =
    s.flatMap {
      case '&'   => "&amp;"
      case '<'   => "&lt;"
      case '>'   => "&gt;"
      case '"'   => "&quot;"
      case other => other.toString
    }

  private def render(node: Xml, indent: Int, sb: java.lang.StringBuilder): Unit = {
    val pad = "  " * indent
    node match {
      case Comment(text) =>
        if (text.contains("--")) sys.error(s"XML comments cannot contain '--': $text")
        sb.append(pad).append("<!-- ").append(text).append(" -->\n")
        ()
      case Txt(value) =>
        sys.error(s"mixed content is not supported: $value")
      case element: El =>
        element.children match {
          case Nil =>
            sb.append(pad).append('<').append(element.tag).append("/>\n")
            ()
          case List(Txt(value)) =>
            sb.append(pad).append('<').append(element.tag).append('>').append(xmlEscape(value)).append("</").append(element.tag).append(">\n")
            ()
          case children =>
            sb.append(pad).append('<').append(element.tag).append(">\n")
            children.foreach {
              case Txt(value) => sys.error(s"mixed content under <${element.tag}> is not supported: $value")
              case child      => render(child, indent + 1, sb)
            }
            sb.append(pad).append("</").append(element.tag).append(">\n")
            ()
        }
    }
  }

  private def renderPom(children: List[Xml]): String = {
    val sb = new java.lang.StringBuilder
    sb.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
    sb.append("<project xmlns=\"http://maven.apache.org/POM/4.0.0\"\n")
    sb.append("         xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n")
    sb.append("         xsi:schemaLocation=\"http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd\">\n")
    children.foreach(child => render(child, 1, sb))
    sb.append("</project>\n")
    sb.toString
  }

  private final case class Coords(groupId: String, artifactId: String, version: String)

  private def sanitizeArtifactId(value: String): String =
    value.map {
      case c if c.isLetterOrDigit || c == '.' || c == '_' || c == '-' => c
      case _                                                          => '-'
    }

  private def groupIdFor(project: model.Project): String =
    project.publish.flatMap(_.groupId) match {
      case Some(groupId) => groupId
      case None          => DefaultGroupId
    }

  private final case class MavenDep(groupId: String, artifactId: String, version: String, scope: Option[String], exclusions: List[(String, String)])

  private def translateDep(owner: model.CrossProjectName, dep: model.Dep, combo: VersionCombo): MavenDep = {
    val javaDep: model.Dep.JavaDependency = dep.asJava(combo) match {
      case Right(javaDep) => javaDep
      case Left(msg)      => sys.error(s"${owner.value}: cannot render dependency ${dep.repr} for Maven: $msg")
    }
    if (javaDep.publication != model.Dep.defaults.publication)
      sys.error(s"${owner.value}: dependency ${dep.repr} has a publication (classifier/type) — not supported by this PoC")
    val scope: Option[String] =
      if (javaDep.configuration.isEmpty) None
      else if (MavenScopes(javaDep.configuration.value)) Some(javaDep.configuration.value)
      else sys.error(s"${owner.value}: dependency ${dep.repr} has configuration '${javaDep.configuration.value}' which has no Maven scope equivalent")
    val exclusions: List[(String, String)] =
      javaDep.exclusions.value.toList.flatMap { case (org, moduleNames) => moduleNames.values.toList.map(moduleName => (org.value, moduleName.value)) }.sorted
    MavenDep(javaDep.organization.value, javaDep.moduleName.value, javaDep.version, scope, exclusions)
  }

  private def depXml(dep: MavenDep): El = {
    val exclusionsXml: List[El] =
      if (dep.exclusions.isEmpty) Nil
      else List(El("exclusions", dep.exclusions.map { case (group, artifact) => el("exclusion")(leaf("groupId", group), leaf("artifactId", artifact)) }))
    El(
      "dependency",
      List(leaf("groupId", dep.groupId), leaf("artifactId", dep.artifactId), leaf("version", dep.version)) ++
        dep.scope.map(scope => leaf("scope", scope)).toList ++ exclusionsXml
    )
  }

  override def run(started: Started, commands: Commands, args: List[String]): Unit = {
    val usage = "usage: export-maven <output-directory> [--skip-tests <test-project-name>]..."
    val (outDir: Path, skipTestExecution: Set[model.ProjectName]) = args match {
      case dir :: rest =>
        def parseSkips(remaining: List[String], acc: Set[model.ProjectName]): Set[model.ProjectName] =
          remaining match {
            case Nil                                => acc
            case "--skip-tests" :: name :: moreRest => parseSkips(moreRest, acc + model.ProjectName(name))
            case other                              => sys.error(s"$usage — cannot parse ${other.mkString("'", "' '", "'")}")
          }
        (Paths.get(dir).toAbsolutePath.normalize(), parseSkips(rest, Set.empty))
      case Nil => sys.error(s"$usage — got no arguments")
    }

    val all: List[(model.CrossProjectName, model.Project)] =
      started.build.explodedProjects.toList.sortBy { case (crossName, _) => crossName }

    val withCombo: List[(model.CrossProjectName, model.Project, VersionCombo)] =
      all.map { case (crossName, project) =>
        val combo = VersionCombo.fromExplodedProject(project) match {
          case Right(combo) => combo
          case Left(msg)    => sys.error(s"${crossName.value}: cannot determine version combo: $msg")
        }
        (crossName, project, combo)
      }

    val directSkipReasons: Map[model.CrossProjectName, String] =
      withCombo.flatMap { case (crossName, _, combo) =>
        combo match {
          case VersionCombo.Java | VersionCombo.Jvm(_) | VersionCombo.Kotlin(_) => None
          case VersionCombo.Js(_, _)                                            => Some((crossName, "Scala.js platform is not exported by this PoC"))
          case VersionCombo.Native(_, _)                                        => Some((crossName, "Scala Native platform is not exported by this PoC"))
        }
      }.toMap

    // skipping a project must cascade: a module whose dependsOn was skipped cannot get correct coordinates, so it is skipped too (to fixpoint)
    val skipReasons: Map[model.CrossProjectName, String] = {
      var acc = directSkipReasons
      var changed = true
      while (changed) {
        val next = withCombo.flatMap { case (crossName, _, _) =>
          if (acc.contains(crossName)) None
          else
            started.build.resolvedDependsOn(crossName).find(acc.contains) match {
              case Some(skippedDep) => Some((crossName, s"depends on skipped project ${skippedDep.value}"))
              case None             => None
            }
        }
        changed = next.nonEmpty
        acc = acc ++ next
      }
      acc
    }

    val (skipped, exported) = withCombo.partition { case (crossName, _, _) => skipReasons.contains(crossName) }

    skipped.foreach { case (crossName, _, _) => started.logger.warn(s"skipping ${crossName.value}: ${skipReasons(crossName)}") }

    if (exported.isEmpty) sys.error("no JVM projects found to export")

    val moduleIds: Map[model.CrossProjectName, String] =
      exported.map { case (crossName, _, _) => (crossName, sanitizeArtifactId(crossName.value)) }.toMap

    moduleIds.groupBy { case (_, artifactId) => artifactId }.foreach { case (artifactId, entries) =>
      if (entries.size > 1)
        sys.error(s"artifactId collision after sanitizing: ${entries.keys.map(_.value).mkString(", ")} all map to '$artifactId'")
    }

    val coordsByName: Map[model.CrossProjectName, Coords] =
      exported.map { case (crossName, project, _) => (crossName, Coords(groupIdFor(project), moduleIds(crossName), ExportVersion)) }.toMap

    skipTestExecution.foreach { projectName =>
      val isExportedTestProject = exported.exists { case (crossName, project, _) =>
        crossName.name == projectName && project.isTestProject.contains(true)
      }
      if (!isExportedTestProject)
        sys.error(s"--skip-tests ${projectName.value}: not an exported test project")
    }

    Files.createDirectories(outDir)

    exported.foreach { case (crossName, project, combo) =>
      val coords = coordsByName(crossName)
      val moduleDir = outDir / coords.artifactId
      Files.createDirectories(moduleDir)

      val isTest = project.isTestProject.contains(true)

      val projectPaths = started.projectPaths(crossName)

      // the model keeps templates like ${BLEEP_VERSION}/${SCALA_VERSION} in dep versions and options; concretize them
      // with the same Replacements machinery bleep itself uses (see CoursierResolver/ResolveProjects)
      val replacements =
        model.Replacements.paths(build = started.buildPaths.buildDir) ++
          model.Replacements.projectPaths(project = projectPaths.dir) ++
          model.Replacements.targetDir(projectPaths.targetDir) ++
          model.Replacements.versions(
            Some(started.build.$version),
            combo,
            includeEpoch = true,
            includeBinVersion = true,
            buildDir = Some(started.buildPaths.buildDir)
          )

      val interProjectDeps: List[El] =
        started.build.resolvedDependsOn(crossName).toList.map { depCrossName =>
          val depCoords = coordsByName.get(depCrossName) match {
            case Some(depCoords) => depCoords
            case None => sys.error(s"${crossName.value}: depends on ${depCrossName.value}, which was skipped by the export — cannot emit coordinates")
          }
          depXml(MavenDep(depCoords.groupId, depCoords.artifactId, depCoords.version, scope = None, exclusions = Nil))
        }

      val comboLibraries: List[model.Dep] = combo.libraries(isTest = isTest)

      val externalDeps: List[MavenDep] =
        (comboLibraries ++ project.dependencies.values.toList.sorted).map { dep =>
          val translated = translateDep(crossName, replacements.fill.dep(dep), combo)
          if (translated.version.contains("${"))
            sys.error(s"${crossName.value}: dependency ${dep.repr} still has an unresolved template in version '${translated.version}' after replacements")
          translated
        }

      val dedupedExternal: List[MavenDep] =
        externalDeps
          .groupBy(dep => (dep.groupId, dep.artifactId))
          .toList
          .map { case ((groupId, artifactId), entries) =>
            val versions = entries.map(_.version).distinct
            if (versions.size > 1)
              sys.error(s"${crossName.value}: dependency $groupId:$artifactId requested at conflicting versions ${versions.mkString(", ")}")
            entries.head
          }
          .sortBy(dep => (dep.groupId, dep.artifactId))

      val dependenciesXml: List[El] = {
        val entries = interProjectDeps ++ dedupedExternal.map(depXml)
        if (entries.isEmpty) Nil else List(El("dependencies", entries))
      }

      def relativeTo(dir: Path): String = moduleDir.relativize(dir).toString

      // see scaladoc: prefer the current generated-output layout, drop the legacy dir when its current twin is also listed
      def dropLegacyGenerated(dirs: List[Path], folderName: String): List[Path] = {
        val legacyBase = started.buildPaths.buildDir / ".bleep" / folderName / crossName.value
        val currentBase = started.buildPaths.buildDir / ".bleep" / "projects" / crossName.value / folderName
        dirs.filterNot(dir => dir.startsWith(legacyBase) && dirs.contains(currentBase.resolve(legacyBase.relativize(dir))))
      }

      val sourceDirs: List[String] = dropLegacyGenerated(projectPaths.sourcesDirs.all.toList, "generated-sources").map(relativeTo)
      val resourceDirs: List[String] = dropLegacyGenerated(projectPaths.resourcesDirs.all.toList, "generated-resources").map(relativeTo)

      // a bleep test project's whole source tree is tests: wire it as Maven test sources so surefire/scalatest compile and run it as such
      val (addSourceGoal, addResourceGoal, sourcesTag) =
        if (isTest) ("add-test-source", "add-test-resource", "generate-test-sources") else ("add-source", "add-resource", "generate-sources")

      val buildHelperExecutions: List[El] =
        List(
          el("execution")(
            leaf("id", addSourceGoal),
            leaf("phase", sourcesTag),
            el("goals")(leaf("goal", addSourceGoal)),
            el("configuration")(El("sources", sourceDirs.map(dir => leaf("source", dir))))
          )
        ) ++ (
          if (resourceDirs.isEmpty) Nil
          else
            List(
              el("execution")(
                leaf("id", addResourceGoal),
                leaf("phase", if (isTest) "generate-test-resources" else "generate-resources"),
                el("goals")(leaf("goal", addResourceGoal)),
                el("configuration")(
                  El("resources", resourceDirs.map(dir => el("resource")(leaf("directory", dir))))
                )
              )
            )
        )

      val buildHelperPlugin: El =
        el("plugin")(
          leaf("groupId", "org.codehaus.mojo"),
          leaf("artifactId", "build-helper-maven-plugin"),
          leaf("version", BuildHelperPluginVersion),
          El("executions", buildHelperExecutions)
        )

      // javac configuration applies regardless of combo: Scala/Kotlin modules can still hold java sources and always
      // carry javac options in bleep (e.g. scripts-java is a `scala:`-configured project whose sources are pure java)
      val mavenCompilerXml: List[El] = {
        val javacOpts: List[model.Options.Opt] = project.java match {
          case Some(javaConfig) => replacements.fill.opts(javaConfig.options).values.toList.sorted
          case None             => Nil
        }
        // `--release` must go into the plugin's <release> element: passing it through compilerArgs collides with the
        // implicit `--source` maven-compiler-plugin derives from its defaults
        val (releaseOpts, otherOpts) = javacOpts.partition {
          case model.Options.Opt.WithArgs("--release" | "-release", _) => true
          case _                                                       => false
        }
        val releaseXml: List[El] = releaseOpts match {
          case Nil                                                => Nil
          case List(model.Options.Opt.WithArgs(_, List(version))) => List(leaf("release", version))
          case other                                              => sys.error(s"${crossName.value}: cannot translate javac release options: $other")
        }
        val remainingArgs: List[String] = otherOpts.flatMap(_.render)
        val argsXml: List[El] = if (remainingArgs.isEmpty) Nil else List(El("compilerArgs", remainingArgs.map(opt => leaf("arg", opt))))
        if (releaseXml.isEmpty && argsXml.isEmpty) Nil
        else
          List(
            el("plugin")(
              leaf("groupId", "org.apache.maven.plugins"),
              leaf("artifactId", "maven-compiler-plugin"),
              leaf("version", MavenCompilerPluginVersion),
              El("configuration", releaseXml ++ argsXml)
            )
          )
      }

      val compilerPlugins: List[El] = combo match {
        case scala: VersionCombo.Scala =>
          val scalacOptions: List[String] = project.scala match {
            case Some(scalaConfig) => replacements.fill.opts(scalaConfig.options).render
            case None              => sys.error(s"${crossName.value}: scala VersionCombo but no scala config")
          }
          val argsXml: List[El] = if (scalacOptions.isEmpty) Nil else List(El("args", scalacOptions.map(opt => leaf("arg", opt))))
          el("plugin")(
            leaf("groupId", "net.alchim31.maven"),
            leaf("artifactId", "scala-maven-plugin"),
            leaf("version", ScalaMavenPluginVersion),
            el("executions")(el("execution")(el("goals")(leaf("goal", "compile"), leaf("goal", "testCompile")))),
            El("configuration", leaf("scalaVersion", scala.scalaVersion.scalaVersion) :: argsXml)
          ) :: mavenCompilerXml
        case VersionCombo.Kotlin(kotlinVersion) =>
          val kotlinConfig = project.kotlin match {
            case Some(kotlinConfig) => kotlinConfig
            case None               => sys.error(s"${crossName.value}: kotlin VersionCombo but no kotlin config")
          }
          if (kotlinConfig.compilerPlugins.values.nonEmpty)
            sys.error(s"${crossName.value}: kotlin compilerPlugins are not translated by this PoC")
          if (kotlinConfig.symbolProcessors.values.nonEmpty || kotlinConfig.scanForSymbolProcessors.contains(true))
            sys.error(s"${crossName.value}: KSP symbol processing is not wired by this PoC")
          val kotlinOptions: List[String] = replacements.fill.opts(kotlinConfig.options).render
          val argsXml: List[El] = if (kotlinOptions.isEmpty) Nil else List(El("args", kotlinOptions.map(opt => leaf("arg", opt))))
          val jvmTargetXml: List[El] = kotlinConfig.jvmTarget.map(target => leaf("jvmTarget", target)).toList
          val configXml: List[El] = jvmTargetXml ++ argsXml match {
            case Nil      => Nil
            case nonEmpty => List(El("configuration", nonEmpty))
          }
          val compileExecutions =
            el("execution")(
              leaf("id", "compile"),
              leaf("phase", "compile"),
              el("goals")(leaf("goal", "compile")),
              el("configuration")(El("sourceDirs", sourceDirs.map(dir => leaf("sourceDir", dir))))
            ) :: (
              if (isTest)
                List(
                  el("execution")(
                    leaf("id", "test-compile"),
                    leaf("phase", "test-compile"),
                    el("goals")(leaf("goal", "test-compile")),
                    el("configuration")(El("sourceDirs", sourceDirs.map(dir => leaf("sourceDir", dir))))
                  )
                )
              else Nil
            )
          El(
            "plugin",
            List(
              leaf("groupId", "org.jetbrains.kotlin"),
              leaf("artifactId", "kotlin-maven-plugin"),
              leaf("version", kotlinVersion.kotlinVersion),
              El("executions", compileExecutions)
            ) ++ configXml
          ) :: mavenCompilerXml
        case VersionCombo.Java =>
          mavenCompilerXml
      }

      val testPlugins: List[Xml] =
        if (!isTest) Nil
        else {
          val usesScalatest = project.dependencies.values.exists {
            case scala: model.Dep.ScalaDependency => scala.organization.value == "org.scalatest" && scala.baseModuleName.value == "scalatest"
            case _                                => false
          }
          if (!usesScalatest)
            sys.error(s"${crossName.value}: test project without a scalatest dependency — no Maven test wiring implemented for its framework in this PoC")

          val forkJvmOptions: List[String] = project.platform match {
            case Some(platform) => replacements.fill.opts(platform.jvmOptions).union(replacements.fill.opts(platform.jvmRuntimeOptions)).render
            case None           => Nil
          }
          val argLineXml: List[El] = if (forkJvmOptions.isEmpty) Nil else List(leaf("argLine", forkJvmOptions.mkString(" ")))

          val skipThisModule = skipTestExecution.contains(crossName.name)
          val skipCommentXml: List[Xml] =
            if (!skipThisModule) Nil
            else
              List(
                Comment(
                  s"TESTS SKIPPED: the exporter was invoked with its skip-tests flag for ${crossName.name.value}. These suites drive bleep's compile " +
                    "toolchain end-to-end (in-process BSP server, zinc, platform linkers) and do not run under stock Maven. " +
                    s"They still COMPILE as part of this build. Run them with `bleep test ${crossName.name.value}` instead"
                )
              )
          val skipTestsXml: List[El] = if (skipThisModule) List(leaf("skipTests", "true")) else Nil

          skipCommentXml ++ List(
            Comment("bleep test projects hold ScalaTest suites, not JUnit tests: surefire is skipped, scalatest-maven-plugin below runs the suites"),
            Comment(
              "suite discovery is narrowed to the *Test suffix: bleep's *IT suites are end-to-end integration tests that drive " +
                "bleep's own toolchain (in-process BSP server, dev-deps shim, real imports) and cannot sensibly run under stock Maven. " +
                "The *Test suites run for real. workingDirectory reproduces bleep's forked-test cwd (the project directory inside the workspace)"
            ),
            el("plugin")(
              leaf("groupId", "org.apache.maven.plugins"),
              leaf("artifactId", "maven-surefire-plugin"),
              leaf("version", SurefirePluginVersion),
              el("configuration")(leaf("skipTests", "true"))
            ),
            El(
              "plugin",
              List(
                leaf("groupId", "org.scalatest"),
                leaf("artifactId", "scalatest-maven-plugin"),
                leaf("version", ScalaTestMavenPluginVersion),
                El(
                  "configuration",
                  List(
                    leaf("reportsDirectory", "${project.build.directory}/scalatest-reports"),
                    leaf("junitxml", "."),
                    leaf("filereports", "TestSuite.txt"),
                    leaf("suffixes", "Test"),
                    leaf("workingDirectory", relativeTo(projectPaths.dir))
                  ) ++ argLineXml ++ skipTestsXml
                ),
                el("executions")(el("execution")(leaf("id", "test"), el("goals")(leaf("goal", "test"))))
              )
            )
          )
        }

      val pom = renderPom(
        List(
          leaf("modelVersion", "4.0.0"),
          leaf("groupId", coords.groupId),
          leaf("artifactId", coords.artifactId),
          leaf("version", coords.version),
          leaf("packaging", "jar")
        ) ++ dependenciesXml ++ List(
          el("build")(El("plugins", (buildHelperPlugin :: compilerPlugins) ++ testPlugins))
        )
      )

      Files.writeString(moduleDir / "pom.xml", pom)
      started.logger.info(s"wrote ${moduleDir / "pom.xml"} for ${crossName.value}")
      ()
    }

    val aggregatorPom = renderPom(
      List(
        leaf("modelVersion", "4.0.0"),
        leaf("groupId", DefaultGroupId),
        leaf("artifactId", "aggregator"),
        leaf("version", ExportVersion),
        leaf("packaging", "pom"),
        El("modules", exported.map { case (crossName, _, _) => leaf("module", moduleIds(crossName)) })
      )
    )
    Files.writeString(outDir / "pom.xml", aggregatorPom)
    started.logger.info(s"wrote aggregator ${outDir / "pom.xml"}: ${exported.size} modules exported, ${skipped.size} projects skipped")
    ()
  }
}
