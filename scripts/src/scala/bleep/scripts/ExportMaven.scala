package bleep
package scripts

import bleep.model.VersionCombo

import java.nio.file.{Files, Path, Paths}

/** Proof-of-concept Maven exporter: walks the exploded build model and writes a buildable Maven layout.
  *
  * This exists to make the claim in `docs/guides/exit-strategy.mdx` literally true: bleep's build model is data, so exporting to Maven POMs is a mechanical
  * walk. Usage: `bleep export-maven <output-directory>` — the output directory is required, there is no default.
  *
  * What it writes:
  *   - an aggregator `pom.xml` in the output directory listing every exported project as a `<module>`
  *   - per project a `<artifactId>/pom.xml` with coordinates, dependencies, source/resource dirs (via `build-helper-maven-plugin`, since bleep's layout differs
  *     from Maven's default), and compiler setup (`scala-maven-plugin` for Scala projects, `maven-compiler-plugin` for pure-Java projects)
  *
  * Decisions documented for the PoC:
  *   - version: every exported module gets the fixed version [[ExportVersion]] (`0.1.0-SNAPSHOT`). A real exporter would derive it from dynver the way the
  *     publish scripts do; version schemes are not the point being proven here.
  *   - groupId: the project's `publish.groupId` if configured, else [[DefaultGroupId]].
  *   - artifactId: the cross project name (`name` or `name@crossId`) with every character outside `[A-Za-z0-9._-]` replaced by `-`.
  *   - external dependencies are concretized through the model's own machinery (`Dep.asJava` with the project's `VersionCombo`), so Scala `::`/`:::` deps get
  *     the exact `_2.13`/`_3`/full-version suffix bleep itself resolves — no string-concatenation reimplementation. Deps that cannot be rendered fail the
  *     export.
  *   - Scala projects additionally get the standard library deps the model prescribes for their `VersionCombo` (`combo.libraries`), so the modules actually
  *     compile under Maven.
  *   - `dependsOn` edges become `<dependency>` entries with the sibling module's exported coordinates, resolved cross-aware via `build.resolvedDependsOn`.
  *   - test projects (`isTestProject`) are exported as ordinary modules; translating them to Maven's test scope (test-jars, `<scope>test</scope>` wiring,
  *     surefire) is out of scope for the PoC.
  *
  * Skipped (logged, not exported): JS/Native cross variants and Kotlin projects — Maven compiler setup for those is not wired in this PoC. Skips cascade: a
  * project depending on a skipped project is skipped too, since its inter-project coordinates could not be emitted correctly.
  *
  * Known limitations beyond the above: sourcegen is not executed by Maven (generated source dirs are referenced but only populated if bleep ran first),
  * unmanaged `jars` are ignored, Scala `compilerPlugins` are not translated, and publish/assembly configuration is not emitted.
  */
object ExportMaven extends BleepScript("ExportMaven") {

  /** Every exported module shares this fixed version. See scaladoc above for why the PoC does not derive it from dynver. */
  val ExportVersion = "0.1.0-SNAPSHOT"

  /** groupId for projects without a `publish.groupId` in the build. */
  val DefaultGroupId = "build.bleep.exported"

  val BuildHelperPluginVersion = "3.6.0"
  val MavenCompilerPluginVersion = "3.13.0"
  val ScalaMavenPluginVersion = "4.9.2"

  private val MavenScopes = Set("compile", "provided", "runtime", "test")

  // a tiny XML tree with proper escaping. scala-xml is only transitively on the scripts classpath (via published
  // bleep-core -> bleep-nosbt), and Scala 3 XML literals are on their way out, so we keep the writer local: element
  // content only, no mixed content, deterministic two-space indentation.
  private sealed trait Xml
  private final case class El(tag: String, children: List[Xml]) extends Xml
  private final case class Txt(value: String) extends Xml

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

  private def render(element: El, indent: Int, sb: java.lang.StringBuilder): Unit = {
    val pad = "  " * indent
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
          case child: El  => render(child, indent + 1, sb)
          case Txt(value) => sys.error(s"mixed content under <${element.tag}> is not supported: $value")
        }
        sb.append(pad).append("</").append(element.tag).append(">\n")
        ()
    }
  }

  private def renderPom(children: List[El]): String = {
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
    val outDir: Path = args match {
      case List(dir) => Paths.get(dir).toAbsolutePath.normalize()
      case other     => sys.error(s"usage: export-maven <output-directory> — got ${if (other.isEmpty) "no arguments" else other.mkString("'", "' '", "'")}")
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
          case VersionCombo.Java | VersionCombo.Jvm(_) => None
          case VersionCombo.Js(_, _)                   => Some((crossName, "Scala.js platform is not exported by this PoC"))
          case VersionCombo.Native(_, _)               => Some((crossName, "Scala Native platform is not exported by this PoC"))
          case VersionCombo.Kotlin(_)                  => Some((crossName, "Kotlin compiler setup is not wired by this PoC"))
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

    Files.createDirectories(outDir)

    exported.foreach { case (crossName, project, combo) =>
      val coords = coordsByName(crossName)
      val moduleDir = outDir / coords.artifactId
      Files.createDirectories(moduleDir)

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

      val comboLibraries: List[model.Dep] = combo match {
        case scala: VersionCombo.Scala => scala.libraries(isTest = project.isTestProject.contains(true))
        case VersionCombo.Java         => Nil
        case other                     => sys.error(s"${crossName.value}: unexpected combo $other for an exported project")
      }

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

      val sourceDirs: List[String] = projectPaths.sourcesDirs.all.toList.map(relativeTo)
      val resourceDirs: List[String] = projectPaths.resourcesDirs.all.toList.map(relativeTo)

      val buildHelperExecutions: List[El] =
        List(
          el("execution")(
            leaf("id", "add-sources"),
            leaf("phase", "generate-sources"),
            el("goals")(leaf("goal", "add-source")),
            el("configuration")(El("sources", sourceDirs.map(dir => leaf("source", dir))))
          )
        ) ++ (
          if (resourceDirs.isEmpty) Nil
          else
            List(
              el("execution")(
                leaf("id", "add-resources"),
                leaf("phase", "generate-resources"),
                el("goals")(leaf("goal", "add-resource")),
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

      val compilerPlugins: List[El] = combo match {
        case scala: VersionCombo.Scala =>
          val scalacOptions: List[String] = project.scala match {
            case Some(scalaConfig) => replacements.fill.opts(scalaConfig.options).render
            case None              => sys.error(s"${crossName.value}: scala VersionCombo but no scala config")
          }
          val argsXml: List[El] = if (scalacOptions.isEmpty) Nil else List(El("args", scalacOptions.map(opt => leaf("arg", opt))))
          List(
            el("plugin")(
              leaf("groupId", "net.alchim31.maven"),
              leaf("artifactId", "scala-maven-plugin"),
              leaf("version", ScalaMavenPluginVersion),
              el("executions")(el("execution")(el("goals")(leaf("goal", "compile"), leaf("goal", "testCompile")))),
              El("configuration", leaf("scalaVersion", scala.scalaVersion.scalaVersion) :: argsXml)
            )
          )
        case VersionCombo.Java =>
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
        case other => sys.error(s"${crossName.value}: unexpected combo $other for an exported project")
      }

      val pom = renderPom(
        List(
          leaf("modelVersion", "4.0.0"),
          leaf("groupId", coords.groupId),
          leaf("artifactId", coords.artifactId),
          leaf("version", coords.version),
          leaf("packaging", "jar")
        ) ++ dependenciesXml ++ List(
          el("build")(El("plugins", buildHelperPlugin :: compilerPlugins))
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
