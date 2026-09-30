package bleep.testing

import bleep.mavenimport.MavenProject
import bleep.{model, ResolvedProject, Usage}
import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec

import java.nio.file.Path
import scala.collection.immutable.{SortedMap, SortedSet}

/** Checks that a maven import loses nothing, by comparing the classpaths bleep resolves with the ones maven itself computed for the same modules (`mvn
  * dependency:build-classpath`, for compile and test scope).
  *
  * A maven module becomes a bleep project, and its tests a `-test` project. Both sides are reduced to sets: jars as `name:version`, a module's classes
  * directory as the project it belongs to. The module's own main classes are on its test classpath in maven, and on the `-test` project's classpath through
  * `dependsOn` in bleep.
  */
object MavenRoundtrip {
  import RoundtripReport.View

  /** What `mvn dependency:build-classpath` printed for a module, in compile and in test scope */
  case class Classpaths(compile: List[String], test: List[String])

  object Classpaths {
    implicit val codec: Codec.AsObject[Classpaths] = deriveCodec
  }

  def report(
      mavenProjects: List[MavenProject],
      classpaths: Map[Path, Classpaths],
      resolved: Map[model.CrossProjectName, ResolvedProject],
      mavenBuildDir: Path,
      bleepBuildDir: Path
  ): String = {
    def location(p: Path): String =
      if (p.startsWith(mavenBuildDir)) s"maven-build/${mavenBuildDir.relativize(p)}"
      else if (p.startsWith(bleepBuildDir)) s"bleep/${bleepBuildDir.relativize(p)}"
      else p.toString

    // same naming as the import
    def projectName(module: MavenProject): String = module.artifactId.replace('.', '-')

    val modules = mavenProjects.filter(_.packaging != "pom")

    // a sibling module maven packaged in the same session: `target/<artifact>.jar`, and `target/<artifact>-tests.jar` for its tests
    def packagedModule(p: Path): Option[String] =
      modules.collectFirst {
        case m if p.getParent == m.directory.resolve("target") =>
          if (p.getFileName.toString.endsWith("-tests.jar")) s"project ${projectName(m)}-test" else s"project ${projectName(m)}"
      }

    def jar(p: Path): Option[String] =
      RoundtripReport.jar(p, ownBuild = p => if (p.startsWith(mavenBuildDir)) packagedModule(p).orElse(Some(location(p))) else None)

    val projectByClassesDir: Map[Path, String] =
      modules.flatMap { m =>
        List(m.directory.resolve("target/classes") -> projectName(m), m.directory.resolve("target/test-classes") -> s"${projectName(m)}-test")
      }.toMap

    def mavenView(entries: List[Path]): View =
      SortedMap(
        "classpath" -> entries.iterator
          .flatMap {
            case p if p.getFileName.toString.endsWith(".jar") => jar(p)
            case p                                            =>
              projectByClassesDir.get(p) match {
                case Some(project) => Some(s"project $project")
                case None          => Some(location(p))
              }
          }
          .to(SortedSet)
      )

    val bleepProjectByClassesDir: Map[Path, String] = resolved.map { case (crossName, p) => (p.classesDir, crossName.value) }
    // bleep puts resource directories of dependencies directly on the classpath, maven copies them into the classes directory
    val bleepResourceDirs: Set[Path] = resolved.values.flatMap(_.resources(Usage.Runtime)).toSet

    def bleepView(p: ResolvedProject): View =
      SortedMap(
        "classpath" -> p
          .classpath(Usage.Compile)
          .iterator
          .flatMap {
            case entry if bleepResourceDirs(entry)                    => None
            case entry if entry.getFileName.toString.endsWith(".jar") => jar(entry)
            case entry                                                =>
              bleepProjectByClassesDir.get(entry) match {
                case Some(project) => Some(s"project $project")
                case None          => Some(location(entry))
              }
          }
          .to(SortedSet)
      )

    val compared = List.newBuilder[String]
    val notImported = List.newBuilder[String]
    val diffs = List.newBuilder[(String, List[(String, List[String])])]

    modules.sortBy(projectName).foreach { module =>
      val cps = classpaths.getOrElse(module.directory, sys.error(s"no maven classpath recorded for ${module.directory}"))
      val main = model.CrossProjectName(model.ProjectName(projectName(module)), None)
      val test = model.CrossProjectName(model.ProjectName(s"${projectName(module)}-test"), None)
      val ownClasses = module.directory.resolve("target/classes")

      List((main, cps.compile.map(Path.of(_))), (test, ownClasses :: cps.test.map(Path.of(_)))).foreach { case (crossName, mavenClasspath) =>
        resolved.get(crossName) match {
          case None =>
            // a module without tests has no test project, and that is fine
            if (crossName == main) notImported += crossName.value
          case Some(p) =>
            compared += crossName.value
            val byAspect = RoundtripReport.diff(mavenView(mavenClasspath), bleepView(p))
            if (byAspect.nonEmpty) diffs += ((crossName.value, byAspect))
        }
      }
    }

    val missing = notImported.result()
    val header = List(
      "maven classpaths (dependency:build-classpath) vs bleep resolved projects, normalized. `-` only in maven, `+` only in bleep, `~` same jar at another version",
      s"maven modules without a bleep project: ${if (missing.isEmpty) "none" else missing.mkString(", ")}"
    )
    RoundtripReport.render(header, compared.result().size, diffs.result())
  }
}
