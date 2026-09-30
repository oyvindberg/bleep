package bleep
package mavenimport

import java.nio.file.Path
import scala.collection.immutable.SortedMap

/** Where the imported build resolves a library differently from maven.
  *
  * Maven lets the version declared nearest a module win, coursier the highest version asked for anywhere, so an imported build can get newer versions of some
  * libraries, and code which compiled under maven may not compile or run the same. This compares, for each module, what `mvn dependency:list` resolved with the
  * classpaths bleep resolves: the module's code against maven's compile, provided, runtime and system scopes, its tests against all of them and test. Libraries
  * are compared by artifact and classifier. Modules of the build itself are left out, in bleep they are projects the others depend on.
  */
object ResolutionReport {

  /** A library as maven or bleep has it: artifact, and classifier if any */
  case class Library(artifactId: String, classifier: String) {
    def render: String = if (classifier.isEmpty) artifactId else s"$artifactId:$classifier"
  }

  sealed trait Difference {
    def render: String
  }
  object Difference {
    case class OtherVersion(library: Library, maven: String, bleep: String) extends Difference {
      def render = s"~ ${library.render}: $maven -> $bleep"
    }
    case class OnlyMaven(library: Library, version: String) extends Difference {
      def render = s"- ${library.render}:$version"
    }
    case class OnlyBleep(library: Library, version: String) extends Difference {
      def render = s"+ ${library.render}:$version"
    }
  }

  case class Report(differences: SortedMap[model.CrossProjectName, List[Difference]], compared: Int) {
    def isEmpty: Boolean = differences.isEmpty

    /** the differences shared by most projects first */
    def mostCommon: List[(String, Int)] =
      differences.values.flatten.map(_.render).groupMapReduce(identity)(_ => 1)(_ + _).toList.sortBy { case (d, n) => (-n, d) }

    def render: String = {
      val header = List(
        "Where bleep resolves a library differently from maven (`mvn dependency:list`).",
        "Maven lets the version declared nearest a module win, bleep the highest version asked for anywhere.",
        "`~` another version, `-` only maven has it, `+` only bleep has it",
        "",
        s"compared: $compared projects, with differences: ${differences.size}"
      )
      val common = if (isEmpty) Nil else "" :: "most common (projects, difference):" :: mostCommon.map { case (d, n) => f"$n%5d  $d" }
      val perProject = differences.toList.flatMap { case (project, diffs) => "" :: project.value :: diffs.map(d => s"  ${d.render}") }
      (header ++ common ++ perProject).mkString("", "\n", "\n")
    }
  }

  /** maven's scopes on the classpath of a module's code. Its tests get `test` too */
  private val mainScopes = Set("compile", "provided", "runtime", "system")

  def apply(
      mavenProjects: List[MavenProject],
      mavenResolved: Map[String, List[MavenResolvedDependency]],
      /** the jars on each bleep project's classpath */
      bleepClasspaths: Map[model.CrossProjectName, List[Path]]
  ): Report = {
    val reactor: Set[(String, String)] = mavenProjects.map(p => (p.groupId, p.artifactId)).toSet

    val rows: List[(model.CrossProjectName, Map[Library, Set[String]], Map[Library, Set[String]])] =
      mavenProjects.filter(_.packaging != "pom").flatMap { module =>
        val resolvedForModule = mavenResolved.getOrElse(module.artifactId, Nil).filterNot(d => reactor((d.groupId, d.artifactId)))
        def maven(scopes: String => Boolean): Map[Library, Set[String]] =
          resolvedForModule.filter(d => scopes(d.scope)).groupMap(d => Library(d.artifactId, d.classifier))(_.version).map { case (k, v) => (k, v.toSet) }

        val name = buildFromMavenPom.projectNameFor(module)
        val main = model.CrossProjectName(name, None)
        val test = model.CrossProjectName(model.ProjectName(s"${name.value}-test"), None)
        List(
          bleepClasspaths.get(main).map(cp => (main, maven(mainScopes), bleep(cp))),
          bleepClasspaths.get(test).map(cp => (test, maven(scope => mainScopes(scope) || scope == "test"), bleep(cp)))
        ).flatten
      }

    val differences = rows.flatMap { case (project, maven, bleep) =>
      val diffs = (maven.keySet ++ bleep.keySet).toList.sortBy(_.render).flatMap { library =>
        (maven.get(library), bleep.get(library)) match {
          case (Some(m), Some(b)) if m == b => None
          case (Some(m), Some(b))           => Some(Difference.OtherVersion(library, m.toList.sorted.mkString(","), b.toList.sorted.mkString(",")))
          case (Some(m), None)              => Some(Difference.OnlyMaven(library, m.toList.sorted.mkString(",")))
          case (None, Some(b))              => Some(Difference.OnlyBleep(library, b.toList.sorted.mkString(",")))
          case (None, None)                 => None
        }
      }
      if (diffs.isEmpty) None else Some((project, diffs))
    }
    Report(SortedMap.from(differences), rows.size)
  }

  /** The libraries on a classpath, read from where coursier keeps them: `.../<artifact>/<version>/<artifact>-<version>[-<classifier>].jar`. What is not in that
    * layout, the classes of other projects say, is not a library
    */
  def bleep(classpath: List[Path]): Map[Library, Set[String]] =
    classpath
      .filter(_.getFileName.toString.endsWith(".jar"))
      .flatMap { jar =>
        for {
          versionDir <- Option(jar.getParent)
          artifactDir <- Option(versionDir.getParent)
          version = versionDir.getFileName.toString
          artifactId = artifactDir.getFileName.toString
          base = s"$artifactId-$version"
          file = jar.getFileName.toString.stripSuffix(".jar")
          if file.startsWith(base)
        } yield (Library(artifactId, file.drop(base.length).stripPrefix("-")), version)
      }
      .groupMap(_._1)(_._2)
      .map { case (k, v) => (k, v.toSet) }
}
