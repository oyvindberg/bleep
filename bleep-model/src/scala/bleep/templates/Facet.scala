package bleep
package templates

/** What a setting is about.
  *
  * A template should be about one thing. A template for a group of projects the build already has a name for (all tests, the Scala 2.13 variants) is about that
  * group and may hold anything they share. For any other group of projects the template is named after what it holds, so it holds settings of one facet only: a
  * compiler setup, a set of dependencies, publishing coordinates. Settings which merely happen to be shared by the same projects are not bundled.
  */
sealed abstract class Facet(val name: String)

object Facet {
  case object Compiler extends Facet("compiler")
  case object Platform extends Facet("platform")
  case object Dependencies extends Facet("dependencies")
  case object Layout extends Facet("layout")
  case object Testing extends Facet("testing")
  case object Publishing extends Facet("publishing")

  val All: List[Facet] = List(Compiler, Platform, Dependencies, Layout, Testing, Publishing)

  /** by the path of a setting, see [[mineTemplates.Setting]] */
  def of(settingPath: String): Facet =
    settingPath.takeWhile(_ != '.') match {
      case "scala" | "java" | "kotlin" | "postCompile"                                                        => Compiler
      case "platform"                                                                                         => Platform
      case "dependencies" | "boms" | "libraryVersionSchemes" | "ignoreEvictionErrors" | "jars"                => Dependencies
      case "sources" | "resources" | "source-layout" | "sourcegen"                                            => Layout
      case "isTestProject" | "testFrameworks" | "testTags" | "maxConcurrentSuites" | "testFork" | "sbt-scope" => Testing
      case "publish" | "stamp"                                                                                => Publishing
      case other => throw new BleepException.Text(s"Template inference does not know what the setting `$other` is about")
    }

  def of(p: model.Project): Set[Facet] =
    mineTemplates.settings(p).map(s => of(s.path))

  /** Only the settings of `p` which are about `facet` */
  def restrict(p: model.Project, facet: Facet): model.Project = {
    val e = model.Project.empty
    facet match {
      case Compiler     => e.copy(java = p.java, scala = p.scala, kotlin = p.kotlin, postCompile = p.postCompile)
      case Platform     => e.copy(platform = p.platform)
      case Dependencies =>
        e.copy(
          dependencies = p.dependencies,
          boms = p.boms,
          libraryVersionSchemes = p.libraryVersionSchemes,
          ignoreEvictionErrors = p.ignoreEvictionErrors,
          jars = p.jars
        )
      case Layout  => e.copy(sources = p.sources, resources = p.resources, `source-layout` = p.`source-layout`, sourcegen = p.sourcegen)
      case Testing =>
        e.copy(
          isTestProject = p.isTestProject,
          testFrameworks = p.testFrameworks,
          testTags = p.testTags,
          maxConcurrentSuites = p.maxConcurrentSuites,
          testFork = p.testFork,
          `sbt-scope` = p.`sbt-scope`
        )
      case Publishing => e.copy(publish = p.publish, stamp = p.stamp)
    }
  }
}
