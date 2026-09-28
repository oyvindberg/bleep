package bleep
package mavenimport

import java.nio.file.Path

case class MavenProject(
    groupId: String,
    artifactId: String,
    version: String,
    packaging: String,
    directory: Path,
    sourceDirectory: Path,
    testSourceDirectory: Path,
    additionalSources: List[Path],
    additionalTestSources: List[Path],
    resources: List[Path],
    testResources: List[Path],
    dependencies: List[MavenDependency],
    /** `<dependencyManagement>` of the effective pom: the module's own entries, those it inherits, and those of the BOMs it imports, all with versions */
    dependencyManagement: List[MavenDependency],
    plugins: List[MavenPlugin],
    repositories: List[MavenRepository],
    modules: List[String],
    /** `<properties>` of the effective pom, the module's own and those it inherits. Plugins read some of them as defaults: `maven.compiler.release`, say */
    properties: Map[String, String]
)

/** @param tpe
  *   maven's `<type>`, `jar` unless stated. `test-jar` is a module's tests
  * @param classifier
  *   maven's `<classifier>`, empty unless stated
  */
case class MavenDependency(
    groupId: String,
    artifactId: String,
    version: String,
    scope: String,
    optional: Boolean,
    exclusions: List[MavenExclusion],
    tpe: String,
    classifier: String
) {

  /** A module's tests, which maven publishes as a jar with classifier `tests` */
  def isTests: Boolean = tpe == "test-jar" || classifier == "tests"
}

case class MavenExclusion(
    groupId: String,
    artifactId: String
)

case class MavenPlugin(
    groupId: String,
    artifactId: String,
    version: String,
    configuration: scala.xml.NodeSeq,
    executions: List[MavenExecution]
)

/** An `<execution>` of a plugin, with configuration of its own which wins over the plugin's
  *
  * @param phase
  *   empty unless stated. `none` turns the execution off, which is how a build replaces a default execution with its own
  */
case class MavenExecution(id: String, phase: String, goals: List[String], configuration: scala.xml.NodeSeq) {
  def isEnabled: Boolean = phase != "none"
}

case class MavenRepository(
    id: String,
    url: String
)

/** A dependency as maven resolved it for a module (`mvn dependency:list`): direct or transitive, at the version and scope maven settled on */
case class MavenResolvedDependency(groupId: String, artifactId: String, tpe: String, classifier: String, version: String, scope: String)
