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
    modules: List[String]
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
    configuration: scala.xml.NodeSeq
)

case class MavenRepository(
    id: String,
    url: String
)

/** A dependency as maven resolved it for a module (`mvn dependency:list`): direct or transitive, at the version and scope maven settled on */
case class MavenResolvedDependency(groupId: String, artifactId: String, tpe: String, classifier: String, version: String, scope: String)
