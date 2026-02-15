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
    resources: List[Path],
    testResources: List[Path],
    dependencies: List[MavenDependency],
    plugins: List[MavenPlugin],
    repositories: List[MavenRepository],
    modules: List[String]
)

case class MavenDependency(
    groupId: String,
    artifactId: String,
    version: String,
    scope: String,
    optional: Boolean,
    exclusions: List[MavenExclusion]
)

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
