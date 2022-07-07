package bleep.tasks.publishing

import bleep.PathOps
import bleep.testing.SnapshotTest
import coursier.core._

class PublishSnapshotTests extends SnapshotTest {

  test("maven") {
    val self = Dependency(Module(Organization("com.org"), ModuleName("moduleName_2.13"), Map.empty), "1.0.0")
    val deps = List(
      Dependency(Module(Organization("com.org"), ModuleName("moduleName_2.13"), Map.empty), "1.0.0")
    )
    val info = Info(
      description = "description",
      homePage = "homepage",
      developers = List(Info.Developer("flaff", "devName", "devUrl")),
      publication = Some(Versions.DateTime(2022, 1, 1, 12, 1, 1)),
      scm = Some(value = Info.Scm(Some("http://url"), Some("http://connection"), Some("http://devConnection"))),
      licenseInfo = List(
        Info.License(name = "license name", url = Some("http://licenseUri"), distribution = Some("distribution"), comments = Some("comments"))
      )
    )
    val path = outFolder / "publish" / "pom.xml"
    val string = new String(GenLayout.fromXml(GenLayout.pomFile(self, deps, info)))
    writeAndCompare(path, Map(path -> string))
  }
}
