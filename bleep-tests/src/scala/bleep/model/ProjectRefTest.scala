package bleep.model

import bleep.BleepException
import io.circe.Json
import io.circe.syntax.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.SortedSet

class ProjectRefTest extends AnyFunSuite with Matchers {
  private def cross(name: String, crossId: String): CrossProjectName = CrossProjectName(ProjectName(name), Some(CrossId(crossId)))
  private def plain(name: String): CrossProjectName = CrossProjectName(ProjectName(name), None)

  private def build(projects: (CrossProjectName, Project)*): Build.Exploded =
    Build.Exploded(
      $version = BleepVersion("1.0.0-M9"),
      explodedProjects = projects.toMap,
      resolvers = JsonList.empty,
      jvm = None,
      scripts = Map.empty,
      remoteCache = None
    )

  private def dependsOn(refs: String*): Project =
    Project.empty.copy(dependsOn = JsonSet(SortedSet.from(refs.map(r => ProjectRef.fromString(r).get))))

  private def platform(id: PlatformId): Project =
    Project.empty.copy(platform = Some(Platform.Jvm(Options.empty, None, Options.empty).copy(name = Some(id))))

  test("`name@crossId` reads as that cross version, `name` as a name alone, and both write back as they were read") {
    Json.fromString("lib@b").as[ProjectRef] shouldBe Right(ProjectRef(ProjectName("lib"), Some(CrossId("b"))))
    Json.fromString("lib").as[ProjectRef] shouldBe Right(ProjectRef(ProjectName("lib"), None))
    ProjectRef(ProjectName("lib"), Some(CrossId("b"))).asJson shouldBe Json.fromString("lib@b")
    ProjectRef(ProjectName("lib"), None).asJson shouldBe Json.fromString("lib")
    Json.fromString("a@b@c").as[ProjectRef].isLeft shouldBe true
  }

  test("a stated cross id is the cross version depended on, where inference could not decide") {
    // a Scala.js project on one of two JVM cross versions: no cross id in common, and no version of the same platform
    val b = build(
      cross("lib", "one") -> platform(PlatformId.Jvm),
      cross("lib", "two") -> platform(PlatformId.Jvm),
      plain("app") -> platform(PlatformId.Js).copy(dependsOn = dependsOn("lib@two").dependsOn)
    )
    b.resolvedDependsOn(plain("app")) shouldBe SortedSet(cross("lib", "two"))
  }

  test("without the cross id, the same build cannot decide, and says so") {
    val b = build(
      cross("lib", "one") -> platform(PlatformId.Jvm),
      cross("lib", "two") -> platform(PlatformId.Jvm),
      plain("app") -> platform(PlatformId.Js).copy(dependsOn = dependsOn("lib").dependsOn)
    )
    intercept[BleepException.Text](b.resolvedDependsOn).getMessage should include("Couldn't figure out which of")
  }

  test("a stated cross id overrides the matching one inference would pick") {
    val b = build(
      cross("lib", "one") -> Project.empty,
      cross("lib", "two") -> Project.empty,
      cross("app", "one") -> dependsOn("lib@two")
    )
    b.resolvedDependsOn(cross("app", "one")) shouldBe SortedSet(cross("lib", "two"))
  }

  test("a bare name still follows the cross id") {
    val b = build(
      cross("lib", "one") -> Project.empty,
      cross("lib", "two") -> Project.empty,
      cross("app", "one") -> dependsOn("lib"),
      cross("app", "two") -> dependsOn("lib")
    )
    b.resolvedDependsOn(cross("app", "one")) shouldBe SortedSet(cross("lib", "one"))
    b.resolvedDependsOn(cross("app", "two")) shouldBe SortedSet(cross("lib", "two"))
  }

  test("a stated cross version that does not exist fails, naming it; there is no fallback") {
    val b = build(cross("lib", "one") -> Project.empty, plain("app") -> dependsOn("lib@three"))
    intercept[BleepException.Text](b.resolvedDependsOn).getMessage should include("depends on non-existing project lib@three")
  }
}
