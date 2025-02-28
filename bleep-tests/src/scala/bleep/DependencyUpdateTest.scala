package bleep

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._
import bleep.commands.DependencyUpgrader
import org.scalactic.TripleEqualsSupport
import coursier.core.Dependency
import coursier.core.Organization
import coursier.core.ModuleName
import bleep.model.Dep
import bleep.model.VersionCombo
import bleep.model.VersionScala
import coursier.core.Versions
import coursier.core.Versions.DateTime
import bleep.rewrites.UpgradeDependencies

class DependencyUpdateTest extends AnyFunSuite with TripleEqualsSupport {

  test("parses single dependency") {
    val dependency = "org.http4s::http4s-core"
    DependencyUpgrader.singleDepParser.parseAll(dependency) match {

      case Left(_) => assert(false)
      case Right(value) =>
        val org = value._1
        val module = value._2.getOrElse("")

        (org, module) shouldBe ("org.http4s", "http4s-core")
    }
  }

  test("parses single java dependency") {
    val dependency = "org.springframework:spring-boot-starter-web"
    DependencyUpgrader.singleDepParser.parseAll(dependency) match {

      case Left(_) => assert(false)
      case Right(value) =>
        val org = value._1
        val module = value._2.getOrElse("")

        (org, module) shouldBe ("org.springframework", "spring-boot-starter-web")
    }
  }
  test("parses single dependency - only organization") {
    val dependency = "org.http4s"
    DependencyUpgrader.singleDepParser.parseAll(dependency) match {
      case Left(_) => assert(false)
      case Right(value) =>
        val org = value._1
        val module = value._2

        (org, module) shouldBe ("org.http4s", None)
    }
  }

  test("find upgrades with no single dep defined") {

    val upgrades = DependencyUpgrader.depsToUpgrade(None, DependencyUpdateTestFixture.foundByDep, false, false)

    upgrades match {
      case Left(_)      => assert(false)
      case Right(value) => assert(value.size === 4)
    }

  }

  test("find upgrades with single dep") {

    val upgrades = DependencyUpgrader.depsToUpgrade(Some("org.http4s::http4s-dsl"), DependencyUpdateTestFixture.foundByDep, false, false)

    upgrades match {
      case Left(_) => assert(false)
      case Right(value) =>
        val moduleName = value.head._2.baseModuleName.value
        (value.size, moduleName) shouldBe (1, "http4s-dsl")
    }
  }

  test("find upgrades with single dep - org only") {
    val upgrades = DependencyUpgrader.depsToUpgrade(Some("org.http4s"), DependencyUpdateTestFixture.foundByDep, false, false)

    upgrades match {
      case Left(_) => assert(false)
      case Right(value) =>
        val modules = List("http4s-dsl", "http4s-core")
        assert(value.values.map(_.baseModuleName.value) === modules)
    }
  }

  test("find upgrades with no matching deps gives error") {
    val upgrades = DependencyUpgrader.depsToUpgrade(Some("org.nottoday"), DependencyUpdateTestFixture.foundByDep, false, false)

    assert(upgrades.isLeft)
  }

  test("correctly update to newest non-prerelease") {

    val upgrades = DependencyUpgrader.depsToUpgrade(Some("org.http4s::http4s-dsl"), DependencyUpdateTestFixture.foundByDep, false, false)

    upgrades match {
      case Left(_) => assert(false)
      case Right(value) =>
        val version = value.head._2.version
        assert(version === "0.23.28")
    }
  }

  test("correctly updates java-dep") {

    val upgrades = DependencyUpgrader.depsToUpgrade(Some("org.springframework:spring-boot-starter-web"), DependencyUpdateTestFixture.foundByDep, false, false)

    upgrades match {
      case Left(_) => assert(false)
      case Right(value) =>
        val version = value.head._2.version
        assert(version === "3.3.4")
    }
  }
  test("correctly update to newest prerelease") {

    val upgrades = DependencyUpgrader.depsToUpgrade(Some("org.http4s::http4s-dsl"), DependencyUpdateTestFixture.foundByDep, false, true)

    upgrades match {
      case Left(_) => assert(false)
      case Right(value) =>
        val version = value.head._2.version
        assert(version === "1.0.0-M41")
    }
  }
}

object DependencyUpdateTestFixture {
  val springVersions = Versions(
    "3.3.4",
    "3.3.4",
    List(
      "1.0.0.RELEASE",
      "1.0.1.RELEASE",
      "1.0.2.RELEASE",
      "1.1.0.RELEASE",
      "1.1.1.RELEASE",
      "1.1.2.RELEASE",
      "1.1.3.RELEASE",
      "1.1.4.RELEASE",
      "1.1.5.RELEASE",
      "1.1.6.RELEASE",
      "1.1.7.RELEASE",
      "1.1.8.RELEASE",
      "1.1.9.RELEASE",
      "1.1.10.RELEASE",
      "1.1.11.RELEASE",
      "1.1.12.RELEASE",
      "1.2.0.RELEASE",
      "1.2.1.RELEASE",
      "1.2.2.RELEASE",
      "1.2.3.RELEASE",
      "1.2.4.RELEASE",
      "1.2.5.RELEASE",
      "1.2.6.RELEASE",
      "1.2.7.RELEASE",
      "1.2.8.RELEASE",
      "1.3.0.RELEASE",
      "1.3.1.RELEASE",
      "1.3.2.RELEASE",
      "1.3.3.RELEASE",
      "1.3.4.RELEASE",
      "1.3.5.RELEASE",
      "1.3.6.RELEASE",
      "1.3.7.RELEASE",
      "1.3.8.RELEASE",
      "1.4.0.RELEASE",
      "1.4.1.RELEASE",
      "1.4.2.RELEASE",
      "1.4.3.RELEASE",
      "1.4.4.RELEASE",
      "1.4.5.RELEASE",
      "1.4.6.RELEASE",
      "1.4.7.RELEASE",
      "1.5.0.RELEASE",
      "1.5.1.RELEASE",
      "1.5.2.RELEASE",
      "1.5.3.RELEASE",
      "1.5.4.RELEASE",
      "1.5.5.RELEASE",
      "1.5.6.RELEASE",
      "1.5.7.RELEASE",
      "1.5.8.RELEASE",
      "1.5.9.RELEASE",
      "1.5.10.RELEASE",
      "1.5.11.RELEASE",
      "1.5.12.RELEASE",
      "1.5.13.RELEASE",
      "1.5.14.RELEASE",
      "1.5.15.RELEASE",
      "1.5.16.RELEASE",
      "1.5.17.RELEASE",
      "1.5.18.RELEASE",
      "1.5.19.RELEASE",
      "1.5.20.RELEASE",
      "1.5.21.RELEASE",
      "1.5.22.RELEASE",
      "2.0.0.RELEASE",
      "2.0.1.RELEASE",
      "2.0.2.RELEASE",
      "2.0.3.RELEASE",
      "2.0.4.RELEASE",
      "2.0.5.RELEASE",
      "2.0.6.RELEASE",
      "2.0.7.RELEASE",
      "2.0.8.RELEASE",
      "2.0.9.RELEASE",
      "2.1.0.RELEASE",
      "2.1.1.RELEASE",
      "2.1.2.RELEASE",
      "2.1.3.RELEASE",
      "2.1.4.RELEASE",
      "2.1.5.RELEASE",
      "2.1.6.RELEASE",
      "2.1.7.RELEASE",
      "2.1.8.RELEASE",
      "2.1.9.RELEASE",
      "2.1.10.RELEASE",
      "2.1.11.RELEASE",
      "2.1.12.RELEASE",
      "2.1.13.RELEASE",
      "2.1.14.RELEASE",
      "2.1.15.RELEASE",
      "2.1.16.RELEASE",
      "2.1.17.RELEASE",
      "2.1.18.RELEASE",
      "2.2.0.RELEASE",
      "2.2.1.RELEASE",
      "2.2.2.RELEASE",
      "2.2.3.RELEASE",
      "2.2.4.RELEASE",
      "2.2.5.RELEASE",
      "2.2.6.RELEASE",
      "2.2.7.RELEASE",
      "2.2.8.RELEASE",
      "2.2.9.RELEASE",
      "2.2.10.RELEASE",
      "2.2.11.RELEASE",
      "2.2.12.RELEASE",
      "2.2.13.RELEASE",
      "2.3.0.RELEASE",
      "2.3.1.RELEASE",
      "2.3.2.RELEASE",
      "2.3.3.RELEASE",
      "2.3.4.RELEASE",
      "2.3.5.RELEASE",
      "2.3.6.RELEASE",
      "2.3.7.RELEASE",
      "2.3.8.RELEASE",
      "2.3.9.RELEASE",
      "2.3.10.RELEASE",
      "2.3.11.RELEASE",
      "2.3.12.RELEASE",
      "2.4.0",
      "2.4.1",
      "2.4.2",
      "2.4.3",
      "2.4.4",
      "2.4.5",
      "2.4.6",
      "2.4.7",
      "2.4.8",
      "2.4.9",
      "2.4.10",
      "2.4.11",
      "2.4.12",
      "2.4.13",
      "2.5.0",
      "2.5.1",
      "2.5.2",
      "2.5.3",
      "2.5.4",
      "2.5.5",
      "2.5.6",
      "2.5.7",
      "2.5.8",
      "2.5.9",
      "2.5.10",
      "2.5.11",
      "2.5.12",
      "2.5.13",
      "2.5.14",
      "2.5.15",
      "2.6.0",
      "2.6.1",
      "2.6.2",
      "2.6.3",
      "2.6.4",
      "2.6.5",
      "2.6.6",
      "2.6.7",
      "2.6.8",
      "2.6.9",
      "2.6.10",
      "2.6.11",
      "2.6.12",
      "2.6.13",
      "2.6.14",
      "2.6.15",
      "2.7.0",
      "2.7.1",
      "2.7.2",
      "2.7.3",
      "2.7.4",
      "2.7.5",
      "2.7.6",
      "2.7.7",
      "2.7.8",
      "2.7.9",
      "2.7.10",
      "2.7.11",
      "2.7.12",
      "2.7.13",
      "2.7.14",
      "2.7.15",
      "2.7.16",
      "2.7.17",
      "2.7.18",
      "3.0.0",
      "3.0.1",
      "3.0.2",
      "3.0.3",
      "3.0.4",
      "3.0.5",
      "3.0.6",
      "3.0.7",
      "3.0.8",
      "3.0.9",
      "3.0.10",
      "3.0.11",
      "3.0.12",
      "3.0.13",
      "3.1.0",
      "3.1.1",
      "3.1.2",
      "3.1.3",
      "3.1.4",
      "3.1.5",
      "3.1.6",
      "3.1.7",
      "3.1.8",
      "3.1.9",
      "3.1.10",
      "3.1.11",
      "3.1.12",
      "3.2.0",
      "3.2.1",
      "3.2.2",
      "3.2.3",
      "3.2.4",
      "3.2.5",
      "3.2.6",
      "3.2.7",
      "3.2.8",
      "3.2.9",
      "3.2.10",
      "3.3.0",
      "3.3.1",
      "3.3.2",
      "3.3.3",
      "3.3.4"
    ),
    Some(DateTime(2024, 9, 19, 10, 54, 26))
  )

  val http4sVersions = Versions(
    "1.0-234-d1a2b53",
    "1.0-234-d1a2b53",
    List(
      "0.10.0-M10",
      "0.21.0-M1",
      "0.21.0-M2",
      "0.21.0-M3",
      "0.21.0-M4",
      "0.21.0-M5",
      "0.21.0-M6",
      "0.21.0-RC1",
      "0.21.0-RC2",
      "0.21.0-RC3",
      "0.21.0-RC4",
      "0.21.0-RC5",
      "0.21.0",
      "0.21.1",
      "0.21.2",
      "0.21.3",
      "0.21.4",
      "0.21.5",
      "0.21.6",
      "0.21.7",
      "0.21.8",
      "0.21.9",
      "0.21.11",
      "0.21.12",
      "0.21.13",
      "0.21.14",
      "0.21.15",
      "0.21.16",
      "0.21.17",
      "0.21.18",
      "0.21.19",
      "0.21.20",
      "0.21.21",
      "0.21.22",
      "0.21.23",
      "0.21.24",
      "0.21.25",
      "0.21.26",
      "0.21.27",
      "0.21.28",
      "0.21.29",
      "0.21.30",
      "0.21.31",
      "0.21.33",
      "0.21.34",
      "0.22.0-M1",
      "0.22.0-M2",
      "0.22.0-M3",
      "0.22.0-M4",
      "0.22.0-M5",
      "0.22.0-M6",
      "0.22.0-M7",
      "0.22.0-M8",
      "0.22.0-RC1",
      "0.22.0",
      "0.22.1",
      "0.22.2",
      "0.22.3",
      "0.22.4",
      "0.22.5",
      "0.22.6",
      "0.22.7",
      "0.22.8",
      "0.22.9",
      "0.22.10",
      "0.22.11",
      "0.22.12",
      "0.22.13",
      "0.22.14",
      "0.22.15",
      "0.22-53-01128f5",
      "0.22-96-55d3184",
      "0.22-129-24d065b",
      "0.22-143-49b5a8d",
      "0.23.0-M1",
      "0.23.0-RC1",
      "0.23.0",
      "0.23.1",
      "0.23.2",
      "0.23.3",
      "0.23.4",
      "0.23.5",
      "0.23.6",
      "0.23.7",
      "0.23.8",
      "0.23.9",
      "0.23.10",
      "0.23.11",
      "0.23.12",
      "0.23.13",
      "0.23.14",
      "0.23.15",
      "0.23.16",
      "0.23.17",
      "0.23.18",
      "0.23.19-RC1",
      "0.23.19-RC2",
      "0.23.19-RC3",
      "0.23.19",
      "0.23.20",
      "0.23.21",
      "0.23.22",
      "0.23.23",
      "0.23.24",
      "0.23.25",
      "0.23.26",
      "0.23.27",
      "0.23.28",
      "1.0.0-M2",
      "1.0.0-M3",
      "1.0.0-M4",
      "1.0.0-M5",
      "1.0.0-M6",
      "1.0.0-M7",
      "1.0.0-M8",
      "1.0.0-M9",
      "1.0.0-M10",
      "1.0.0-M11",
      "1.0.0-M13",
      "1.0.0-M14",
      "1.0.0-M15",
      "1.0.0-M16",
      "1.0.0-M17",
      "1.0.0-M18",
      "1.0.0-M19",
      "1.0.0-M20",
      "1.0.0-M21",
      "1.0.0-M22",
      "1.0.0-M23",
      "1.0.0-M24",
      "1.0.0-M25",
      "1.0.0-M27",
      "1.0.0-M28",
      "1.0.0-M29",
      "1.0.0-M30",
      "1.0.0-M31",
      "1.0.0-M32",
      "1.0.0-M33",
      "1.0.0-M34",
      "1.0.0-M35",
      "1.0.0-M36",
      "1.0.0-M37",
      "1.0.0-M38",
      "1.0.0-M39",
      "1.0.0-M40",
      "1.0.0-M41",
      "1.0-2-1e49ccf",
      "1.0-2-79831c5",
      "1.0-4-a7fc4a9",
      "1.0-6-e439945",
      "1.0-10-df008e1",
      "1.0-12-e6a1f1f",
      "1.0-14-cdeb1e3",
      "1.0-14-7c5ec00",
      "1.0-16-b673b49",
      "1.0-18-a497d1a",
      "1.0-18-659c58b",
      "1.0-21-a0087dd",
      "1.0-23-f99b0af",
      "1.0-25-f1a4ab0",
      "1.0-37-9661f07",
      "1.0-39-5736fdc",
      "1.0-57-0368053",
      "1.0-81-ce14ddb",
      "1.0-103-5f6e845",
      "1.0-107-6676c1e",
      "1.0-231-e9b2b41",
      "1.0-232-85dadc2",
      "1.0-234-d1a2b53"
    ),
    Some(DateTime(2024, 9, 9, 18, 53, 55))
  )
  val http4sCore: (UpgradeDependencies.ContextualDep, (Dependency, Versions)) = (
    (Dep.Scala("org.http4s", "http4s-core", "0.21.0"), VersionCombo.Jvm(VersionScala("2.13.12"))),
    (Dependency(coursier.Module(Organization("org.http4s"), ModuleName("http4s-core"), Map.empty), "0.21.0"), http4sVersions)
  )
  val http4sDsl: (UpgradeDependencies.ContextualDep, (Dependency, Versions)) = (
    (Dep.Scala("org.http4s", "http4s-dsl", "0.21.0"), VersionCombo.Jvm(VersionScala("2.13.12"))),
    (Dependency(coursier.Module(Organization("org.http4s"), ModuleName("http4s-dsl"), Map.empty), "0.21.0"), http4sVersions)
  )

  val someOtherDep: (UpgradeDependencies.ContextualDep, (Dependency, Versions)) = (
    (Dep.Scala("org.other", "http4s-dsl", "0.21.0"), VersionCombo.Jvm(VersionScala("2.13.12"))),
    (Dependency(coursier.Module(Organization("org.other"), ModuleName("http4s-dsl"), Map.empty), "0.21.0"), http4sVersions)
  )

  val springBootWeb: (UpgradeDependencies.ContextualDep, (Dependency, Versions)) = (
    (Dep.Scala("org.springframework", "spring-boot-starter-web", "3.0.0"), VersionCombo.Jvm(VersionScala("2.13.12"))),
    (Dependency(coursier.Module(Organization("org.springframework"), ModuleName("spring-boot-starter-web"), Map.empty), "3.0.0"), springVersions)
  )
  val foundByDep = List(http4sDsl, http4sCore, someOtherDep, springBootWeb).toMap

}
