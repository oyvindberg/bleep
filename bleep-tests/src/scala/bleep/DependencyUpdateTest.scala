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
import bleep.rewrites.UpgradeDependencies.UpgradeLogger.Noop.upgraded

class DependencyUpdateTest extends AnyFunSuite with TripleEqualsSupport {

  test("parses single dependency") {
    val dependency = "org.http4s::http4s-core"
    DependencyUpgrader.parseSingleDep.parseAll(dependency) match {

      case Left(_) => assert(false)
      case Right(value) =>
        val org = value._1
        val module = value._2.getOrElse("")

        (org, module) shouldBe ("org.http4s", "http4s-core")
    }
  }

  test("parses single dependency - only organization") {
    val dependency = "org.http4s"
    DependencyUpgrader.parseSingleDep.parseAll(dependency) match {
      case Left(_) => assert(false)
      case Right(value) =>
        val org = value._1
        val module = value._2

        (org, module) shouldBe ("org.http4s", None)
    }
  }

  test("parses single dependency - only organization") {
    val dependency = "org.http4s"
    DependencyUpgrader.parseSingleDep.parseAll(dependency) match {
      case Left(_) => assert(false)
      case Right(value) =>
        val org = value._1
        val module = value._2

        (org, module) shouldBe ("org.http4s", None)
    }
  }

  test("find upgrades with no single dep defined") {

    val upgrades = DependencyUpgrader.runUpgrade(None, DependencyUpdateTestFixture.foundByDep, dummy)

    assert(upgrades.isRight)
  }

  test("find upgrades fails with improperly formated single dep") {

    val upgrades = DependencyUpgrader.runUpgrade(Some("this;;can't;;be;;right"), DependencyUpdateTestFixture.foundByDep, dummy)

    assert(upgrades.isLeft)
  }

  test("find upgrades with single dep") {

    val upgrades = DependencyUpgrader.runUpgrade(Some("org.http4s::http4s-dsl"), DependencyUpdateTestFixture.foundByDep, dummy)

    assert(upgrades.isRight)
  }

  test("find upgrades with single dep - org only") {

    val upgrades = DependencyUpgrader.runUpgrade(Some("org.http4s"), DependencyUpdateTestFixture.foundByDep, dummy)

    assert(upgrades.isRight)
  }

  def dummy(ignored: Map[UpgradeDependencies.ContextualDep, (Dependency, Versions)]): Either[BleepException, Unit] = Right(())
}

object DependencyUpdateTestFixture {
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

  val foundByDep = List(http4sDsl, http4sCore).toMap

}
