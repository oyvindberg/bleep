package bleep

import coursier.core.{ModuleName, Organization}
import org.scalatest.funsuite.AnyFunSuite

class DepExclusionsTest extends AnyFunSuite {
  test("every excluded module reaches coursier, also several from one organization") {
    val org = Organization("org.scala-sbt")
    val excluded = List("util-logging_2.12", "util-control_2.12", "util-tracking_2.12").map(ModuleName.apply)
    val dep = model.Dep
      .Java("org.scala-sbt", "zinc_2.12", "1.10.8")
      .copy(exclusions = model.JsonMap(Map(org -> model.JsonSet.fromIterable(excluded))))

    val coursierExclusions = dep.dependency.minimizedExclusions.toSet()
    assert(coursierExclusions == excluded.map(name => (org, name)).toSet, coursierExclusions.toString)
  }
}
