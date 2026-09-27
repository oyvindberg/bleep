package bleep.model

import coursier.core.{ModuleName, Organization}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class DepExclusionsTest extends AnyFunSuite with Matchers {
  test("every excluded module reaches coursier, also several from one organization") {
    val dep = Dep
      .Java("io.get-coursier", "interface", "1.0.29-M4")
      .withExclusions(Organization("org.scala-lang"), Set(ModuleName("scala-library"), ModuleName("scala3-library_3")))
      .withExclusions(Organization("org.slf4j"), Set(ModuleName("slf4j-api")))
    val excluded = dep.asDependency(VersionCombo.Java).toOption.get.minimizedExclusions
    List("scala-library", "scala3-library_3").foreach { name =>
      assert(!excluded(Organization("org.scala-lang"), ModuleName(name)), s"org.scala-lang:$name should be excluded")
    }
    assert(!excluded(Organization("org.slf4j"), ModuleName("slf4j-api")))
    assert(excluded(Organization("org.scala-lang"), ModuleName("scala-reflect")), "only what was excluded")
  }
}
