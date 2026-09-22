package bleep

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class StampFileTest extends AnyFunSuite with Matchers {

  private def cpn(name: String, crossId: Option[String]): model.CrossProjectName =
    model.CrossProjectName(model.ProjectName(name), crossId.map(model.CrossId.apply))

  test("named by the cross project, so every variant has its own") {
    StampFile.resourcePath(cpn("lib", Some("jvm213"))) shouldBe "bleep-stamp/lib@jvm213.properties"
    StampFile.resourcePath(cpn("lib", Some("jvm3"))) shouldBe "bleep-stamp/lib@jvm3.properties"
    StampFile.resourcePath(cpn("lib", None)) shouldBe "bleep-stamp/lib.properties"
  }

  test("a slash in a project name does not nest directories") {
    StampFile.resourcePath(cpn("dlab/version", None)) shouldBe "bleep-stamp/dlab-version.properties"
  }
}
