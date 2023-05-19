package bleep
package model

import org.scalactic.TripleEqualsSupport
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite
import io.circe.syntax.*

class JsonTest extends AnyFunSuite with TripleEqualsSupport {
  def roundtrip(vc: VersionCombo): Assertion = {
    val json = vc.asJson
    val vc2 = json.as[VersionCombo].orThrowWithError("Failed to parse json")
    assert(vc === vc2)
  }

  test("VersionCombo") {
    roundtrip(VersionCombo.Jvm(VersionScala.Scala3))
    roundtrip(VersionCombo.Js(VersionScala.Scala212, VersionScalaJs.ScalaJs1))
    roundtrip(VersionCombo.Native(VersionScala.Scala3, VersionScalaNative.ScalaNative04))
    roundtrip(VersionCombo.Java)
  }
}
