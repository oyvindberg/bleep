package bleep
package model

import io.circe.syntax.*
import org.scalactic.TripleEqualsSupport
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite

class JsonTest extends AnyFunSuite with TripleEqualsSupport {
  def roundtrip(vc: VersionCombo): Assertion = {
    val json = vc.asJson
    val vc2 = json.as[VersionCombo].orThrowWithError("Failed to parse json")
    assert(vc === vc2)
  }

  test("VersionCombo") {
    roundtrip(VersionCombo.Jvm(VersionScala.Scala3)).discard()
    roundtrip(VersionCombo.Js(VersionScala.Scala212, VersionScalaJs.ScalaJs1)).discard()
    roundtrip(VersionCombo.Native(VersionScala.Scala3, VersionScalaNative.ScalaNative04)).discard()
    roundtrip(VersionCombo.Java).discard()
  }
}
