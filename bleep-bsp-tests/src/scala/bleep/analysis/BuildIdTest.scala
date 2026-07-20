package bleep.analysis

import bleep.bsp.BuildId
import io.circe.{Json, Printer}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** `BuildId` is what lets the server tell "the same build again" from "a different build for this workspace". Both directions have to hold, and the hash has to
  * be stable across processes — the client computes it, the server compares it.
  */
class BuildIdTest extends AnyFunSuite with Matchers {

  test("same content hashes the same") {
    BuildId.ofJson(Json.obj("a" -> Json.fromString("1"))) shouldBe BuildId.ofJson(Json.obj("a" -> Json.fromString("1")))
  }

  test("different content hashes differently") {
    BuildId.ofJson(Json.obj("a" -> Json.fromString("1"))) should not be BuildId.ofJson(Json.obj("a" -> Json.fromString("2")))
  }

  test("key order does not affect the hash") {
    // The client and the server encode the same build in two different processes. If the hash
    // depended on map iteration or field order, identical builds would look different and every
    // connection would re-resolve.
    val one = Json.obj("a" -> Json.fromInt(1), "b" -> Json.fromInt(2))
    val other = Json.obj("b" -> Json.fromInt(2), "a" -> Json.fromInt(1))

    // circe compares objects by content, but *prints* them in insertion order — so without sorted
    // keys the hash would see two different strings for the same build.
    Printer.noSpaces.print(one) should not be Printer.noSpaces.print(other)
    BuildId.ofJson(one) shouldBe BuildId.ofJson(other)
  }

  test("nested key order does not affect the hash either") {
    val one = Json.obj("outer" -> Json.obj("a" -> Json.fromInt(1), "b" -> Json.fromInt(2)))
    val other = Json.obj("outer" -> Json.obj("b" -> Json.fromInt(2), "a" -> Json.fromInt(1)))

    BuildId.ofJson(one) shouldBe BuildId.ofJson(other)
  }

  test("short is a prefix of the full hash") {
    val id = BuildId.ofJson(Json.obj("a" -> Json.fromString("1")))
    id.value should startWith(id.short)
    id.short.length shouldBe 12
  }
}
