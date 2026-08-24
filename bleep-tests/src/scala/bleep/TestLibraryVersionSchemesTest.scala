package bleep

import bleep.model.{LibraryVersionScheme, VersionCombo, VersionScala, VersionScalaJs, VersionScalaNative}
import io.circe.syntax.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** [[VersionCombo.testLibraryVersionSchemes]] injects schemes the user never wrote, so nothing in a build file constrains their shape. These pin it.
  *
  * The shape is easy to get wrong because a [[LibraryVersionScheme]] keeps its scheme in the dep's *version* slot — `org::name:early-semver` is how one is
  * spelled — so a scheme built from a real library dep carries a version where the scheme belongs. That resolves fine and then fails to decode out of the
  * resolution cache, which is a long way from the mistake.
  */
class TestLibraryVersionSchemesTest extends AnyFunSuite with Matchers {

  private val js: VersionCombo = VersionCombo.Js(VersionScala.Scala3, VersionScalaJs(model.Versions.ScalaJs1))
  private val native: VersionCombo = VersionCombo.Native(VersionScala.Scala3, VersionScalaNative(model.Versions.ScalaNative05))
  private val jvm: VersionCombo = VersionCombo.Jvm(VersionScala.Scala3)

  private val platformCombos = List(js, native)

  test("the scheme field and the dep's version say the same thing") {
    platformCombos.flatMap(_.testLibraryVersionSchemes(isTest = true)).foreach { lvs =>
      withClue(s"${lvs.dep.repr}: ") {
        lvs.dep.version shouldBe lvs.scheme.value
      }
    }
  }

  test("every injected scheme survives a round-trip through its own codec") {
    // This is the assertion that would have caught the bug: the value resolves either way, but only a well-formed one comes back out of the resolution cache.
    platformCombos.flatMap(_.testLibraryVersionSchemes(isTest = true)).foreach { lvs =>
      withClue(s"${lvs.dep.repr}: ") {
        lvs.asJson.as[LibraryVersionScheme] shouldBe Right(lvs)
      }
    }
  }

  test("the scheme names the module, never a version of it") {
    val reprs = platformCombos.flatMap(_.testLibraryVersionSchemes(isTest = true)).map(_.dep.repr)
    reprs should not be empty
    reprs.foreach(r => r should endWith(":early-semver"))
  }

  test("non-test projects get none, and the JVM has no platform test libraries to pin") {
    platformCombos.foreach(_.testLibraryVersionSchemes(isTest = false) shouldBe Nil)
    jvm.testLibraryVersionSchemes(isTest = true) shouldBe Nil
  }
}
