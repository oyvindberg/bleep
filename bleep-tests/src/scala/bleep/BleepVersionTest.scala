package bleep

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.util.Properties
import scala.util.Using

/** `BleepVersion.current` is read from bleep-model's `dynver` stamp rather than a generated constant. Nearly every test in this project bootstraps a build and
  * reads it, so if the stamp were missing from the classpath they would all fail — this one exists so that such a failure names its cause.
  */
class BleepVersionTest extends AnyFunSuite with Matchers {

  test("the version comes from bleep-model's dynver stamp, which is on this classpath") {
    val resource = "/" + StampFile.resourcePath(model.CrossProjectName(model.ProjectName("bleep-model"), None))
    val fromStamp = Using.resource(Option(getClass.getResourceAsStream(resource)).getOrElse(fail(s"$resource is not on the test classpath"))) { in =>
      val p = new Properties()
      p.load(in)
      p.getProperty("dynver")
    }

    model.BleepVersion.current.value shouldBe fromStamp
  }

  test("and it is a real version, not a placeholder") {
    model.BleepVersion.current.value should fullyMatch regex """\d+\.\d+\.\d+(-M\d+)?(\+.*)?"""
    model.BleepVersion.current should not be model.BleepVersion.dev
  }

  test("bleepscript reads its own version from its own stamp, and it agrees with bleep's") {
    // Both are the same derivation over the same commit, so a published pair can never disagree — the thing the generated constant claimed and did not deliver
    // under `bleep publish --version`.
    bleepscript.BleepscriptVersion.value() shouldBe model.BleepVersion.current.value
  }
}
