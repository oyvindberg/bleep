package bleep

import bleep.plugin.sonatype.sonatype.SonatypeCentralClient
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** What a failed Central deployment says went wrong, from the status response's `errors`.
  *
  * 1.0.0-M15's first release attempt failed with only "Current deployment state: FAILED": the client library decodes the status without `errors`, and finding
  * the one bad signature took a screenshot of Central's web UI.
  */
class SonatypeCentralErrorsTest extends AnyFunSuite with Matchers {

  test("each component is listed with the messages it failed validation with") {
    val status = ujson.read(
      """{
        |  "deploymentId": "13ba96ed-8065-4eca-92fa-b7e1c611ff2b",
        |  "deploymentName": "build.bleep.build.bleep-1.0.0-M15",
        |  "deploymentState": "FAILED",
        |  "purls": [],
        |  "errors": {
        |    "pkg:maven/build.bleep/bleep-plugin-native-image_3@1.0.0-M15": [
        |      "Invalid signature for file: bleep-plugin-native-image_3-1.0.0-M15.pom.asc - Failed to verify the PGP signature. Please contact support for assistance."
        |    ]
        |  }
        |}""".stripMargin
    )

    SonatypeCentralClient.describeErrors(status) shouldBe
      """  pkg:maven/build.bleep/bleep-plugin-native-image_3@1.0.0-M15
        |    - Invalid signature for file: bleep-plugin-native-image_3-1.0.0-M15.pom.asc - Failed to verify the PGP signature. Please contact support for assistance.""".stripMargin
  }

  test("a status without errors says so") {
    SonatypeCentralClient.describeErrors(ujson.read("""{"deploymentState": "FAILED"}""")) shouldBe "  (the status response has no errors field)"
  }
}
