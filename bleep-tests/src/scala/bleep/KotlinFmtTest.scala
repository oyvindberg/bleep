package bleep

import bleep.commands.Fmt
import org.scalactic.TripleEqualsSupport
import org.scalatest.funsuite.AnyFunSuite

class KotlinFmtTest extends AnyFunSuite with TripleEqualsSupport {

  test("parseConfig returns default config for empty input") {
    val config = Fmt.KotlinFmt.parseConfig("")
    assert(config === Fmt.KotlinFmt.KotlinFmtConfig.default)
    assert(config.enabled === true)
    assert(config.style === "kotlinlang")
  }

  test("parseConfig parses enabled = false") {
    val config = Fmt.KotlinFmt.parseConfig("enabled = false")
    assert(config.enabled === false)
  }

  test("parseConfig parses all options") {
    val input =
      """enabled = true
        |version = "0.61"
        |style = google
        |excludePaths = ["glob:**/generated/**", "glob:**/build/**"]
        |""".stripMargin
    val config = Fmt.KotlinFmt.parseConfig(input)
    assert(config.enabled === true)
    assert(config.version === "0.61")
    assert(config.style === "google")
    assert(config.excludePaths === List("glob:**/generated/**", "glob:**/build/**"))
  }

  test("buildFlags emits --kotlinlang-style for kotlinlang") {
    val config = Fmt.KotlinFmt.KotlinFmtConfig.default
    assert(Fmt.KotlinFmt.buildFlags(config) === List("--kotlinlang-style"))
  }

  test("buildFlags emits --google-style for google") {
    val config = Fmt.KotlinFmt.KotlinFmtConfig.default.copy(style = "google")
    assert(Fmt.KotlinFmt.buildFlags(config) === List("--google-style"))
  }

  test("buildFlags emits nothing for meta (ktfmt's default style)") {
    val config = Fmt.KotlinFmt.KotlinFmtConfig.default.copy(style = "meta")
    assert(Fmt.KotlinFmt.buildFlags(config) === Nil)
  }

  test("buildFlags throws on unknown style") {
    val config = Fmt.KotlinFmt.KotlinFmtConfig.default.copy(style = "nonsense")
    intercept[BleepException.Text](Fmt.KotlinFmt.buildFlags(config))
  }

  test("default version matches FetchKtfmt.DefaultVersion") {
    assert(Fmt.KotlinFmt.KotlinFmtConfig.default.version === FetchKtfmt.DefaultVersion)
  }
}
