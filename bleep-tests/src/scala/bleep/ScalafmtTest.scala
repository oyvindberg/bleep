package bleep

import bleep.commands.Fmt
import org.scalactic.TripleEqualsSupport
import org.scalatest.funsuite.AnyFunSuite

class ScalafmtTest extends AnyFunSuite with TripleEqualsSupport {

  test("Fmt.getScalafmtVersion parses Fmt.defaultScalafmtConfig") {
    val actual = Fmt.getScalafmtVersion(Fmt.defaultScalafmtConfig)
    val expected = Some("3.5.9")
    assert(actual === expected)
  }

  test("Fmt.getScalafmtVersion parses config with spaces and double-quotes") {
    val input =
      """version = "3.5.9"
        |maxColumn = 160
        |runner.dialect = scala213
        |""".stripMargin
    val actual = Fmt.getScalafmtVersion(input)
    val expected = Some("3.5.9")
    assert(actual === expected)
  }

}
