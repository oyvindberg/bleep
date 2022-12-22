package bleep

import bleep.commands.Scalafmt
import org.scalactic.TripleEqualsSupport
import org.scalatest.funsuite.AnyFunSuite

class ScalafmtTest extends AnyFunSuite with TripleEqualsSupport {

  test("Scalafmt.getVersion parses Scalafmt.defaultConfig") {
    val actual = Scalafmt.getVersion(Scalafmt.defaultConfig)
    val expected = Some("3.5.9")
    assert(actual === expected)
  }

  test("Scalafmt.getVersion parses config with spaces and double-quotes") {
    val input =
      """version = "3.5.9"
        |maxColumn = 160
        |runner.dialect = scala213
        |""".stripMargin
    val actual = Scalafmt.getVersion(input)
    val expected = Some("3.5.9")
    assert(actual === expected)
  }

}
