package bleep

import bleep.commands.Fmt
import org.scalactic.TripleEqualsSupport
import org.scalatest.funsuite.AnyFunSuite

class ScalafmtTest extends AnyFunSuite with TripleEqualsSupport {

  test("Fmt.ScalaFmt.getVersion parses Fmt.ScalaFmt.defaultConfig") {
    val actual = Fmt.ScalaFmt.getVersion(Fmt.ScalaFmt.defaultConfig)
    val expected = Some("3.5.9")
    assert(actual === expected)
  }

  test("Fmt.ScalaFmt.getVersion parses config with spaces and double-quotes") {
    val input =
      """version = "3.5.9"
        |maxColumn = 160
        |runner.dialect = scala213
        |""".stripMargin
    val actual = Fmt.ScalaFmt.getVersion(input)
    val expected = Some("3.5.9")
    assert(actual === expected)
  }

}
