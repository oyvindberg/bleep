package bleep
package model

import io.circe.Json
import io.circe.syntax.*
import org.scalatest.funsuite.AnyFunSuite

class OptionsTest extends AnyFunSuite {
  private def roundtrip(opts: Options): Options =
    opts.asJson.as[Options].fold(err => fail(err.getMessage), identity)

  test("an argument with spaces is one argument, quoted in the build file") {
    val errorProne = "-Xplugin:ErrorProne -XepDisableWarningsInGeneratedCode -Xep:NullAway:ERROR"
    val opts = Options.fromArgs(List("-parameters", errorProne, "-source", "17"), None)
    assert(opts.render.contains(errorProne))
    // options are written sorted
    assert(opts.asJson == Json.fromString(s""""$errorProne" -parameters -source 17"""))
    assert(roundtrip(opts) == opts)
  }

  test("scalac's -Wconf with a message pattern") {
    val wconf = "-Wconf:msg=unused value of type org.scalatest.Assertion:s"
    val opts = Options.fromArgs(List(wconf, "-deprecation"), None)
    assert(opts.render.toSet == Set(wconf, "-deprecation"))
    assert(roundtrip(opts) == opts)
  }

  test("quotes and backslashes in an argument survive the build file") {
    val arg = """-Dmessage=say "hi" \o/"""
    val opts = Options.fromArgs(List(arg), None)
    assert(roundtrip(opts).render == List(arg))
  }

  test("options without quotes read as before, backslashes outside quotes are literal") {
    val opts = Options.parse(List("""-encoding UTF-8 -Xlint -javaagent:C:\agents\a.jar"""), None)
    assert(opts.values.map(_.render) == Set(List("-encoding", "UTF-8"), List("-Xlint"), List("""-javaagent:C:\agents\a.jar""")))
  }

  test("a quote starts where an argument starts or in its middle") {
    assert(Options.split("""-Wconf:"msg=a b":s -x""") == List("-Wconf:msg=a b:s", "-x"))
  }

  test("an unterminated quote is an error") {
    assert(Json.fromString("""-x "-Xplugin:ErrorProne""").as[Options].isLeft)
  }
}
