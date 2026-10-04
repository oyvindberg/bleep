package bleep.mcp

import io.circe.parser.decode
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** `bleep.test`'s `jvmOptions` and `env` arguments, from the JSON an agent sends to the `TestOptions` the daemon receives.
  *
  * Both exist because the MCP server is long-lived: tests forked for it see the environment it was started with, so before these a caller could not set a
  * system property or an environment variable for one run at all.
  */
class McpTestArgsTest extends AnyFunSuite with Matchers {

  private def args(json: String): TestArgs =
    decode[TestArgs](json).fold(e => fail(e.getMessage), identity)

  test("jvmOptions and env are optional, and absent means none") {
    val a = args("""{"directory": "/w"}""")
    a.jvmOptions shouldBe Nil
    a.env shouldBe Map.empty
  }

  test("jvmOptions reach the daemon in order, so a later -Xmx still wins") {
    val a = args("""{"directory": "/w", "jvmOptions": ["-Dcorpus=all", "-Xmx2g"]}""")
    BleepMcpServer.testOptions(a, clientEnv = Map.empty).jvmOptions shouldBe List("-Dcorpus=all", "-Xmx2g")
  }

  test("env goes over the server's own environment instead of replacing it") {
    val a = args("""{"directory": "/w", "env": {"CORPUS": "all", "HOME": "/elsewhere"}}""")
    val env = BleepMcpServer.testOptions(a, clientEnv = Map("HOME" -> "/home/me", "PATH" -> "/usr/bin")).env
    env shouldBe Map("CORPUS" -> "all", "HOME" -> "/elsewhere", "PATH" -> "/usr/bin")
  }

  test("test filters still pass through next to the new arguments") {
    val a = args("""{"directory": "/w", "only": ["a.B"], "exclude": ["a.C"], "jvmOptions": ["-Dx=1"]}""")
    val o = BleepMcpServer.testOptions(a, clientEnv = Map.empty)
    o.only shouldBe List("a.B")
    o.exclude shouldBe List("a.C")
  }

  test("env refuses the variables the fork's launcher owns, rather than corrupting the fork") {
    val e = decode[TestArgs]("""{"directory": "/w", "env": {"CLASSPATH": "/x", "OK": "1"}}""")
    e.left.map(_.getMessage).swap.getOrElse(fail("expected a decoding failure")) should include("CLASSPATH")
  }

  test("a non-string env value is an error, not dropped") {
    decode[TestArgs]("""{"directory": "/w", "env": {"N": 1}}""").isLeft shouldBe true
  }

  test("jvmOptions as a single string is an error, not split or dropped") {
    decode[TestArgs]("""{"directory": "/w", "jvmOptions": "-Dx=1"}""").isLeft shouldBe true
  }
}
