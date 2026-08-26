package bleep

import bleep.mcp.BleepMcpServer
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** What the MCP server says when a caller names a project that does not exist.
  *
  * It used to say nothing. `resolveProjects` looked each name up with `getOrElse(name, Array.empty)`, so an unmatched name contributed no projects and no
  * complaint, and the caller got a green report about work that did not happen — `bleep.compile` on two projects with one name mistyped answered "1 projects
  * compiled, all clear". The only trace was a count the caller had to notice. Real builds hid compile errors and failing tests behind that for whole review
  * cycles.
  *
  * An agent cannot see a project list it never received, so the server has to be the one that notices. These pin the noticing.
  */
class McpUnmatchedProjectTest extends AnyFunSuite with Matchers {

  private val known = List("aws-common", "aws-common-test", "protocol", "protocol-test")

  test("the message names the unmatched entry, not just the count") {
    val message = BleepMcpServer.unmatchedMessage(List("protocl"), known, "project")
    message should include("protocl")
    message should startWith("no project matches this name:")
  }

  test("a cross-target suffix on a project that has none is explained, because that is the mistake people make") {
    // The reported case: `DuckAlignerProtocolTest@jvm` — a cross-target name for a project with no cross targets, which looks entirely reasonable next to the
    // `@jvm213` names a cross-built project in the same build really does have.
    val message = BleepMcpServer.unmatchedMessage(List("protocol-test@jvm"), known, "project")
    message should include("protocol-test@jvm")
    message should include("it has no cross targets")
    message should include("protocol-test (")
  }

  test("a near miss suggests the real name") {
    // Both `aws-common` and `aws-common-test` are plausible for a truncated `aws-common-te`, and offering both is right — so this asserts that the suggestion
    // is present, not that it is alone.
    val message = BleepMcpServer.unmatchedMessage(List("aws-common-te"), known, "project")
    val suggestionLine = message.linesIterator.find(_.contains("aws-common-te —")).getOrElse(fail(s"no suggestion line in:\n$message"))
    suggestionLine should include("did you mean")
    suggestionLine should include("aws-common-test")
  }

  test("several unmatched names are all reported, not just the first") {
    val message = BleepMcpServer.unmatchedMessage(List("nope-one", "nope-two"), known, "project")
    message should startWith("no project matches these names:")
    message should include("nope-one")
    message should include("nope-two")
  }

  test("the known names are listed, so the caller can correct without another round trip") {
    val message = BleepMcpServer.unmatchedMessage(List("nope"), known, "project")
    known.foreach(k => message should include(k))
  }

  test("a very large build truncates the list rather than burying the complaint") {
    val many = (1 to 200).map(i => f"project$i%03d").toList
    val message = BleepMcpServer.unmatchedMessage(List("nope"), many, "project")
    message should include("160 more")
    // The complaint comes before the list: header, then the offending name, then the known names.
    message.linesIterator.toList(1) should include("nope")
    message.indexOf("nope") should be < message.indexOf("known projects:")
  }

  test("the wording follows what was asked for, so a test-only tool does not talk about projects") {
    BleepMcpServer.unmatchedMessage(List("nope"), known, "test project") should include("no test project matches")
  }
}
