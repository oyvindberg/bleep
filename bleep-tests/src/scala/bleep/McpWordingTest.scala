package bleep

import bleep.mcp.BleepMcpServer
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** The MCP server's words are load-bearing: they are the only thing standing between an agent and the worst-case reasoning chain "bleep tool failed/missing =>
  * this is not a bleep project => silently switch build tools" (or its inverse: retrying bleep forever against a Maven project). These tests pin the properties
  * that make the words work.
  */
class McpWordingTest extends AnyFunSuite with Matchers {

  test("instructions fit within Claude Code's truncation limit, so no guidance is silently invisible") {
    // Observed in Claude Code's MCP logs: "Server instructions truncated from 3384 to 2048 chars".
    // Everything past the limit never reaches the agent.
    withClue(s"instructions are ${BleepMcpServer.instructionsText.length} chars, limit ${BleepMcpServer.InstructionsCharLimit}:") {
      BleepMcpServer.instructionsText.length should be <= BleepMcpServer.InstructionsCharLimit
    }
  }

  test("instructions lead with scope: bleep-builds-only guidance comes before everything else") {
    val text = BleepMcpServer.instructionsText
    val scopeIdx = text.indexOf("## Scope")
    scopeIdx should be >= 0
    // Ahead of the other sections, so even harsher truncation than Claude Code's keeps it.
    scopeIdx should be < text.indexOf("## Workspaces")
    // The two lessons from the observed incident, stated in words the agent reads:
    // a missing/failing tool never identifies the project's build system, and non-bleep projects get redirected.
    text should include("bleep.yaml")
    text should include("Maven")
    text should include("say NOTHING about what kind of project this is")
  }

  test("the not-a-bleep-build error reads as a definitive redirect, not a transient failure") {
    val message = BleepMcpServer.notABleepBuild("/some/checkout").getMessage
    message should include("/some/checkout is not part of a bleep build")
    message should include("definitive answer")
    message should include("do not retry")
    // It hands the agent its next move instead of leaving it to infer one.
    message should include("Maven")
    message should include("Gradle")
    message should include("sbt")
  }
}
