package bleep

import bleep.mcp.BleepMcpServer
import ch.linkyard.mcp.server.ToolFunction
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import ryddig.TypedLogger

import scala.concurrent.ExecutionContext

/** The effect annotation on an MCP tool is a security control, not documentation. MCP clients map it onto `readOnlyHint` / `destructiveHint` and use those to
  * decide what may run without asking the user. A tool that executes anything the checkout controls — macros, annotation processors, sourcegen scripts, test
  * bodies, main methods — must therefore never be advertised as read-only, no matter how read-only its *purpose* sounds ("just compile", "just run the tests").
  *
  * This table is the pin. Changing an entry is a deliberate act: state below what the tool does that justifies the new effect.
  */
class McpToolEffectTest extends AnyFunSuite with Matchers {
  import ToolFunction.Effect.*

  private val expected: Map[String, (ToolFunction.Effect, Boolean)] = Map(
    // Executes code from the checkout: sourcegen scripts, annotation processors, KSP, macros. Writes generated sources and compile output.
    "bleep.compile" -> (Destructive(idempotent = false), true),
    // Executes the checkout's test bodies, after a compile that runs the above.
    "bleep.test" -> (Destructive(idempotent = false), true),
    // Executes the named program in a forked JVM, after a compile.
    "bleep.run" -> (Destructive(idempotent = false), true),
    // Forks the build's own sourcegen scripts and overwrites the sources they own.
    "bleep.sourcegen" -> (Destructive(idempotent = false), true),
    // Rewrites source files in place.
    "bleep.fmt" -> (Destructive(idempotent = true), true),
    // Deletes build output.
    "bleep.clean" -> (Destructive(idempotent = true), false),
    // Exits this process, dropping every in-flight call on the connection.
    "bleep.restart" -> (Destructive(idempotent = true), false),
    // Loads classes from the project classpath and instantiates the test frameworks it finds, inside the daemon. Writes nothing.
    "bleep.test.suites" -> (Additive(idempotent = true), true),
    // Writes compiled state into a workspace that has none yet; the daemon refuses when state already exists, so the second call fails.
    "bleep.copy-state" -> (Additive(idempotent = false), true),
    // Reads of the workspace's transcript files: no bootstrap, no daemon, no network, no build code.
    "bleep.history.list" -> (ReadOnly, false),
    "bleep.history.show" -> (ReadOnly, false),
    "bleep.history.diff" -> (ReadOnly, false),
    "bleep.history.diff-timing" -> (ReadOnly, false),
    // Reads of the build model. Dependency resolution stays lazy, so nothing is fetched and nothing runs.
    "bleep.build.effective" -> (ReadOnly, false),
    "bleep.projects" -> (ReadOnly, false),
    "bleep.programs" -> (ReadOnly, false),
    "bleep.scripts" -> (ReadOnly, false),
    // Same, except forcing the resolved projects makes coursier reach the build's repositories.
    "bleep.build.resolved" -> (ReadOnly, true)
  )

  private val tools: List[ToolFunction.Info] =
    new BleepMcpServer(TypedLogger.DevNull, UserPaths.fromAppDirs, ExecutionContext.global).declaredTools.map(_.info)

  test("every advertised tool has a pinned effect, and every pinned effect is advertised") {
    tools.map(_.name).toSet shouldBe expected.keySet
  }

  test("each tool declares the effect and open-world reach its implementation actually has") {
    tools.foreach { info =>
      val (effect, isOpenWorld) = expected(info.name)
      withClue(s"${info.name}: ") {
        info.effect shouldBe effect
        info.isOpenWorld shouldBe isOpenWorld
      }
    }
  }

  test("no tool that executes code from the checkout claims to be read-only") {
    // The regression this file exists for: bleep.compile and bleep.test were once advertised read-only, which invites a client to run them unattended.
    val executesCheckoutCode = Set("bleep.compile", "bleep.test", "bleep.run", "bleep.sourcegen", "bleep.test.suites")
    tools.filter(info => executesCheckoutCode(info.name)).foreach { info =>
      withClue(s"${info.name}: ") {
        info.isReadOnly shouldBe false
      }
    }
    // ... and the ones we do call read-only really only read.
    tools.filter(_.isReadOnly).map(_.name).toSet shouldBe Set(
      "bleep.history.list",
      "bleep.history.show",
      "bleep.history.diff",
      "bleep.history.diff-timing",
      "bleep.build.effective",
      "bleep.build.resolved",
      "bleep.projects",
      "bleep.programs",
      "bleep.scripts"
    )
  }
}
