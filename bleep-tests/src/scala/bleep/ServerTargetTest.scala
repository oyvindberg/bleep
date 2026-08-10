package bleep

import bleep.bsp.{ServerDirInfo, ServerState}
import bleep.commands.server.ServerTarget
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.Path

/** One rule for "which server did you mean", shared by status, log, kill and restart.
  *
  * Four daemons on a machine is normal — one per bleep version and JVM config — so each command guessing differently would be its own kind of hostile. The rule
  * is: the id you named, else the daemon serving the build you are standing in, else the only one running, else refuse and list the candidates.
  *
  * These cases deliberately use stopped servers, so the "serving this build" lookup short-circuits without opening a connection.
  */
class ServerTargetTest extends AnyFunSuite with Matchers {

  private def info(hash: String, state: ServerState): ServerDirInfo =
    ServerDirInfo(
      socketDir = Path.of("/tmp/sockets").resolve(hash),
      hash = hash,
      state = state,
      pid = None,
      identity = None,
      sizeBytes = 0L
    )

  private val dead = info("dead1111", ServerState.Dead(crashed = false))
  private val litter = info("litt2222", ServerState.Litter)

  test("a named id wins over everything else") {
    ServerTarget
      .select(List(dead, litter), id = Some("litt"), currentWorkspace = None, allowStopped = true, what = "log")
      .map(_.hash) shouldBe Right("litt2222")
  }

  test("a named id that matches nothing fails rather than falling back to a guess") {
    ServerTarget
      .select(List(dead, litter), id = Some("nope"), currentWorkspace = None, allowStopped = true, what = "log")
      .isLeft shouldBe true
  }

  test("with nothing running and stopped servers allowed, a lone stopped server is the answer") {
    withClue("reading a dead daemon's log is a main reason to be here: ") {
      ServerTarget
        .select(List(dead), id = None, currentWorkspace = None, allowStopped = true, what = "log")
        .map(_.hash) shouldBe Right("dead1111")
    }
  }

  test("with nothing running and several stopped, it refuses and names them") {
    val error = ServerTarget
      .select(List(dead, litter), id = None, currentWorkspace = None, allowStopped = true, what = "log")
      .left
      .getOrElse(fail("expected a refusal"))
      .message

    error should include("dead1111")
    error should include("litt2222")
    error should include("log")
  }

  test("commands that need a live server say so instead of offering a corpse") {
    val error = ServerTarget
      .select(List(dead), id = None, currentWorkspace = None, allowStopped = false, what = "inspect")
      .left
      .getOrElse(fail("expected a refusal"))
      .message

    error should include("no compile server is running")
    error should include("bleep server ls")
  }

  test("no servers at all is its own message, not an ambiguity") {
    ServerTarget
      .select(Nil, id = None, currentWorkspace = None, allowStopped = true, what = "log")
      .left
      .getOrElse(fail("expected a refusal"))
      .message shouldBe "no compile servers"
  }
}
