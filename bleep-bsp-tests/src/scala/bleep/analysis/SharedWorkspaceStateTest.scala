package bleep.analysis

import bleep.bsp.SharedWorkspaceState
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.BeforeAndAfterEach

import java.nio.file.{Path, Paths}

/** Unit tests for SharedWorkspaceState — the non-blocking operation registry.
  *
  * Verifies:
  *   1. register always succeeds (multiple operations per workspace)
  *   2. unregister removes specific operation
  *   3. unregisterAll removes all operations for specific IDs
  *   4. getActiveOperations returns all registered operations
  *   5. cancelAll cancels all operations for a workspace
  *   6. cancelOperation cancels a specific operation
  */
class SharedWorkspaceStateTest extends AnyFunSuite with Matchers with BeforeAndAfterEach {

  // Use unique workspace paths per test to avoid cross-test interference
  // (SharedWorkspaceState is a singleton object)
  private var testWorkspace: Path = scala.compiletime.uninitialized

  override def beforeEach(): Unit =
    testWorkspace = Paths.get(s"/tmp/test-workspace-${System.nanoTime()}")

  override def afterEach(): Unit =
    // Clean up all operations for this workspace
    SharedWorkspaceState.getActiveOperations(testWorkspace).foreach { work =>
      SharedWorkspaceState.unregister(testWorkspace, work.operationId)
    }

  private def makeWork(operationId: String, operation: String): SharedWorkspaceState.ActiveWork = {
    val token = CancellationToken.create()
    SharedWorkspaceState.ActiveWork(
      operationId = operationId,
      operation = operation,
      projects = Set("projectA"),
      cancellationToken = token,
      startTimeMs = System.currentTimeMillis(),
      forceKill = () => ()
    )
  }

  test("register always succeeds") {
    val work = makeWork("op-1", "compile")
    SharedWorkspaceState.register(testWorkspace, work)
    SharedWorkspaceState.getActiveOperations(testWorkspace) should have size 1
  }

  test("multiple operations can be registered concurrently") {
    val work1 = makeWork("op-1", "compile")
    val work2 = makeWork("op-2", "test")

    SharedWorkspaceState.register(testWorkspace, work1)
    SharedWorkspaceState.register(testWorkspace, work2)

    val active = SharedWorkspaceState.getActiveOperations(testWorkspace)
    active should have size 2
    active.map(_.operationId).toSet shouldBe Set("op-1", "op-2")
  }

  test("getActiveOperations returns empty for free workspace") {
    SharedWorkspaceState.getActiveOperations(testWorkspace) shouldBe empty
  }

  test("unregister removes specific operation") {
    val work1 = makeWork("op-1", "compile")
    val work2 = makeWork("op-2", "test")

    SharedWorkspaceState.register(testWorkspace, work1)
    SharedWorkspaceState.register(testWorkspace, work2)

    SharedWorkspaceState.unregister(testWorkspace, "op-1")

    val active = SharedWorkspaceState.getActiveOperations(testWorkspace)
    active should have size 1
    active.head.operationId shouldBe "op-2"
  }

  test("unregister on non-existent operation is a no-op") {
    val work = makeWork("op-1", "compile")
    SharedWorkspaceState.register(testWorkspace, work)

    // Should not throw
    SharedWorkspaceState.unregister(testWorkspace, "non-existent")

    SharedWorkspaceState.getActiveOperations(testWorkspace) should have size 1
  }

  test("unregisterAll removes only specified operation IDs") {
    val work1 = makeWork("op-1", "compile")
    val work2 = makeWork("op-2", "test")
    val work3 = makeWork("op-3", "link")

    SharedWorkspaceState.register(testWorkspace, work1)
    SharedWorkspaceState.register(testWorkspace, work2)
    SharedWorkspaceState.register(testWorkspace, work3)

    SharedWorkspaceState.unregisterAll(testWorkspace, List("op-1", "op-3"))

    val active = SharedWorkspaceState.getActiveOperations(testWorkspace)
    active should have size 1
    active.head.operationId shouldBe "op-2"
  }

  test("cancelAll cancels all operations for workspace") {
    var forceKill1Called = false
    var forceKill2Called = false
    val token1 = CancellationToken.create()
    val token2 = CancellationToken.create()
    val work1 = SharedWorkspaceState.ActiveWork(
      operationId = "op-1",
      operation = "compile",
      projects = Set("projectA"),
      cancellationToken = token1,
      startTimeMs = System.currentTimeMillis(),
      forceKill = () => forceKill1Called = true
    )
    val work2 = SharedWorkspaceState.ActiveWork(
      operationId = "op-2",
      operation = "test",
      projects = Set("projectB"),
      cancellationToken = token2,
      startTimeMs = System.currentTimeMillis(),
      forceKill = () => forceKill2Called = true
    )

    SharedWorkspaceState.register(testWorkspace, work1)
    SharedWorkspaceState.register(testWorkspace, work2)
    SharedWorkspaceState.cancelAll(testWorkspace)

    token1.isCancelled shouldBe true
    token2.isCancelled shouldBe true
    forceKill1Called shouldBe true
    forceKill2Called shouldBe true
  }

  test("cancelOperation cancels only the specified operation") {
    val token1 = CancellationToken.create()
    val token2 = CancellationToken.create()
    val work1 = SharedWorkspaceState.ActiveWork(
      operationId = "op-1",
      operation = "compile",
      projects = Set("projectA"),
      cancellationToken = token1,
      startTimeMs = System.currentTimeMillis(),
      forceKill = () => ()
    )
    val work2 = SharedWorkspaceState.ActiveWork(
      operationId = "op-2",
      operation = "test",
      projects = Set("projectB"),
      cancellationToken = token2,
      startTimeMs = System.currentTimeMillis(),
      forceKill = () => ()
    )

    SharedWorkspaceState.register(testWorkspace, work1)
    SharedWorkspaceState.register(testWorkspace, work2)
    SharedWorkspaceState.cancelOperation(testWorkspace, "op-1")

    token1.isCancelled shouldBe true
    token2.isCancelled shouldBe false
  }

  test("cancelAll on free workspace is a no-op") {
    // Should not throw
    SharedWorkspaceState.cancelAll(testWorkspace)
  }

  test("workspace becomes free after all operations unregistered") {
    val work1 = makeWork("op-1", "compile")
    val work2 = makeWork("op-2", "test")

    SharedWorkspaceState.register(testWorkspace, work1)
    SharedWorkspaceState.register(testWorkspace, work2)

    SharedWorkspaceState.unregister(testWorkspace, "op-1")
    SharedWorkspaceState.unregister(testWorkspace, "op-2")

    SharedWorkspaceState.getActiveOperations(testWorkspace) shouldBe empty
  }
}
