package bleep.analysis

import bleep.bsp.RequestRegistry
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.BeforeAndAfterEach

import java.nio.file.{Path, Paths}

/** Unit tests for RequestRegistry — the daemon's non-blocking operation registry.
  *
  * Verifies:
  *   1. register always succeeds (multiple operations per workspace)
  *   2. unregister removes specific operation
  *   3. unregisterAll removes all operations for specific IDs
  *   4. getActiveOperations returns all registered operations
  *   5. cancelAll cancels all operations for a workspace
  *   6. cancelOperation cancels a specific operation
  */
class RequestRegistryTest extends AnyFunSuite with Matchers with BeforeAndAfterEach {

  // A fresh registry per test: it is an instance now, so nothing leaks between tests.
  private var registry: RequestRegistry = scala.compiletime.uninitialized
  private val testWorkspace: Path = Paths.get("/tmp/test-workspace")

  override def beforeEach(): Unit =
    registry = new RequestRegistry

  private def makeWork(operationId: String, operation: String): RequestRegistry.ActiveWork = {
    val token = CancellationToken.create()
    RequestRegistry.ActiveWork(
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
    registry.register(testWorkspace, work)
    registry.getActiveOperations(testWorkspace) should have size 1
  }

  test("multiple operations can be registered concurrently") {
    val work1 = makeWork("op-1", "compile")
    val work2 = makeWork("op-2", "test")

    registry.register(testWorkspace, work1)
    registry.register(testWorkspace, work2)

    val active = registry.getActiveOperations(testWorkspace)
    active should have size 2
    active.map(_.operationId).toSet shouldBe Set("op-1", "op-2")
  }

  test("getActiveOperations returns empty for free workspace") {
    registry.getActiveOperations(testWorkspace) shouldBe empty
  }

  test("unregister removes specific operation") {
    val work1 = makeWork("op-1", "compile")
    val work2 = makeWork("op-2", "test")

    registry.register(testWorkspace, work1)
    registry.register(testWorkspace, work2)

    registry.unregister(testWorkspace, "op-1")

    val active = registry.getActiveOperations(testWorkspace)
    active should have size 1
    active.head.operationId shouldBe "op-2"
  }

  test("unregister on non-existent operation is a no-op") {
    val work = makeWork("op-1", "compile")
    registry.register(testWorkspace, work)

    // Should not throw
    registry.unregister(testWorkspace, "non-existent")

    registry.getActiveOperations(testWorkspace) should have size 1
  }

  test("unregisterAll removes only specified operation IDs") {
    val work1 = makeWork("op-1", "compile")
    val work2 = makeWork("op-2", "test")
    val work3 = makeWork("op-3", "link")

    registry.register(testWorkspace, work1)
    registry.register(testWorkspace, work2)
    registry.register(testWorkspace, work3)

    registry.unregisterAll(testWorkspace, List("op-1", "op-3"))

    val active = registry.getActiveOperations(testWorkspace)
    active should have size 1
    active.head.operationId shouldBe "op-2"
  }

  test("cancelAll cancels all operations for workspace") {
    var forceKill1Called = false
    var forceKill2Called = false
    val token1 = CancellationToken.create()
    val token2 = CancellationToken.create()
    val work1 = RequestRegistry.ActiveWork(
      operationId = "op-1",
      operation = "compile",
      projects = Set("projectA"),
      cancellationToken = token1,
      startTimeMs = System.currentTimeMillis(),
      forceKill = () => forceKill1Called = true
    )
    val work2 = RequestRegistry.ActiveWork(
      operationId = "op-2",
      operation = "test",
      projects = Set("projectB"),
      cancellationToken = token2,
      startTimeMs = System.currentTimeMillis(),
      forceKill = () => forceKill2Called = true
    )

    registry.register(testWorkspace, work1)
    registry.register(testWorkspace, work2)
    registry.cancelAll(testWorkspace)

    token1.isCancelled shouldBe true
    token2.isCancelled shouldBe true
    forceKill1Called shouldBe true
    forceKill2Called shouldBe true
  }

  test("cancelOperation cancels only the specified operation") {
    val token1 = CancellationToken.create()
    val token2 = CancellationToken.create()
    val work1 = RequestRegistry.ActiveWork(
      operationId = "op-1",
      operation = "compile",
      projects = Set("projectA"),
      cancellationToken = token1,
      startTimeMs = System.currentTimeMillis(),
      forceKill = () => ()
    )
    val work2 = RequestRegistry.ActiveWork(
      operationId = "op-2",
      operation = "test",
      projects = Set("projectB"),
      cancellationToken = token2,
      startTimeMs = System.currentTimeMillis(),
      forceKill = () => ()
    )

    registry.register(testWorkspace, work1)
    registry.register(testWorkspace, work2)
    registry.cancelOperation(testWorkspace, "op-1")

    token1.isCancelled shouldBe true
    token2.isCancelled shouldBe false
  }

  test("cancelAll on free workspace is a no-op") {
    // Should not throw
    registry.cancelAll(testWorkspace)
  }

  test("workspace becomes free after all operations unregistered") {
    val work1 = makeWork("op-1", "compile")
    val work2 = makeWork("op-2", "test")

    registry.register(testWorkspace, work1)
    registry.register(testWorkspace, work2)

    registry.unregister(testWorkspace, "op-1")
    registry.unregister(testWorkspace, "op-2")

    registry.getActiveOperations(testWorkspace) shouldBe empty
  }
}
