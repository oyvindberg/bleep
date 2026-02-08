package bleep.analysis

import bleep.bsp.SharedWorkspaceState
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.BeforeAndAfterEach

import java.nio.file.{Path, Paths}
import java.util.concurrent.CompletableFuture

/** Unit tests for SharedWorkspaceState — the in-memory workspace locking mechanism.
  *
  * Verifies:
  *   1. trySetActive succeeds when workspace is free
  *   2. trySetActive fails when workspace is already taken
  *   3. clearActive with matching work instance removes entry and completes future
  *   4. clearActive with WRONG work instance does NOT remove entry (race condition fix)
  *   5. clearActiveUnconditional always removes entry
  *   6. cancelActive calls cancellation token and forceKill
  *   7. getActive returns the registered work
  */
class SharedWorkspaceStateTest extends AnyFunSuite with Matchers with BeforeAndAfterEach {

  // Use unique workspace paths per test to avoid cross-test interference
  // (SharedWorkspaceState is a singleton object)
  private var testWorkspace: Path = scala.compiletime.uninitialized

  override def beforeEach(): Unit = {
    testWorkspace = Paths.get(s"/tmp/test-workspace-${System.nanoTime()}")
    // Ensure clean state for this workspace
    SharedWorkspaceState.clearActiveUnconditional(testWorkspace)
  }

  override def afterEach(): Unit =
    SharedWorkspaceState.clearActiveUnconditional(testWorkspace)

  private def makeWork(operation: String): SharedWorkspaceState.ActiveWork = {
    val token = CancellationToken.create()
    val completion = new CompletableFuture[Unit]()
    SharedWorkspaceState.ActiveWork(
      operation = operation,
      projects = Set("projectA"),
      cancellationToken = token,
      completion = completion,
      startTimeMs = System.currentTimeMillis(),
      forceKill = () => ()
    )
  }

  test("trySetActive succeeds when workspace is free") {
    val work = makeWork("compile")
    SharedWorkspaceState.trySetActive(testWorkspace, work) shouldBe true
  }

  test("trySetActive fails when workspace is already taken") {
    val work1 = makeWork("compile")
    val work2 = makeWork("test")

    SharedWorkspaceState.trySetActive(testWorkspace, work1) shouldBe true
    SharedWorkspaceState.trySetActive(testWorkspace, work2) shouldBe false
  }

  test("getActive returns registered work") {
    val work = makeWork("compile")
    SharedWorkspaceState.trySetActive(testWorkspace, work)

    val active = SharedWorkspaceState.getActive(testWorkspace)
    active shouldBe Some(work)
    active.get.operation shouldBe "compile"
  }

  test("getActive returns None for free workspace") {
    SharedWorkspaceState.getActive(testWorkspace) shouldBe None
  }

  test("clearActive with matching work removes entry and completes future") {
    val work = makeWork("compile")
    SharedWorkspaceState.trySetActive(testWorkspace, work)

    SharedWorkspaceState.clearActive(testWorkspace, work)

    SharedWorkspaceState.getActive(testWorkspace) shouldBe None
    work.completion.isDone shouldBe true
  }

  test("clearActive with WRONG work instance does NOT remove entry") {
    // This is the critical race condition test:
    // Connection A registers, then disconnects.
    // Meanwhile Connection B has re-registered.
    // Connection A's cleanup should NOT remove B's registration.

    val workA = makeWork("compile-A")
    val workB = makeWork("compile-B")

    // A registers, then B replaces A
    SharedWorkspaceState.trySetActive(testWorkspace, workA) shouldBe true
    // Simulate A releasing and B taking over
    SharedWorkspaceState.clearActive(testWorkspace, workA)
    SharedWorkspaceState.trySetActive(testWorkspace, workB) shouldBe true

    // Now A's disconnect cleanup tries to clear — should NOT affect B
    SharedWorkspaceState.clearActive(testWorkspace, workA)

    // B should still be active
    SharedWorkspaceState.getActive(testWorkspace) shouldBe Some(workB)
    workB.completion.isDone shouldBe false
  }

  test("clearActiveUnconditional always removes entry") {
    val work = makeWork("compile")
    SharedWorkspaceState.trySetActive(testWorkspace, work)

    SharedWorkspaceState.clearActiveUnconditional(testWorkspace)

    SharedWorkspaceState.getActive(testWorkspace) shouldBe None
    work.completion.isDone shouldBe true
  }

  test("cancelActive calls cancellation token and forceKill") {
    var forceKillCalled = false
    val token = CancellationToken.create()
    val completion = new CompletableFuture[Unit]()
    val work = SharedWorkspaceState.ActiveWork(
      operation = "compile",
      projects = Set("projectA"),
      cancellationToken = token,
      completion = completion,
      startTimeMs = System.currentTimeMillis(),
      forceKill = () => { forceKillCalled = true }
    )

    SharedWorkspaceState.trySetActive(testWorkspace, work)
    SharedWorkspaceState.cancelActive(testWorkspace)

    token.isCancelled shouldBe true
    forceKillCalled shouldBe true
  }

  test("cancelActive on free workspace is a no-op") {
    // Should not throw
    SharedWorkspaceState.cancelActive(testWorkspace)
  }

  test("completion future unblocks waiters when clearActive is called") {
    val work = makeWork("compile")
    SharedWorkspaceState.trySetActive(testWorkspace, work)

    // Simulate a waiter thread
    @volatile var waiterUnblocked = false
    val waiterThread = new Thread(
      () => {
        work.completion.get() // Blocks until completed
        waiterUnblocked = true
      },
      "test-waiter"
    )
    waiterThread.setDaemon(true)
    waiterThread.start()

    // Waiter should be blocked
    Thread.sleep(100)
    waiterUnblocked shouldBe false

    // Clear active — should unblock waiter
    SharedWorkspaceState.clearActive(testWorkspace, work)

    waiterThread.join(5000)
    waiterUnblocked shouldBe true
  }

  test("workspace slot becomes free after clearActive") {
    val work1 = makeWork("compile")
    SharedWorkspaceState.trySetActive(testWorkspace, work1) shouldBe true

    SharedWorkspaceState.clearActive(testWorkspace, work1)

    // Should be able to register again
    val work2 = makeWork("test")
    SharedWorkspaceState.trySetActive(testWorkspace, work2) shouldBe true
  }
}
