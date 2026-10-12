package bleep.testing

import bleep.MachineResources
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import ryddig.TypedLogger

import java.nio.file.{Files, Path}
import scala.concurrent.duration._

/** What a pool does at the edges of its life: asked for a JVM after it has shut down.
  *
  * The failure behind this: a test run was cancelled while suites were still being scheduled. The pool shut down, the run's remaining suites kept asking it for
  * JVMs, and every JVM started after the shutdown was owned by a pool nothing would ever shut down again — six of them holding 30GB of the machine's memory
  * budget for the life of the daemon. None of these tests needs a JVM to start: each asks for one where none may be started.
  */
class JvmPoolLifecycleTest extends AnyFunSuite with Matchers {

  private def machine(memMb: Long): MachineResources =
    MachineResources.create(totalCpu = 4, totalMemoryMb = memMb, logger = TypedLogger.DevNull, longWaitWarnMs = 60000L)

  private def pool(m: MachineResources, workDir: Path) =
    JvmPool.create(maxConcurrency = 4, jvmCommand = workDir.resolve("no-such-java"), workingDirectory = workDir, machine = m, listener = JvmPoolListener.noop)

  private val request = (label: String) =>
    TestSessionRequest(
      label = label,
      classpath = Nil,
      jvmOptions = Nil,
      defaultHeapMb = 512L,
      runnerClass = "bleep.testing.runner.ForkedTestRunner",
      environment = Map.empty,
      workingDirectory = None,
      sharing = SessionSharing.Exclusive
    )

  test("a pool that has shut down refuses to start a JVM, and reserves nothing for it") {
    val workDir = Files.createTempDirectory("jvm-pool-lifecycle")
    val m = machine(memMb = 8192)
    val prog = for {
      allocated <- pool(m, workDir).allocated
      (p, shutdown) = allocated
      _ <- shutdown
      attempt <- p.acquire(request("late-suite")).use_.attempt
      snap <- m.snapshot
    } yield {
      attempt.left.toOption.get shouldBe a[JvmPool.PoolClosed]
      snap.usedMemoryMb shouldBe 0
      snap.waiting shouldBe empty
    }
    prog.timeout(20.seconds).unsafeRunSync()
  }

  test("a spawn queued for memory is withdrawn when its run is cancelled, and is not granted later") {
    // The wait used to be uncancelable, so a cancelled run's queued spawns sat it out, were granted once
    // the run's own JVMs gave their memory back, and started JVMs for a run that no longer existed.
    val workDir = Files.createTempDirectory("jvm-pool-lifecycle")
    val m = machine(memMb = 1024)
    val prog = pool(m, workDir).use { p =>
      for {
        busy <- m.leaseUntilReleased(MachineResources.ResourceKind.TestFork, "someone-else", cpu = 0, memoryMb = 1024)
        spawn <- p.acquire(request("queued-suite")).use_.start
        _ <- (IO.sleep(20.millis) >> m.snapshot.map(_.waiting.nonEmpty)).iterateUntil(identity).timeout(10.seconds)
        _ <- spawn.cancel.timeout(5.seconds)
        afterCancel <- m.snapshot
        _ <- busy.release
        afterRelease <- m.snapshot
      } yield {
        afterCancel.waiting shouldBe empty
        afterRelease.usedMemoryMb shouldBe 0 // nothing was left queued to take the memory and spawn
        afterRelease.active shouldBe empty
      }
    }
    prog.timeout(30.seconds).unsafeRunSync()
  }

  test("a shared session asked of a pool that has shut down is refused the same way") {
    val workDir = Files.createTempDirectory("jvm-pool-lifecycle")
    val m = machine(memMb = 8192)
    val prog = for {
      allocated <- pool(m, workDir).allocated
      (p, shutdown) = allocated
      _ <- shutdown
      attempt <- p.acquire(request("late-shared-suite").copy(sharing = SessionSharing.Shared("proj"))).use_.attempt
      snap <- m.snapshot
    } yield {
      attempt.left.toOption.get shouldBe a[JvmPool.PoolClosed]
      snap.usedMemoryMb shouldBe 0
    }
    prog.timeout(20.seconds).unsafeRunSync()
  }
}
