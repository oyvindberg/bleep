package bleep

import cats.effect.IO
import cats.effect.std.CountDownLatch
import cats.effect.unsafe.implicits.global
import cats.syntax.all._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import ryddig.TypedLogger

import scala.concurrent.duration._

/** Semantics of the machine-resource governor: accounting, the total memory budget, CPU sharing between compiles and forks, work-conserving fairness,
  * cancellation, and clamping.
  */
class MachineResourcesTest extends AnyFunSuite with Matchers {
  import MachineResources.ResourceKind._

  private def machine(cpu: Int, memMb: Long): MachineResources =
    MachineResources.create(totalCpu = cpu, totalMemoryMb = memMb, logger = TypedLogger.DevNull, longWaitWarnMs = 200L)

  test("a reservation within budget is granted immediately and released") {
    val m = machine(cpu = 4, memMb = 8192)
    val prog = for {
      _ <- m.reserve(TestFork, "a", cpu = 1, memoryMb = 2048).use { _ =>
        m.snapshot.map { s =>
          s.usedCpu shouldBe 1
          s.usedMemoryMb shouldBe 2048
          s.active.map(_.label) shouldBe List("a")
        }
      }
      after <- m.snapshot
    } yield {
      after.usedCpu shouldBe 0
      after.usedMemoryMb shouldBe 0
      after.active shouldBe empty
    }
    prog.timeout(20.seconds).unsafeRunSync()
  }

  test("total memory budget bounds concurrent forks: the one that doesn't fit waits until memory frees") {
    val m = machine(cpu = 16, memMb = 4096) // budget only fits two 2g forks at once
    val prog = for {
      firstAcquired <- CountDownLatch[IO](2)
      release <- CountDownLatch[IO](1)
      holders <- List("h1", "h2").parTraverse { name =>
        m.reserve(TestFork, name, cpu = 1, memoryMb = 2048).use(_ => firstAcquired.release *> release.await).start
      }
      _ <- firstAcquired.await
      third <- m.reserve(TestFork, "h3", cpu = 1, memoryMb = 2048).use(_ => IO.unit).start
      grantedEarly <- third.join.as(true).timeoutTo(300.millis, IO.pure(false))
      _ = grantedEarly shouldBe false
      full <- m.snapshot
      _ = full.usedMemoryMb shouldBe 4096
      _ <- release.release
      _ <- holders.traverse(_.join)
      _ <- third.join.timeout(5.seconds)
      end <- m.snapshot
    } yield end.usedMemoryMb shouldBe 0
    prog.timeout(20.seconds).unsafeRunSync()
  }

  test("compiles and forks compete for the same CPU") {
    val m = machine(cpu = 2, memMb = 100000) // memory is not the constraint here
    val prog = for {
      filled <- CountDownLatch[IO](2)
      release <- CountDownLatch[IO](1)
      compiles <- List("c1", "c2").parTraverse { n =>
        m.reserve(Compile, n, cpu = 1, memoryMb = 0).use(_ => filled.release *> release.await).start
      }
      _ <- filled.await
      fork <- m.reserve(TestFork, "f1", cpu = 1, memoryMb = 1024).use(_ => IO.unit).start
      early <- fork.join.as(true).timeoutTo(300.millis, IO.pure(false))
      _ = early shouldBe false
      s <- m.snapshot
      _ = s.usedCpu shouldBe 2
      _ <- release.release
      _ <- compiles.traverse(_.join)
      _ <- fork.join.timeout(5.seconds)
    } yield ()
    prog.timeout(20.seconds).unsafeRunSync()
  }

  test("work-conserving: a small request is not blocked behind a larger one that doesn't fit") {
    val m = machine(cpu = 8, memMb = 4096)
    val prog = for {
      held <- CountDownLatch[IO](1)
      releaseHolder <- CountDownLatch[IO](1)
      holder <- m.reserve(TestFork, "big-holder", cpu = 1, memoryMb = 3072).use(_ => held.release *> releaseHolder.await).start
      _ <- held.await
      // A 2048MB fork can't fit (only 1024 free) and waits — it proceeds once the holder frees.
      waiter <- m.reserve(TestFork, "wants-2g", cpu = 1, memoryMb = 2048).use(_ => IO.unit).start
      _ <- IO.sleep(100.millis)
      // A 512MB fork fits and must proceed now, despite the older 2g waiter — work-conserving.
      small <- m.reserve(TestFork, "wants-512m", cpu = 1, memoryMb = 512).use(_ => IO.unit).start
      _ <- small.join.timeout(3.seconds)
      s <- m.snapshot
      _ = s.waiting.map(_.label) should contain("wants-2g")
      _ <- releaseHolder.release
      _ <- holder.join
      _ <- waiter.join.timeout(5.seconds)
      end <- m.snapshot
    } yield end.usedMemoryMb shouldBe 0
    prog.timeout(20.seconds).unsafeRunSync()
  }

  test("a waiting reservation is released on cancel (a cancelled fork frees its slot)") {
    val m = machine(cpu = 1, memMb = 1024)
    val prog = for {
      held <- CountDownLatch[IO](1)
      releaseHolder <- CountDownLatch[IO](1)
      holder <- m.reserve(TestFork, "holder", cpu = 1, memoryMb = 1024).use(_ => held.release *> releaseHolder.await).start
      _ <- held.await
      // Full on both cpu and memory — this one waits.
      waiter <- m.reserve(TestFork, "waiter", cpu = 1, memoryMb = 1024).use(_ => IO.unit).start
      _ <- IO.sleep(200.millis)
      s1 <- m.snapshot
      _ = s1.waiting.map(_.label) shouldBe List("waiter")
      _ <- waiter.cancel.timeout(5.seconds) // must complete — production cancels test runs mid-wait
      s2 <- m.snapshot
      _ = s2.waiting shouldBe empty
      _ <- releaseHolder.release
      _ <- holder.join
    } yield ()
    prog.timeout(20.seconds).unsafeRunSync()
  }

  test("every fork is bounded: a build stating no -Xmx gets one imposed, and a stated one is left alone") {
    // The whole point of the redesign. An unstated -Xmx is not "unlimited" — HotSpot hands the fork
    // MaxRAMPercentage=25, a quarter of the machine, which is how ~18 forks came to request 216GB on
    // a 48GB box. Containment is the bound, not the scheduling.
    MachineResources.withHeapBound(Nil) shouldBe List(s"-Xmx${MachineResources.DefaultForkHeapMb}m")
    MachineResources.withHeapBound(List("-XX:+UseZGC")) shouldBe List("-XX:+UseZGC", s"-Xmx${MachineResources.DefaultForkHeapMb}m")
    // A build that stated its own bound keeps it, exactly.
    MachineResources.withHeapBound(List("-Xmx12g")) shouldBe List("-Xmx12g")
    MachineResources.DefaultForkHeapMb shouldBe 2048L
  }

  test("one number decides both what a fork may use and what it is charged") {
    // If these were derived separately they would drift, and the governor would be accounting for a
    // bound the process was never held to — which is what made the old accounting fiction.
    MachineResources.forkHeapMb(None) shouldBe MachineResources.DefaultForkHeapMb
    MachineResources.forkHeapMb(Some("12g")) shouldBe 12288L
    // Charged the heap plus the non-heap the JVM also commits (metaspace, code cache, stacks, GC).
    MachineResources.forkFootprintMb(2048) shouldBe 2560L
    MachineResources.forkFootprintMb(256) shouldBe 512L // small heaps get a floor, not a useless percentage
  }

  test("at the 2GB default, CPU is very nearly the binding constraint on a 48GB box") {
    // Regression for bug-report-testfork-jvm-died-dual-corpus-suites, but asserting the property that
    // actually matters for large builds: the memory rail must not be what decides how wide the build
    // runs. Previously two forks exhausted the budget; now ~10 fit against 18 cores.
    val physicalMb = 49152L // 48GB
    val serverHeapMb = 8192L
    val budget = MachineResources.forkMemoryBudgetMb(physicalMb, serverHeapMb)
    val defaultFork = MachineResources.forkFootprintMb(MachineResources.DefaultForkHeapMb)

    budget shouldBe 26624L
    defaultFork shouldBe 2560L
    (budget / defaultFork) shouldBe 10L

    // And a genuinely heavy fork that DECLARES 12g still can't pair up with another — two of those
    // is the ~30GB that got SIGKILLed on this exact machine.
    val heavyFork = MachineResources.forkFootprintMb(12288)
    heavyFork should be <= budget
    (2 * heavyFork) should be > budget
  }

  test("tryReserve grants what fits, refuses what doesn't, and never queues") {
    val m = machine(cpu = 4, memMb = 4096)
    val prog = for {
      first <- m.tryReserve(TestFork, "fits", cpu = 1, memoryMb = 3072)
      _ = first shouldBe defined
      // Doesn't fit — and crucially returns immediately rather than parking, so the caller can go
      // free memory by other means (the JVM pool evicts an idle process here).
      second <- m.tryReserve(TestFork, "does-not-fit", cpu = 1, memoryMb = 3072).timeout(2.seconds)
      _ = second shouldBe empty
      waiting <- m.snapshot.map(_.waiting)
      _ = waiting shouldBe empty // a refusal must not leave a waiter behind
      _ <- first.get // release
      third <- m.tryReserve(TestFork, "fits-now", cpu = 1, memoryMb = 3072)
      _ = third shouldBe defined
      _ <- third.get
      end <- m.snapshot
    } yield end.usedMemoryMb shouldBe 0
    prog.timeout(20.seconds).unsafeRunSync()
  }

  test("reserveUntilReleased holds resources past the scope that took them, until the release action runs") {
    // Process-lifetime semantics: a pooled test JVM keeps its memory reservation across suites, so
    // the reservation cannot be a Resource scoped to whichever suite spawned the process.
    val m = machine(cpu = 4, memMb = 4096)
    val prog = for {
      release <- m.reserveUntilReleased(TestFork, "pooled-jvm", cpu = 0, memoryMb = 2048)
      afterScope <- m.snapshot
      _ = afterScope.usedMemoryMb shouldBe 2048 // still held although nothing lexically owns it
      _ = afterScope.usedCpu shouldBe 0 //  an idle pooled JVM costs RAM but no cores
      _ <- release
      afterRelease <- m.snapshot
    } yield afterRelease.usedMemoryMb shouldBe 0
    prog.timeout(20.seconds).unsafeRunSync()
  }

  test("a request larger than the machine is clamped so it still eventually runs") {
    val m = machine(cpu = 2, memMb = 1024)
    val prog = m.reserve(TestFork, "greedy", cpu = 99, memoryMb = 1_000_000).use(_ => m.snapshot.map(_.usedCpu shouldBe 2))
    prog.timeout(5.seconds).unsafeRunSync()
  }

  test("the budget tracks what the machine can spare, and admits more when it grows") {
    // The point of making it dynamic: a budget derived once from physical RAM is wrong the moment
    // anything else on the machine starts or stops.
    val m = machine(cpu = 8, memMb = 2048)
    val prog = for {
      held <- CountDownLatch[IO](1)
      release <- CountDownLatch[IO](1)
      first <- m.reserve(TestFork, "a", cpu = 1, memoryMb = 2048).use(_ => held.release *> release.await).start
      _ <- held.await
      // Full: a second fork can't fit.
      second <- m.reserve(TestFork, "b", cpu = 1, memoryMb = 2048).use(_ => IO.unit).start
      early <- second.join.as(true).timeoutTo(300.millis, IO.pure(false))
      _ = early shouldBe false
      // The machine frees up (a browser closed, another build finished) — the waiter proceeds
      // without anything being released.
      _ <- m.retuneMemoryBudget(4096)
      _ <- second.join.timeout(5.seconds)
      _ <- release.release
      _ <- first.join
    } yield ()
    prog.timeout(20.seconds).unsafeRunSync()
  }

  test("shrinking the budget stops new admissions but never evicts a running fork") {
    // Reclaiming from live processes would mean killing suites that are doing nothing wrong, so a
    // shrink only closes the door; usage falls back under the line as forks finish naturally.
    val m = machine(cpu = 8, memMb = 4096)
    val prog = for {
      held <- CountDownLatch[IO](1)
      release <- CountDownLatch[IO](1)
      running <- m.reserve(TestFork, "running", cpu = 1, memoryMb = 3072).use(_ => held.release *> release.await).start
      _ <- held.await
      _ <- m.retuneMemoryBudget(1024) // now less than what is already held
      snap <- m.snapshot
      _ = snap.usedMemoryMb shouldBe 3072 // still held, not clawed back
      budget <- m.memoryBudgetMb
      _ = budget shouldBe 1024
      // Nothing new gets in while we are over the line.
      blocked <- m.reserve(TestFork, "new", cpu = 1, memoryMb = 512).use(_ => IO.unit).start
      early <- blocked.join.as(true).timeoutTo(300.millis, IO.pure(false))
      _ = early shouldBe false
      _ <- release.release
      _ <- running.join
      _ <- blocked.join.timeout(5.seconds) // admitted once the running fork finished
    } yield ()
    prog.timeout(20.seconds).unsafeRunSync()
  }

  test("budgetFor is relative to what we already hold, so it self-corrects") {
    // The OS's "available" figure already accounts for the forks we are running, so the total we may
    // hold is what we hold now plus what is left, minus slack. No modelling of other processes.
    val physical = 49152L
    val slack = MachineMemory.slackMb(physical)
    MachineMemory.budgetFor(currentlyUsedMb = 6000, availableForMoreMb = 10000, physicalMb = physical, floorMb = 1024) shouldBe (16000 - slack)
    // A machine with nothing to spare collapses to the floor rather than to zero — something must
    // always be able to run.
    MachineMemory.budgetFor(currentlyUsedMb = 0, availableForMoreMb = 0, physicalMb = physical, floorMb = 1024) shouldBe 1024L
  }

  test("vm_stat output parses into the page counts the budget needs") {
    val sample =
      """Mach Virtual Memory Statistics: (page size of 16384 bytes)
        |Pages free:                                4000.
        |Pages wired down:                        349235.
        |Anonymous pages:                        1314168.
        |File-backed pages:                       468294.""".stripMargin
    val parsed = MachineMemory.MacOs.parse(sample)
    parsed("page size") shouldBe 16384L
    parsed("Anonymous pages") shouldBe 1314168L
    parsed("Pages wired down") shouldBe 349235L
    parsed("File-backed pages") shouldBe 468294L // parsed, but deliberately NOT subtracted: clean and evictable
  }
}
