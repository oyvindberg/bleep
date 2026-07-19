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
    MachineResources.create(totalCpu = cpu, totalMemoryMb = memMb, defaultForkMemoryMb = memMb, logger = TypedLogger.DevNull, longWaitWarnMs = 200L)

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

  test("a fork is charged more than its bare -Xmx (non-heap footprint)") {
    // -Xmx bounds the heap; the process also commits metaspace, code cache, thread stacks, direct
    // buffers and GC bookkeeping. Charging bare -Xmx under-counts every fork.
    MachineResources.forkFootprintMb(12288) should be > 12288L
    MachineResources.forkFootprintMb(12288) shouldBe 15360L
    // Small heaps get a floor rather than a useless percentage.
    MachineResources.forkFootprintMb(256) shouldBe 512L
  }

  test("the two-heaviest-corpus-suites case: two UNCAPPED forks serialize on a 48GB box, one still runs") {
    // Regression for bug-report-testfork-jvm-died-dual-corpus-suites. Both forks state no -Xmx, so
    // HotSpot caps each at a quarter of RAM (12288MB here) and the governor charges the default
    // weight. Originally that weight was the bare quarter-of-RAM and the budget reserved only a
    // tenth of RAM for non-bleep processes, so 2 x 12288 fitted inside 36045MB — while the real
    // footprints (~15GB each, heap plus metaspace/code cache/stacks/GC) did not, and the OS
    // SIGKILLed them. Either suite alone passes and must keep passing.
    val physicalMb = 49152L // 48GB
    val serverHeapMb = 8192L
    val budget = MachineResources.forkMemoryBudgetMb(physicalMb, serverHeapMb)
    val oneFork = MachineResources.defaultForkFootprintMb(physicalMb)

    // The exact numbers from the bug report, so a future change to either formula fails loudly here.
    MachineResources.defaultForkHeapMb(physicalMb) shouldBe 12288L
    oneFork shouldBe 15360L
    budget shouldBe 26624L

    oneFork should be <= budget
    (2 * oneFork) should be > budget

    // And prove it through the governor itself, not just the arithmetic.
    val m = MachineResources.create(
      totalCpu = 18,
      totalMemoryMb = budget,
      defaultForkMemoryMb = oneFork,
      logger = TypedLogger.DevNull,
      longWaitWarnMs = MachineResources.DefaultLongWaitWarnMs
    )
    val prog = for {
      held <- CountDownLatch[IO](1)
      release <- CountDownLatch[IO](1)
      postgres <- m.reserve(TestFork, "PostgresCorpusDiffSuite", cpu = 1, memoryMb = oneFork).use(_ => held.release *> release.await).start
      _ <- held.await
      clickhouse <- m.reserve(TestFork, "ClickhouseCorpusDiffSuite", cpu = 1, memoryMb = oneFork).use(_ => IO.unit).start
      concurrent <- clickhouse.join.as(true).timeoutTo(500.millis, IO.pure(false))
      _ = concurrent shouldBe false // serialized, not admitted alongside
      _ <- release.release
      _ <- postgres.join
      _ <- clickhouse.join.timeout(5.seconds) // and it does run, once the first frees its memory
      end <- m.snapshot
    } yield end.usedMemoryMb shouldBe 0
    prog.timeout(20.seconds).unsafeRunSync()
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
}
