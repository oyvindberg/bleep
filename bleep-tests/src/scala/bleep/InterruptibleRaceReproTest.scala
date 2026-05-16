package bleep

import bleep.analysis.CancellationToken
import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.effect.unsafe.implicits.global
import org.scalatest.funsuite.AnyFunSuite

import java.util.concurrent.{CountDownLatch, TimeUnit}
import scala.concurrent.duration.*

/** Bisection of the IO chain that hung in the mixed-compile path. Each test isolates one ingredient and uses a latch-based runner (instead of unsafeRunSync) so
  * we can distinguish "IO never completes" from "queue.poll never wakes."
  *
  * Findings (CE 3.5.4 on JVM 21+):
  *   - `IO.race(IO.never, work)` always returns when `work` succeeds (A3). `IO.never` is implemented via `asyncCheckAttempt` with finalizer = `None` known
  *     upfront, so race can cancel-and-return immediately.
  *   - `IO.race(IO.async_(...), work)` deadlocks at `fa.cancel` even when `work` succeeds (A4, A5, A6). `IO.async_` produces its `None` finalizer through an
  *     IO-that-must-evaluate, so `fa.cancel` waits for that finalizer-IO to resolve — which never happens because the async fiber is suspended on a cb that
  *     never fires.
  *   - The working substitute is a `Deferred[IO, Unit]` whose `.get` is raced against `work`. `Deferred` has proper, race-friendly cancellation semantics (A7).
  *
  * A4–A6 assert the failure mode (latch times out) so they pass as "proof of bug." If a future CE upgrade fixes the underlying issue, these tests will start
  * succeeding and need to be reviewed — at which point we can simplify [[bleep.bsp.Outcome.fromCancellationToken]] / `compileJavaSources` and remove this file.
  */
class InterruptibleRaceReproTest extends AnyFunSuite {

  private def stamp(s: String): Unit = {
    val t = System.currentTimeMillis()
    System.err.println(s"[$t][${Thread.currentThread.getName}] $s")
  }

  /** Runs the IO with a manual latch+callback to bypass IOPlatform.unsafeRunSync. Returns Some(result) on success, None on timeout. */
  private def runWithLatch[A](io: IO[A], timeout: FiniteDuration)(implicit runtime: IORuntime): Either[String, A] = {
    val latch = new CountDownLatch(1)
    val box = new java.util.concurrent.atomic.AtomicReference[Either[Throwable, A]](null)
    io.unsafeRunAsync { res =>
      box.set(res)
      latch.countDown()
    }
    val ok = latch.await(timeout.toMillis, TimeUnit.MILLISECONDS)
    if (!ok) Left("LATCH_TIMEOUT")
    else
      box.get() match {
        case Right(a) => Right(a)
        case Left(t)  => Left(s"FAILED: ${t.getClass.getSimpleName}: ${t.getMessage}")
      }
  }

  test("A1: plain IO.delay { 42 } via latch") {
    stamp("A1 start")
    val r = runWithLatch(IO.delay { stamp("A1 body"); 42 }, 10.seconds)
    stamp(s"A1 result=$r")
    assert(r == Right(42))
  }

  test("A2: plain IO.interruptibleMany { 42 } via latch") {
    stamp("A2 start")
    val r = runWithLatch(IO.interruptibleMany { stamp("A2 body"); 42 }, 10.seconds)
    stamp(s"A2 result=$r")
    assert(r == Right(42))
  }

  test("A3: IO.race(IO.never, IO.interruptibleMany { 42 }) via latch") {
    stamp("A3 start")
    val work = IO.interruptibleMany { stamp("A3 body"); 42 }
    val io = IO.race(IO.never[Unit], work)
    val r = runWithLatch(io, 10.seconds)
    stamp(s"A3 result=$r")
    assert(r == Right(Right(42)))
  }

  test("A4: BUG — IO.race(IO.async_(_ => ()), IO.interruptibleMany { 42 }) never returns (latch times out)") {
    stamp("A4 start")
    val never = IO.async_[Unit](_ => ())
    val work = IO.interruptibleMany { stamp("A4 body"); 42 }
    val io = IO.race(never, work)
    val r = runWithLatch(io, 10.seconds)
    stamp(s"A4 result=$r")
    assert(r == Left("LATCH_TIMEOUT"), "A4 unexpectedly completed — has CE fixed the IO.async_ race deadlock?")
  }

  test("A5: BUG — IO.race(IO.async_ with onCancel, IO.interruptibleMany { 42 }) never returns (latch times out)") {
    stamp("A5 start")
    val token = CancellationToken.create()
    val cancel = IO.async_[Unit] { cb =>
      stamp("A5 async_ registering")
      if (token.isCancelled) cb(Right(()))
      else token.onCancel(() => cb(Right(())))
      stamp("A5 async_ registered")
    }
    val work = IO.interruptibleMany { stamp("A5 body"); 42 }
    val io = IO.race(cancel, work)
    val r = runWithLatch(io, 10.seconds)
    stamp(s"A5 result=$r")
    assert(r == Left("LATCH_TIMEOUT"), "A5 unexpectedly completed — has CE fixed the IO.async_ race deadlock?")
  }

  test("A6: BUG — same as A5 but with IO.delay; the deadlock is IO.async_, not IO.interruptibleMany") {
    stamp("A6 start")
    val token = CancellationToken.create()
    val cancel = IO.async_[Unit] { cb =>
      if (token.isCancelled) cb(Right(()))
      else token.onCancel(() => cb(Right(())))
    }
    val work = IO.delay { stamp("A6 body"); 42 }
    val io = IO.race(cancel, work)
    val r = runWithLatch(io, 10.seconds)
    stamp(s"A6 result=$r")
    assert(r == Left("LATCH_TIMEOUT"), "A6 unexpectedly completed — has CE fixed the IO.async_ race deadlock?")
  }

  test("A7: IO.race(Deferred.get bridge, IO.interruptibleMany { 42 }) via latch — the fix candidate") {
    stamp("A7 start")
    val token = CancellationToken.create()
    val io = cats.effect.Deferred[IO, Unit].flatMap { deferred =>
      IO {
        token.onCancel { () =>
          deferred.complete(()).attempt.unsafeRunSync()(cats.effect.unsafe.IORuntime.global)
          ()
        }
      } *> IO.race(deferred.get, IO.interruptibleMany { stamp("A7 body"); 42 })
    }
    val r = runWithLatch(io, 10.seconds)
    stamp(s"A7 result=$r")
    assert(r == Right(Right(42)))
  }
}
