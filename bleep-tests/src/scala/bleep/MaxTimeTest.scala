package bleep

import bleep.internal.MaxTime
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Path}
import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.duration.*

/** The in-process analogue of the `bound-check` CI job.
  *
  * A bound nobody exercises is a bound that silently does not work — the `run-bounded.sh` timeout was in place for three consecutive Windows hangs before
  * anyone checked whether it fired. So every rung of the ladder is asserted here against a program that really does hang, rather than assumed from the shape of
  * the code.
  */
class MaxTimeTest extends AnyFunSuite {

  private def tempDump(name: String): Path =
    Files.createTempDirectory("max-time-test").resolve(name)

  private def maxTimeOf(duration: FiniteDuration, dumpTo: Path): MaxTime =
    MaxTime(duration = duration, dumpTo = dumpTo, jvmBinDirs = Nil, serverPids = Nil)

  test("a program that finishes in time is untouched") {
    val dump = tempDump("unused.txt")
    val result = MaxTime.bound(Some(maxTimeOf(30.seconds, dump)), IO.pure(42)).unsafeRunSync()
    assert(result == 42)
    assert(!Files.exists(dump), "nothing should be dumped when the program finished")
  }

  test("no max-time means no bound") {
    val result = MaxTime.bound(None, IO.pure("ok")).unsafeRunSync()
    assert(result == "ok")
  }

  test("a hanging program fails, and says how long it waited") {
    val dump = tempDump("dump.txt")
    val th = intercept[BleepException.Text](MaxTime.bound(Some(maxTimeOf(200.millis, dump)), IO.never[Int]).unsafeRunSync())
    assert(th.message.contains("--max-time"), s"message should name the flag that fired: ${th.message}")
    assert(th.message.contains("200 milliseconds"), s"message should name the duration: ${th.message}")
  }

  test("a hanging program leaves a thread dump behind") {
    val dump = tempDump("dump.txt")
    intercept[BleepException.Text](MaxTime.bound(Some(maxTimeOf(200.millis, dump)), IO.never[Int]).unsafeRunSync())
    assert(Files.exists(dump), "the dump is the whole point — it has to outlive the process")
    val content = Files.readString(dump)
    assert(content.contains("Thread Dump"), s"expected a thread dump, got: ${content.take(200)}")
    // Proves it dumped THIS process rather than writing an empty shell. The test thread is necessarily in there.
    assert(content.contains("--- This JVM"), s"expected this JVM's threads, got: ${content.take(200)}")
    // Not just a header: the dump is only worth writing if it carries frames. This test's own thread must be in there.
    assert(content.contains("MaxTimeTest"), "the dump should contain this test's own stack frames")
  }

  test("cancellation actually reaches the program, it is not merely abandoned") {
    // The rung that matters most and is easiest to get silently wrong: `timeoutAndForget` must still CANCEL the fiber,
    // because that cancellation is what makes lsp4j send `$/cancelRequest` to the compile server.
    val cancelled = new AtomicBoolean(false)
    val started = new CountDownLatch(1)
    val dump = tempDump("dump.txt")
    val program = IO.never[Int].onCancel(IO { cancelled.set(true); () }).guarantee(IO(started.countDown()))
    intercept[BleepException.Text](MaxTime.bound(Some(maxTimeOf(200.millis, dump)), program).unsafeRunSync())
    assert(started.await(5, java.util.concurrent.TimeUnit.SECONDS), "the program's finalizers should have run")
    assert(cancelled.get(), "the fiber must be cancelled, not just dropped")
  }

  test("an uncancellable hang does not hang the bound itself") {
    // Why `timeoutAndForget` rather than `timeout`. `timeout` waits for the cancelled fiber's finalizers, so a fiber
    // wedged in uninterruptible blocking would make the timeout hang on exactly the hang it exists to catch.
    // 2s of uninterruptible sleep against a 200ms bound: with `timeout` this test would take 2s, with
    // `timeoutAndForget` it returns immediately.
    val dump = tempDump("dump.txt")
    val stuck = IO.uncancelable(_ => IO.blocking(Thread.sleep(2000)))
    val startedAt = System.nanoTime()
    intercept[BleepException.Text](MaxTime.bound(Some(maxTimeOf(200.millis, dump)), stuck).unsafeRunSync())
    val elapsed = (System.nanoTime() - startedAt) / 1000000L
    assert(elapsed < 1500, s"the bound waited $elapsed ms for an uncancellable fiber — it should have abandoned it")
  }

  test("parses the durations the help text promises") {
    assert(MaxTime.parse("90s") == Right(90.seconds))
    assert(MaxTime.parse("15m") == Right(15.minutes))
    assert(MaxTime.parse("1h") == Right(1.hour))
    assert(MaxTime.parse("  15m  ") == Right(15.minutes), "surrounding whitespace should not matter")
  }

  test("rejects anything whose meaning would have to be guessed") {
    // A bare number is the important one: `--max-time 30` reads as seconds to one person and minutes to the next, and
    // guessing wrong means a bound that silently is not the one that was asked for.
    List("30", "", "15 m", "1.5h", "15M", "-5m", "0m", "forever", "1d").foreach { bad =>
      assert(MaxTime.parse(bad).isLeft, s"'$bad' should be rejected rather than interpreted")
    }
  }

  test("the rejection says what a good value looks like") {
    val err = MaxTime.parse("30").left.getOrElse(fail("should have been rejected"))
    assert(err.contains("15m"), s"the error should show a valid example, got: $err")
  }

  test("a failing dump reports both problems rather than replacing one with the other") {
    // dumpTo points at a path whose parent is a FILE, so the write cannot succeed.
    val file = Files.createTempFile("max-time-not-a-dir", ".txt")
    val dump = file.resolve("dump.txt")
    val th = intercept[BleepException.Text](MaxTime.bound(Some(maxTimeOf(200.millis, dump)), IO.never[Int]).unsafeRunSync())
    assert(th.message.contains("--max-time"), s"the timeout must still be reported: ${th.message}")
    assert(th.message.contains("could not be written"), s"the dump failure must also be reported: ${th.message}")
  }
}
