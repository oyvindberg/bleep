package bleep.bsp

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.duration.*

/** Interrupting the thread that owns a BSP connection is how a caller stops it — `BspTestHarness` does exactly that in teardown. These pin what that does.
  *
  * The observable symptom lives in the harness, not here: it prints `[BSP Test Server] Server thread crashed: NoSuchElementException: None.get` for a clean
  * shutdown, but only when the interrupt beats the transport close, so grepping a suite run reproduces it on a loaded CI runner and not on an idle laptop.
  * These tests take the race out of it by interrupting a program that is guaranteed to still be running.
  */
class ServerRunInterruptTest extends AnyFunSuite with Matchers {

  /** Run `body` on a fresh thread, interrupt it once it signals that it is parked, and return whatever escaped. */
  private def interruptWhileRunning(body: CountDownLatch => Unit): Option[Throwable] = {
    val parked = new CountDownLatch(1)
    val thrown = new AtomicReference[Option[Throwable]](None)

    // scalafmt's parser (any version/dialect as of 3.11.5) cannot parse a lambda whose body is an
    // indented try/catch followed by `, arg` in the same call — bind the Runnable first.
    val runnable: Runnable = () =>
      try body(parked)
      catch { case e: Throwable => thrown.set(Some(e)) }
    val t = new Thread(runnable, "interrupt-under-test")
    t.setDaemon(true)
    t.start()

    parked.await()
    // The latch says the IO started, not that the runtime has parked the caller. Without this the interrupt can land before
    // `unsafeRunTimed` reaches its `queue.poll`, and the test measures nothing.
    Thread.sleep(200)
    t.interrupt()
    t.join(10000)
    withClue("thread did not return within 10s of being interrupted: ")(t.isAlive shouldBe false)
    thrown.get()
  }

  test("unsafeRunSync turns an interrupt into NoSuchElementException — the reason runToCompletion exists") {
    val escaped = interruptWhileRunning { parked =>
      (IO.delay(parked.countDown()) >> IO.never).unsafeRunSync()
    }
    // Guards the premise rather than our code: `unsafeRunSync` is `unsafeRunTimed(Long.MaxValue.nanos).get`, and `unsafeRunTimed`
    // answers None on InterruptedException. If a cats-effect upgrade ever changes that, this fails and the doc on
    // `runToCompletion` needs revisiting.
    escaped.map(_.getClass.getName) shouldBe Some("java.util.NoSuchElementException")
  }

  test("runToCompletion returns cleanly on interrupt and re-asserts the flag") {
    @volatile var flagAfter = false
    val escaped = interruptWhileRunning { parked =>
      MultiWorkspaceBspServer.runToCompletion(IO.delay(parked.countDown()) >> IO.never)
      flagAfter = Thread.currentThread().isInterrupted
    }
    escaped shouldBe None
    withClue("interrupt status must survive, so callers that check it still see the stop request: ")(flagAfter shouldBe true)
  }

  test("runToCompletion still returns normally when the program completes on its own") {
    val escaped = interruptWhileRunning { parked =>
      MultiWorkspaceBspServer.runToCompletion(IO.delay(parked.countDown()) >> IO.sleep(50.millis))
    }
    escaped shouldBe None
  }
}
