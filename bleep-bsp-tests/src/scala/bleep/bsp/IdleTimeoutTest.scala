package bleep.bsp

import cats.effect.IO
import cats.effect.Ref
import cats.effect.testkit.TestControl
import cats.effect.unsafe.implicits.global
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration.*

/** [[IdleTimeout]] under virtual time.
  *
  * Every case here is about *when* something happens, and none of it waits: `TestControl` runs the whole program on a simulated clock, so a two-minute timeout
  * is asserted in microseconds and the result is exact rather than approximate. That matters more than speed. The two bugs this combinator was written twice to
  * avoid — a race that could never fire against uncancellable work, and a teardown that could wait forever — are both invisible to a test that merely observes
  * "it eventually finished", and both are caught here by asserting the precise instant.
  */
class IdleTimeoutTest extends AnyFunSuite with Matchers {

  private val Idle = 2.minutes
  private val Grace = 10.seconds

  /** Work that never completes and cannot be cancelled — the shape of a fork parked in a read that never returns. `IO.race` against this deadlocks, which is
    * the whole reason [[IdleTimeout.bound]] races a `join` instead.
    */
  private val uncancellable: IO[Nothing] = IO.uncancelable(_ => IO.never)

  private def constantActivity(at: FiniteDuration): IO[FiniteDuration] = IO.pure(at)

  test("fires at exactly the idle deadline when nothing reports progress") {
    val program = for {
      fired <- Ref.of[IO, Boolean](false)
      started <- IO.monotonic
      result <- IdleTimeout.bound(Idle, Grace, constantActivity(started), fired.set(true))(uncancellable)
      at <- IO.monotonic
      wasAsked <- fired.get
    } yield (result, at - started, wasAsked)

    val (result, elapsed, wasAsked) = TestControl.executeEmbed(program).unsafeRunSync()
    result shouldBe Left(IdleTimeout.Fired(Idle))
    // Idle, then the full grace period, because uncancellable work never notices being asked to stop.
    elapsed shouldBe (Idle + Grace)
    wasAsked shouldBe true
  }

  test("does not fire while progress keeps being reported") {
    val program = for {
      lastActivity <- IO.monotonic.flatMap(Ref.of[IO, FiniteDuration])
      ticker = (IO.sleep(Idle / 2) >> IO.monotonic.flatMap(lastActivity.set)).foreverM
      // Work that finishes well after the idle bound but never goes quiet for it.
      work = IO.sleep(Idle * 5).as("done")
      result <- IdleTimeout.bound(Idle, Grace, lastActivity.get, IO.unit)(work).race(ticker)
    } yield result

    TestControl.executeEmbed(program).unsafeRunSync() shouldBe Left(Right("done"))
  }

  test("work that finishes normally is neither asked to stop nor delayed") {
    val program = for {
      fired <- Ref.of[IO, Boolean](false)
      started <- IO.monotonic
      result <- IdleTimeout.bound(Idle, Grace, constantActivity(started), fired.set(true))(IO.sleep(1.second).as(42))
      at <- IO.monotonic
      wasAsked <- fired.get
    } yield (result, at - started, wasAsked)

    val (result, elapsed, wasAsked) = TestControl.executeEmbed(program).unsafeRunSync()
    result shouldBe Right(42)
    elapsed shouldBe 1.second
    wasAsked shouldBe false
  }

  test("work that stops when asked returns as soon as it does, not after the whole grace period") {
    val program = for {
      stop <- cats.effect.Deferred[IO, Unit]
      started <- IO.monotonic
      // Notices the request after a second, well inside the grace period.
      work = stop.get >> IO.sleep(1.second) >> IO.pure("tidy")
      result <- IdleTimeout.bound(Idle, Grace, constantActivity(started), stop.complete(()).void)(work)
      at <- IO.monotonic
    } yield (result, at - started)

    val (result, elapsed) = TestControl.executeEmbed(program).unsafeRunSync()
    result shouldBe Left(IdleTimeout.Fired(Idle))
    elapsed shouldBe (Idle + 1.second)
  }

  test("an error from the work surfaces rather than being reported as a timeout") {
    val boom = new RuntimeException("boom")
    val program = IO.monotonic.flatMap(started => IdleTimeout.bound(Idle, Grace, constantActivity(started), IO.unit)(IO.raiseError[Int](boom)))
    TestControl.executeEmbed(program).attempt.unsafeRunSync().left.map(_.getMessage) shouldBe Left("boom")
  }
}
