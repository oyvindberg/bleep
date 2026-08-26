package bleep.bsp

import cats.effect.IO

import scala.concurrent.duration.FiniteDuration

/** Bounds work that reports progress, without bounding how long it may legitimately take.
  *
  * A total timeout has to be guessed, and every guess is wrong in both directions: it kills an honest slow suite and it lets a real hang sit. What can be
  * measured instead is progress. A test run that keeps reporting results is working, however long it takes; one that has reported nothing for a while is stuck.
  *
  * ==Why the cancellation dance==
  *
  * The obvious spelling, `IO.race(run, timer)`, cannot work here, and fails in the one case it exists to catch. `IO.race` cancels the loser and waits for that
  * cancellation to finish — and cancelling work parked in an uninterruptible read never finishes. Racing a hang against a timer therefore produces a timer that
  * can never fire.
  *
  * So the run is started as a fiber and the race is against its `join`, which is always cancellable and leaves the fiber alone. Stopping the work is
  * `onTimeout`'s job — for a test suite that means signalling the runner to tear down its process — and `grace` bounds how long we wait for it to notice. After
  * that we stop waiting whatever the run is doing, because the entire point is that the caller stops waiting.
  */
object IdleTimeout {

  /** Fired because nothing reported progress for [[idle]]. */
  final case class Fired(idle: FiniteDuration)

  /** @param idle
    *   how long without progress counts as stuck
    * @param grace
    *   how long to give the work to stop, once asked
    * @param lastActivityAt
    *   monotonic reading of when progress was last reported. Read repeatedly; it is expected to move.
    * @param onTimeout
    *   asks the work to stop. Runs once, before the grace period.
    */
  def bound[A](
      idle: FiniteDuration,
      grace: FiniteDuration,
      lastActivityAt: IO[FiniteDuration],
      onTimeout: IO[Unit]
  )(run: IO[A]): IO[Either[Fired, A]] = {
    // Sleeps exactly as long as remains, rather than polling on an interval: one wakeup per quiet stretch, and the moment it fires is the moment it is due.
    val awaitIdle: IO[Unit] = {
      def loop: IO[Unit] =
        for {
          now <- IO.monotonic
          last <- lastActivityAt
          remaining = idle - (now - last)
          _ <- if (remaining.toMillis <= 0) IO.unit else IO.sleep(remaining) >> loop
        } yield ()
      loop
    }

    run.start.flatMap { fiber =>
      IO.race(fiber.join, awaitIdle).flatMap {
        case Left(outcome) => outcome.embedError.map(Right(_))
        case Right(())     => onTimeout >> fiber.join.void.timeoutTo(grace, IO.unit).as(Left(Fired(idle)))
      }
    }
  }
}
