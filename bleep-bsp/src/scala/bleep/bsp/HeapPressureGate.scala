package bleep.bsp

object HeapPressureGate {
  val DefaultThreshold: Double = 0.80
  val DefaultRetryMs: DurationMs = DurationMs(2000L)

  /** Minimum scaling factor — even at 0% heap, wait at least this fraction of retryMs */
  val MinDelayFraction: Double = 0.10

  /** Hard cap on how long this gate will stall a single compile. Past this the compile proceeds under pressure rather than looping forever: an unbounded stall
    * wedges the whole build with no diagnostic, whereas proceeding risks at worst an OOM — which now exits cleanly (see -XX:+ExitOnOutOfMemoryError in
    * BspRifleConfig) and is restarted, not bricked.
    */
  val MaxWaitMs: Long = 60000L

  /** Callback for when compilation waits for memory */
  trait Listener {
    def onWait(project: String, used: HeapMb, max: HeapMb, delayMs: Long, now: EpochMs): Unit
    def onResume(project: String, used: HeapMb, max: HeapMb, waitedFor: DurationMs, now: EpochMs): Unit
  }

  object Listener {
    val noop: Listener = new Listener {
      def onWait(project: String, used: HeapMb, max: HeapMb, delayMs: Long, now: EpochMs): Unit = ()
      def onResume(project: String, used: HeapMb, max: HeapMb, waitedFor: DurationMs, now: EpochMs): Unit = ()
    }
  }

  /** Whether a compile may start now, and if not, how long its stagger should be.
    *
    * A decision rather than a wait, because the caller is [[TaskDag]]'s admission loop. Waiting here would mean a compile that had ALREADY been admitted —
    * holding a machine-wide CPU permit — sleeping on it, which withholds capacity from every other kind of work while doing nothing. Deferring at admission
    * leaves the permit for a test or a link that could run right now, and the compile is reconsidered on the next wakeup, which fires when a task completes:
    * exactly when heap is most likely to have been freed.
    */
  sealed trait Decision
  object Decision {
    case object Admit extends Decision

    /** Not now. `delayMs` is only the stagger this would have slept, reported to the listener so the "waiting for memory" event still carries a duration. */
    case class Defer(delayMs: Long) extends Decision
  }

  /** The gate's whole policy, as a total function of what it observes. Pure so it can be tested without a heap, a clock, or a scheduler.
    *
    * `firstRefusedAt` is when this task was first deferred (None if it has never been), which is what makes [[MaxWaitMs]] enforceable across separate admission
    * attempts rather than within one sleep loop.
    */
  def decide(
      usage: HeapMonitor.Usage,
      othersCompiling: Boolean,
      threshold: Double,
      retryMs: DurationMs,
      firstRefusedAt: Option[EpochMs],
      now: EpochMs
  ): Decision =
    if (!othersCompiling) Decision.Admit // sole compile: staggering against nobody, and deferring it would stall the build
    else if (firstRefusedAt.isDefined && usage.fraction < threshold) Decision.Admit // waited at least once and the pressure is gone
    else if (firstRefusedAt.exists(start => now.value - start.value >= MaxWaitMs)) Decision.Admit // deadline: proceed under pressure rather than never
    else {
      // Stagger proportional to how close we are to the threshold:
      //   fraction 0.02 / threshold 0.80 => scale 0.10 (min) => 200ms
      //   fraction 0.50 / threshold 0.80 => scale 0.625      => 1250ms
      //   fraction 0.80 / threshold 0.80 => scale 1.0        => 2000ms
      val scale = math.max(MinDelayFraction, math.min(1.0, usage.fraction / threshold))
      Decision.Defer((retryMs.value * scale).toLong)
    }
}
