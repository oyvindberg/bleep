package bleep.bsp

import cats.effect.{Clock, IO}
import scala.concurrent.duration.*

object HeapPressureGate {
  val DefaultThreshold: Double = 0.80
  val DefaultRetryMs: DurationMs = DurationMs(2000L)
  /** Minimum scaling factor — even at 0% heap, wait at least this fraction of retryMs */
  val MinDelayFraction: Double = 0.10

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

  /** Wait until it is safe to start a new compilation.
    *
    * Always staggers when other compilations are running, with delay
    * proportional to heap pressure:
    *   - At low heap (e.g. 2%/80%): short delay (~200ms) then proceed
    *   - At moderate heap (e.g. 50%/80%): longer delay (~1250ms) then proceed
    *   - At high heap (>= threshold): full delay, keep waiting until heap drops
    *
    * This prevents stampedes (all cores starting simultaneously) while
    * avoiding unnecessary waits when memory is plentiful.
    */
  def waitForHeapPressure(
      heapMonitor: HeapMonitor,
      activeCompileCount: java.util.concurrent.atomic.AtomicInteger,
      threshold: Double,
      retryMs: DurationMs,
      projectName: String,
      listener: Listener
  )(implicit clock: Clock[IO]): IO[Unit] = {
    def loop(waitStart: Option[EpochMs]): IO[Unit] =
      for {
        usage <- IO(heapMonitor.heapUsage())
        nowMs <- clock.realTime.map(d => EpochMs(d.toMillis))
        othersCompiling = activeCompileCount.get() > 1
        result <-
          if (!othersCompiling) {
            // We're the only active compilation — proceed immediately
            waitStart match {
              case Some(start) =>
                val waitedFor = DurationMs(nowMs.value - start.value)
                IO(listener.onResume(projectName, usage.usedMb, usage.maxMb, waitedFor, nowMs))
              case None =>
                IO.unit
            }
          } else if (waitStart.isDefined && usage.fraction < threshold) {
            // Already waited at least one cycle and heap is below threshold — proceed
            val waitedFor = DurationMs(nowMs.value - waitStart.get.value)
            IO(listener.onResume(projectName, usage.usedMb, usage.maxMb, waitedFor, nowMs))
          } else {
            // Others compiling — stagger with proportional delay.
            // Scale delay by how close we are to the threshold:
            //   fraction 0.02 / threshold 0.80 => scale 0.10 (min) => 200ms
            //   fraction 0.50 / threshold 0.80 => scale 0.625       => 1250ms
            //   fraction 0.80 / threshold 0.80 => scale 1.0          => 2000ms
            val scale = math.max(MinDelayFraction, math.min(1.0, usage.fraction / threshold))
            val effectiveDelayMs = (retryMs.value * scale).toLong
            val effectiveWaitStart = waitStart.getOrElse(nowMs)
            IO(listener.onWait(projectName, usage.usedMb, usage.maxMb, effectiveDelayMs, nowMs)) >>
              IO(BspMetrics.recordHeapPressureStall(projectName, usage.usedMb.value, usage.maxMb.value)) >>
              IO.sleep(effectiveDelayMs.millis) >>
              loop(Some(effectiveWaitStart))
          }
      } yield result
    loop(None)
  }
}
