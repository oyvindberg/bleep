package bleep.bsp

import cats.effect.{Clock, IO}
import scala.concurrent.duration.*

object HeapPressureGate {
  val DefaultThreshold: Double = 0.80
  val DefaultRetryMs: DurationMs = DurationMs(2000L)

  /** Callback for when compilation stalls due to heap pressure */
  trait Listener {
    def onStall(project: String, used: HeapMb, max: HeapMb, retryAt: EpochMs, now: EpochMs): Unit
    def onResume(project: String, used: HeapMb, max: HeapMb, stalledFor: DurationMs, now: EpochMs): Unit
    def onSkipBecauseAlone(project: String, used: HeapMb, max: HeapMb): Unit
  }

  object Listener {
    val noop: Listener = new Listener {
      def onStall(project: String, used: HeapMb, max: HeapMb, retryAt: EpochMs, now: EpochMs): Unit = ()
      def onResume(project: String, used: HeapMb, max: HeapMb, stalledFor: DurationMs, now: EpochMs): Unit = ()
      def onSkipBecauseAlone(project: String, used: HeapMb, max: HeapMb): Unit = ()
    }
  }

  /** Wait until it is safe to start a new compilation.
    *
    * When other compilations are running, we ALWAYS wait at least one cycle
    * (`retryMs`) before proceeding — even if heap usage is currently low.
    * This gives existing compilations time to allocate the memory they need.
    * Without this delay, `numCores` compilations could all pass the heap
    * check simultaneously (while heap is still low), then compete for memory.
    *
    * After the mandatory initial delay, the normal heap-pressure check takes
    * over: proceed when heap drops below `threshold`, or loop otherwise.
    */
  def waitForHeapPressure(
      heapMonitor: HeapMonitor,
      activeCompileCount: java.util.concurrent.atomic.AtomicInteger,
      threshold: Double,
      retryMs: DurationMs,
      projectName: String,
      listener: Listener
  )(implicit clock: Clock[IO]): IO[Unit] = {
    def loop(stallStart: Option[EpochMs]): IO[Unit] =
      for {
        usage <- IO(heapMonitor.heapUsage())
        nowMs <- clock.realTime.map(d => EpochMs(d.toMillis))
        othersCompiling = activeCompileCount.get() > 1
        result <-
          if (!othersCompiling) {
            // We're the only active compilation — proceed immediately
            stallStart match {
              case Some(start) =>
                val stalledFor = DurationMs(nowMs.value - start.value)
                IO(listener.onResume(projectName, usage.usedMb, usage.maxMb, stalledFor, nowMs))
              case None =>
                if (usage.fraction >= threshold) {
                  IO(listener.onSkipBecauseAlone(projectName, usage.usedMb, usage.maxMb))
                } else IO.unit
            }
          } else if (stallStart.isDefined && usage.fraction < threshold) {
            // Others compiling, but we already waited at least one cycle
            // and heap is below threshold — safe to proceed
            val stalledFor = DurationMs(nowMs.value - stallStart.get.value)
            IO(listener.onResume(projectName, usage.usedMb, usage.maxMb, stalledFor, nowMs))
          } else {
            // Others compiling and either:
            //  - stallStart.isEmpty: first check, need mandatory initial delay
            //  - heap >= threshold: heap still under pressure, keep waiting
            val retryAt = EpochMs(nowMs.value + retryMs.value)
            val effectiveStallStart = stallStart.getOrElse(nowMs)
            IO(listener.onStall(projectName, usage.usedMb, usage.maxMb, retryAt, nowMs)) >>
              IO(BspMetrics.recordHeapPressureStall(projectName, usage.usedMb.value, usage.maxMb.value)) >>
              IO.sleep(retryMs.value.millis) >>
              loop(Some(effectiveStallStart))
          }
      } yield result
    loop(None)
  }
}
