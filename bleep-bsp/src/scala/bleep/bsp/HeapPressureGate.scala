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
          if (usage.fraction < threshold || !othersCompiling) {
            stallStart match {
              case Some(start) =>
                val stalledFor = DurationMs(nowMs.value - start.value)
                IO(listener.onResume(projectName, usage.usedMb, usage.maxMb, stalledFor, nowMs))
              case None =>
                if (usage.fraction >= threshold) {
                  IO(listener.onSkipBecauseAlone(projectName, usage.usedMb, usage.maxMb))
                } else IO.unit
            }
          } else {
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
