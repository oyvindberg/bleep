package bleep.bsp

import cats.effect.{Clock, IO}
import scala.concurrent.duration.*

object HeapPressureGate {
  val DefaultThreshold: Double = 0.80
  val DefaultRetryMs: DurationMs = DurationMs(2000L)

  /** Callback for when compilation waits for memory */
  trait Listener {
    def onWait(project: String, used: HeapMb, max: HeapMb, retryAt: EpochMs, now: EpochMs): Unit
    def onResume(project: String, used: HeapMb, max: HeapMb, waitedFor: DurationMs, now: EpochMs): Unit
  }

  object Listener {
    val noop: Listener = new Listener {
      def onWait(project: String, used: HeapMb, max: HeapMb, retryAt: EpochMs, now: EpochMs): Unit = ()
      def onResume(project: String, used: HeapMb, max: HeapMb, waitedFor: DurationMs, now: EpochMs): Unit = ()
    }
  }

  /** Wait until heap pressure is below threshold before starting compilation.
    *
    * When other compilations are running and heap usage exceeds `threshold`,
    * delays this compilation until memory is freed. If heap is below the
    * threshold or no other compilations are running, proceeds immediately.
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
          if (!othersCompiling || usage.fraction < threshold) {
            // Safe to proceed: either we're alone or heap is below threshold
            waitStart match {
              case Some(start) =>
                val waitedFor = DurationMs(nowMs.value - start.value)
                IO(listener.onResume(projectName, usage.usedMb, usage.maxMb, waitedFor, nowMs))
              case None =>
                IO.unit
            }
          } else {
            // Others compiling and heap >= threshold — wait for memory
            val retryAt = EpochMs(nowMs.value + retryMs.value)
            val effectiveWaitStart = waitStart.getOrElse(nowMs)
            IO(listener.onWait(projectName, usage.usedMb, usage.maxMb, retryAt, nowMs)) >>
              IO(BspMetrics.recordHeapPressureStall(projectName, usage.usedMb.value, usage.maxMb.value)) >>
              IO.sleep(retryMs.value.millis) >>
              loop(Some(effectiveWaitStart))
          }
      } yield result
    loop(None)
  }
}
