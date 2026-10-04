package bleep.bsp

import bleep.machine.{LockState, Pressure, TickObserver, TickReport}

/** The scheduler's tick reports, aggregated into `metrics.jsonl` (design §10 step 15).
  *
  * Volume: a busy server ticks every 10 ms, a hundred times a second, and most ticks decide nothing. One line per tick would drown the file; one line per
  * wall-clock second, summing what the second's ticks did and keeping the longest lock hold, is a sane rate (at most 3600 lines an hour while busy, none while
  * idle) and loses nothing a reader wants — counts sum, and the hold that matters is the longest. Two things are written the moment they happen, since they are
  * transitions, not rates: a change of pressure level, and the first tick of a second that found the lock unavailable (the rest of that second is counted).
  *
  * One per daemon, called on the tick thread; the aggregation is a few integer adds under a monitor, the write is a queue offer.
  */
final class SchedulerMetrics(write: SchedulerMetrics.Line => Unit) extends TickObserver {
  import SchedulerMetrics._

  private var current: Option[Second] = None
  private var lastPressure: Option[Pressure] = None

  override def tick(report: TickReport): Unit = synchronized {
    val second = report.nowMs / 1000L
    current match {
      case Some(acc) if acc.second != second =>
        write(acc.line)
        current = Some(Second.start(second).add(report))
      case Some(acc) => current = Some(acc.add(report))
      case None      => current = Some(Second.start(second).add(report))
    }
    report.pressure.foreach { pressure =>
      if (!lastPressure.contains(pressure)) {
        lastPressure.foreach(from => write(Line.PressureChanged(report.nowMs, from, pressure)))
        lastPressure = Some(pressure)
      }
    }
    report.lock match {
      case LockState.Unavailable(holder) if current.exists(_.lockUnavailable == 1) => write(Line.LockUnavailable(report.nowMs, holder.describe))
      case _                                                                       => ()
    }
  }

  override def quiet(nowMs: Long): Unit = synchronized {
    current.foreach(acc => write(acc.line))
    current = None
  }
}

object SchedulerMetrics {

  /** One line of `metrics.jsonl`, typed so the test reads fields and the writer spells them. */
  sealed trait Line {
    def json: String
  }
  object Line {
    case class PressureChanged(ts: Long, from: Pressure, to: Pressure) extends Line {
      def json: String = s"""{"type":"pressure","ts":$ts,"from":"${Pressure.name(from)}","to":"${Pressure.name(to)}"}"""
    }
    case class LockUnavailable(ts: Long, holder: String) extends Line {
      def json: String = s"""{"type":"lock_unavailable","ts":$ts,"holder":"${BspMetrics.escape(holder)}"}"""
    }

    /** The second's ticks summed; the `*_guaranteed` counts are the guarantee's share (design §5 rule 5), `hold_max_ms` the longest lock hold and `hold_ms` the
      * sum of each step across the second's holds. `requests`, `forks`, `cpu_in_use`, `live_servers` and `wants_more` are as of the last tick.
      */
    case class SchedulerSecond(
        ts: Long,
        ticks: Int,
        claims: Int,
        lockHeld: Int,
        lockUnavailable: Int,
        holdMaxMs: Long,
        holdMs: Map[String, Long],
        spawns: Int,
        spawnsGuaranteed: Int,
        reuses: Int,
        reusesGuaranteed: Int,
        inHeap: Int,
        inHeapGuaranteed: Int,
        evictedNothingToReuse: Int,
        evictedRoomShortage: Int,
        evictedCriticalPressure: Int,
        evictedOwnerGone: Int,
        heapDeferred: Int,
        requests: Int,
        forks: Int,
        cpuInUse: Int,
        liveServers: Int,
        wantsMore: Boolean
    ) extends Line {
      def json: String = {
        val hold = holdMs.toList.sortBy(_._1).map { case (step, ms) => s""""${BspMetrics.escape(step)}":$ms""" }.mkString("{", ",", "}")
        s"""{"type":"scheduler","ts":$ts,"ticks":$ticks,"claims":$claims,"lock_held":$lockHeld,"lock_unavailable":$lockUnavailable,""" +
          s""""hold_max_ms":$holdMaxMs,"hold_ms":$hold,"spawns":$spawns,"spawns_guaranteed":$spawnsGuaranteed,"reuses":$reuses,""" +
          s""""reuses_guaranteed":$reusesGuaranteed,"in_heap":$inHeap,"in_heap_guaranteed":$inHeapGuaranteed,""" +
          s""""evicted":{"nothing_to_reuse":$evictedNothingToReuse,"room_shortage":$evictedRoomShortage,"critical_pressure":$evictedCriticalPressure,"owner_gone":$evictedOwnerGone},""" +
          s""""heap_deferred":$heapDeferred,"requests":$requests,"forks":$forks,"cpu_in_use":$cpuInUse,"live_servers":$liveServers,"wants_more":$wantsMore}"""
      }
    }
  }

  /** The accumulator for one wall-clock second. */
  private final case class Second(second: Long, line: Line.SchedulerSecond) {
    def lockUnavailable: Int = line.lockUnavailable

    def add(r: TickReport): Second = {
      val held = r.lock == LockState.Held
      val unavailable = r.lock.isInstanceOf[LockState.Unavailable]
      val hold = r.holdBreakdownMs.foldLeft(line.holdMs) { case (acc, (step, ms)) => acc.updated(step, acc.getOrElse(step, 0L) + ms) }
      copy(line =
        line.copy(
          ts = r.nowMs,
          ticks = line.ticks + 1,
          claims = line.claims + (if (r.claimed) 1 else 0),
          lockHeld = line.lockHeld + (if (r.claimed && held) 1 else 0),
          lockUnavailable = line.lockUnavailable + (if (unavailable) 1 else 0),
          holdMaxMs = math.max(line.holdMaxMs, r.holdMs),
          holdMs = hold,
          spawns = line.spawns + r.spawns,
          spawnsGuaranteed = line.spawnsGuaranteed + r.spawnsGuaranteed,
          reuses = line.reuses + r.reuses,
          reusesGuaranteed = line.reusesGuaranteed + r.reusesGuaranteed,
          inHeap = line.inHeap + r.admittedInHeap,
          inHeapGuaranteed = line.inHeapGuaranteed + r.admittedInHeapGuaranteed,
          evictedNothingToReuse = line.evictedNothingToReuse + r.evictedNothingToReuse,
          evictedRoomShortage = line.evictedRoomShortage + r.evictedRoomShortage,
          evictedCriticalPressure = line.evictedCriticalPressure + r.evictedCriticalPressure,
          evictedOwnerGone = line.evictedOwnerGone + r.evictedOwnerGone,
          heapDeferred = line.heapDeferred + r.heapDeferred,
          requests = r.requests,
          forks = r.forks,
          cpuInUse = r.cpuInUse,
          liveServers = r.liveServers,
          wantsMore = r.wantsMore
        )
      )
    }
  }

  private object Second {
    def start(second: Long): Second =
      Second(
        second,
        Line.SchedulerSecond(
          ts = second * 1000L,
          ticks = 0,
          claims = 0,
          lockHeld = 0,
          lockUnavailable = 0,
          holdMaxMs = 0L,
          holdMs = Map.empty,
          spawns = 0,
          spawnsGuaranteed = 0,
          reuses = 0,
          reusesGuaranteed = 0,
          inHeap = 0,
          inHeapGuaranteed = 0,
          evictedNothingToReuse = 0,
          evictedRoomShortage = 0,
          evictedCriticalPressure = 0,
          evictedOwnerGone = 0,
          heapDeferred = 0,
          requests = 0,
          forks = 0,
          cpuInUse = 0,
          liveServers = 1,
          wantsMore = false
        )
      )
  }

  /** The daemon's: every line into `metrics.jsonl`. */
  def toMetricsFile: SchedulerMetrics = new SchedulerMetrics(line => BspMetrics.recordSchedulerLine(line.json))
}
