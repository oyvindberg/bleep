package bleep.machine

/** What one deciding tick did, for metrics (design §10 step 15). Plain counts, so an observer can sum them per second: at a 10 ms cadence a line per tick would
  * be a hundred lines a second of mostly nothing.
  *
  * @param claimed
  *   the tick went for the lock (a ready fork demand nothing warm could absorb)
  * @param holdBreakdownMs
  *   how long each step under the lock took — probe, read, decide, write — when the lock was held; empty otherwise. Their sum is the hold time.
  * @param pressure
  *   as probed this tick; absent in unconstrained mode
  */
case class TickReport(
    nowMs: Long,
    claimed: Boolean,
    lock: LockState,
    holdBreakdownMs: List[(String, Long)],
    spawns: Int,
    spawnsGuaranteed: Int,
    reuses: Int,
    reusesGuaranteed: Int,
    admittedInHeap: Int,
    admittedInHeapGuaranteed: Int,
    evictedNothingToReuse: Int,
    evictedRoomShortage: Int,
    evictedCriticalPressure: Int,
    evictedOwnerGone: Int,
    heapDeferred: Int,
    pressure: Option[Pressure],
    liveServers: Int,
    requests: Int,
    forks: Int,
    cpuInUse: Int,
    wantsMore: Boolean
) {
  def holdMs: Long = holdBreakdownMs.map(_._2).sum
  def decidedAnything: Boolean =
    spawns + reuses + admittedInHeap + evictedNothingToReuse + evictedRoomShortage + evictedCriticalPressure + evictedOwnerGone + heapDeferred > 0
}

object TickReport {
  def of(
      decision: Decision,
      nowMs: Long,
      claimed: Boolean,
      lock: LockState,
      holdBreakdownMs: List[(String, Long)],
      pressure: Option[Pressure],
      liveServers: Int
  ): TickReport =
    TickReport(
      nowMs = nowMs,
      claimed = claimed,
      lock = lock,
      holdBreakdownMs = holdBreakdownMs,
      spawns = decision.spawn.size,
      spawnsGuaranteed = decision.spawn.count(_.guaranteed),
      reuses = decision.reuse.size,
      reusesGuaranteed = decision.reuse.count(_.guaranteed),
      admittedInHeap = decision.admitInHeap.size,
      admittedInHeapGuaranteed = decision.admitInHeap.count(_.guaranteed),
      evictedNothingToReuse = decision.evict.count(_.reason == Decision.EvictReason.NothingToReuseIt),
      evictedRoomShortage = decision.evict.count(_.reason == Decision.EvictReason.RoomShortage),
      evictedCriticalPressure = decision.evict.count(_.reason == Decision.EvictReason.CriticalPressure),
      evictedOwnerGone = decision.evict.count(_.reason == Decision.EvictReason.OwnerGone),
      heapDeferred = decision.heapDeferred.size,
      pressure = pressure,
      liveServers = liveServers,
      requests = decision.next.requests.size,
      forks = decision.next.forks.size,
      cpuInUse = decision.next.cpuInUse,
      wantsMore = decision.next.wantsMore
    )
}

/** Where tick reports go. Separate from [[SchedulerEffects]] — those are instructions to carry out, this is observation — and injected like them, so a test can
  * count ticks without a metrics file and the daemon can aggregate them into one.
  */
trait TickObserver {

  /** A deciding tick ended. Called on the tick thread, after the lock is released; must not block. */
  def tick(report: TickReport): Unit

  /** A slow check on an idle server: nothing decided since the last report. Lets an aggregating observer flush what it holds. */
  def quiet(nowMs: Long): Unit
}

object TickObserver {

  /** For tests and in-process servers without a metrics file. */
  val none: TickObserver = new TickObserver {
    def tick(report: TickReport): Unit = ()
    def quiet(nowMs: Long): Unit = ()
  }
}
