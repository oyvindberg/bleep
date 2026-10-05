package bleep.machine

/** An idle server yields its memory when someone else needs it (design §5.1): no client connected, idle long enough, and a [[MemoryNeed]]. Two pure decisions —
  * whether a slow check should go for the lock at all, and, under the lock, whether this server is the one to go — so that the arrangement "one per tick,
  * longest idle first" is tested without a lock, a clock or a socket directory.
  */
object Yield {

  /** How long a server must have been idle before it may yield. PROVISIONAL (design §11): long enough that a developer's pause between two commands does not
    * cost them a cold server, short enough that an abandoned server frees its memory within the time it takes to notice the machine is slow. A plain yield — no
    * shrink step first; shedding (§5.2) has already happened by then, since an idle server sheds everything on the same need.
    */
  val IdleYieldAfterMs: Long = 5L * 60L * 1000L

  /** What the connection registry knows (design §5.1): open non-observer connections — an IDE through `bleep bsp`, an MCP tool call in flight — and when the
    * server last did anything for a real client. Between MCP calls both say idle: `bleep mcp-server` reconnects per call and holds nothing in between.
    */
  case class Idleness(nonObserverConnections: Int, lastActivityEpochMs: Long)

  /** From a slow check, without the lock: is this server a candidate, worth taking the lock for? Every condition of §5.1, with the need as last read. */
  def candidate(idleness: Idleness, schedulerIdle: Boolean, need: Option[MemoryNeed], nowMs: Long, idleYieldAfterMs: Long): Boolean =
    schedulerIdle &&
      idleness.nonObserverConnections == 0 &&
      nowMs - idleness.lastActivityEpochMs >= idleYieldAfterMs &&
      need.isDefined

  /** Under the lock, with every other live server's state fresh: does this one go? Not if another is already on its way out — one per tick — and not if another
    * idle server has been idle longer (ties broken by pid, so two servers idle since the same instant cannot both stay).
    */
  def goes(myIdleSinceEpochMs: Long, myPid: Long, others: List[StateJson]): Boolean =
    !others.exists(_.shuttingDown) &&
      others.forall(o => o.idleSinceEpochMs.forall(theirs => myIdleSinceEpochMs < theirs || (myIdleSinceEpochMs == theirs && myPid < o.pid)))
}
