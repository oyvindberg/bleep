package bleep.machine

import cats.effect.IO

/** The scheduler's answer to a fork demand: run on a fork that exists, or start this one. */
sealed trait ForkGrant {
  def fork: ForkId
}

object ForkGrant {

  /** Start a new fork; the scheduler already counts it as `Starting` under this id. */
  case class Spawn(fork: ForkId) extends ForkGrant

  /** Run on this existing fork: an idle warm one of the same key, or the request's own busy shared one. */
  case class Reuse(fork: ForkId) extends ForkGrant
}

/** How a component that starts forks on behalf of one request asks the scheduler for them (design §10 step 11: test-fork acquisition asks the scheduler).
  *
  * Per request, since every demand names its request and every grant goes back to it.
  */
trait ForkAcquirer {
  def requestId: RequestId

  /** Submit `demand` and wait for the scheduler's grant. Cancelling the wait withdraws the demand.
    *
    * @param group
    *   what the demand's warm fork is kept for: the test project, whose unstarted suites decide whether an idle fork of this key stays (design §5 rule 3)
    */
  def acquire(demand: ForkDemand, group: String): IO[ForkGrant]
}

/** What a component that runs forks reports about them, for the scheduler's accounting (`MachineScheduler`'s fork lifecycle). Daemon-wide, not per request: a
  * fork outlives the request that started it when another request reuses it.
  */
trait ForkLifecycle {

  /** The process exists; `pid` lets the scheduler measure it. */
  def spawned(fork: ForkId, pid: Long): Unit

  /** The fork finished a unit of work and gives back the cpu slots it held for it; with none left it is idle and reusable. */
  def workFinished(fork: ForkId, cpu: Int): Unit

  /** The process is gone — finished, crashed, evicted, or never managed to start. */
  def exited(fork: ForkId): Unit
}
