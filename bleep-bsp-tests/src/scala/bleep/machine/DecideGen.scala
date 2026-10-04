package bleep.machine

import scala.util.Random

/** Random scheduler inputs for property tests, from a seeded [[Random]]. No property-testing library is on bleep's classpath, so this is the generator and
  * [[DecidePropertiesTest]] the runner: every property is checked over many seeds, and a failure names the seed that broke it.
  */
object DecideGen {

  case class Inputs(view: MachineView, others: List[StateJson], me: MyState, lock: LockState, params: Params) {
    def machine: Machine = Machine.Cooperative(view, others, lock)
    def ceiling: Long = view.physicalMb - params.headroomMb

    /** Rule 1's room before this tick admits or evicts anything. */
    def roomBefore: Long =
      ceiling - view.usedMb - others.map(_.startingBoundMb).sum - me.forks.collect { case f if f.state == ForkState.Starting => f.boundMb }.sum
  }

  val keys: List[ForkKey] = List("k1", "k2", "k3").map(ForkKey.apply)
  val identity: ServerIdentity = ServerIdentity(pid = 4242L, startedAtEpochMs = 1000L, bleepVersion = "test")
  val now: Long = 1_000_000L

  def pick[A](r: Random, xs: List[A]): A = xs(r.nextInt(xs.length))

  def pressure(r: Random): Pressure = pick(r, List(Pressure.Normal, Pressure.Normal, Pressure.Elevated, Pressure.Critical, Pressure.NoSignal("no PSI")))

  def lock(r: Random): LockState =
    pick(r, List(LockState.Held, LockState.Held, LockState.Unavailable(LockHolder.Announced(1L, 0L, 1500L)), LockState.NotNeeded))

  def forkKind(r: Random): ForkKind = pick(r, ForkKind.all)

  def inputs(r: Random): Inputs = {
    val requests = (0 until r.nextInt(4)).toList.map(i =>
      Request(RequestId(s"r$i"), pick(r, List(RequestKind.Test, RequestKind.Compile)), startedAtMs = now - r.nextInt(5000))
    )
    val requestIds = requests.map(_.id)
    val forks =
      if (requestIds.isEmpty) Nil
      else
        (0 until r.nextInt(6)).toList.map { i =>
          RunningFork(
            id = ForkId(i.toLong + 1),
            pids = if (r.nextBoolean()) Set(10000L + i) else Set.empty,
            owner = pick(r, requestIds),
            key = pick(r, keys),
            kind = forkKind(r),
            boundMb = 256L * (1 + r.nextInt(8)),
            shared = r.nextInt(4) == 0,
            state = if (r.nextBoolean()) ForkState.Starting else ForkState.Measured(footprintMb = 100L * (1 + r.nextInt(10)), atMs = now - 1500L),
            busyCpu = if (r.nextBoolean()) 0 else 1 + r.nextInt(3),
            startedAtMs = now - r.nextInt(20000),
            evicting = r.nextInt(6) == 0,
            pidSinceMs = now - r.nextInt(20000)
          )
        }
    val inHeap = requestIds.flatMap(id => if (r.nextInt(3) == 0) Some(InHeapRunning(id, TaskId(s"${id.value}-running"), InHeapKind.Compile, 1)) else None)
    val ready = requestIds.flatMap { id =>
      (0 until r.nextInt(5)).toList.map { i =>
        if (r.nextBoolean())
          InHeap(
            id,
            TaskId(s"${id.value}-h$i"),
            pick(r, List(InHeapKind.Compile, InHeapKind.Discover, InHeapKind.ResolveAnnotationProcessors)),
            cpu = 1 + r.nextInt(2)
          )
        else
          ForkDemand(
            id,
            TaskId(s"${id.value}-f$i"),
            forkKind(r),
            pick(r, keys),
            boundMb = 256L * (1 + r.nextInt(8)),
            cpu = 1 + r.nextInt(2),
            shared = r.nextInt(4) == 0
          )
      }
    }
    val unstarted = keys.flatMap(k => if (r.nextBoolean()) Some(k -> (1 + r.nextInt(3))) else None).toMap
    val others = (0 until r.nextInt(3)).toList.map { i =>
      StateJson(
        version = 1,
        pid = 100L + i,
        startedAtEpochMs = 1L,
        bleepVersion = "other",
        updatedAtEpochMs = now,
        requests = r.nextInt(3),
        cpuInUse = r.nextInt(8),
        wantsMore = r.nextBoolean(),
        shuttingDown = r.nextInt(4) == 0,
        idleSinceEpochMs = if (r.nextInt(5) == 0) Some(now - r.nextInt(100_000)) else None,
        forks = (0 until r.nextInt(3)).toList.map(j =>
          StateFork(
            id = j.toLong,
            pids = List(20000L + j),
            kind = forkKind(r),
            boundMb = 512L * (1 + r.nextInt(4)),
            state = if (r.nextBoolean()) StateForkState.Starting else StateForkState.Measured(300L),
            startedAtEpochMs = now - 100L
          )
        )
      )
    }
    val physical = 8192L
    val view = MachineView(physicalMb = physical, usedMb = r.nextLong(physical + 2048L), pressure = pressure(r), nowMs = now)
    val me = MyState(
      requests = requests,
      forks = forks,
      inHeap = inHeap,
      ready = ready,
      unstartedSuitesByKey = unstarted,
      heap = HeapUsage(usedMb = r.nextLong(4096L), maxMb = 4096L),
      heapDeferredSince = Map.empty,
      nextForkId = 100L,
      shuttingDown = r.nextInt(4) == 0
    )
    val params = Params(headroomMb = r.nextLong(2048L), parallelism = 1 + r.nextInt(8), maxNewForksPerTick = 1)
    Inputs(view, others, me, lock(r), params)
  }

  /** Runs `check` over `n` seeds, naming the seed and inputs that break it. */
  def forAll(n: Int, baseSeed: Long)(check: Inputs => Unit): Unit =
    (0 until n).foreach { i =>
      val seed = baseSeed + i
      val in = inputs(new Random(seed))
      try check(in)
      catch {
        case e: Throwable => throw new AssertionError(s"property failed for seed $seed\ninputs: $in", e)
      }
    }
}
