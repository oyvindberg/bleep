package bleep.machine

import bleep.machine.DecideGen.{forAll, identity, Inputs}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** The invariants of design §5, checked over generated inputs (design §10 step 5). Each test is one rule; the seed is fixed so a failure reproduces. */
class DecidePropertiesTest extends AnyFunSuite with Matchers {
  private val Runs = 1000
  private val Seed = 20261003L

  private def run(in: Inputs, gate: HeapGate = HeapGate.alwaysAdmit): Decision =
    Decide.decide(in.machine, in.me, in.params, gate, identity)

  private def runUnconstrained(in: Inputs): Decision = Decide.decide(Machine.Unconstrained(in.view.nowMs), in.me, in.params, HeapGate.alwaysAdmit, identity)

  private def reclaimed(in: Inputs, d: Decision): Long =
    d.evict.map(e => in.me.forks.find(_.id == e.fork).get.reclaimableMb).sum

  test("Σ bound of new forks never exceeds room, once anything beyond the guarantee is spawned") {
    forAll(Runs, Seed) { in =>
      val d = run(in)
      if (d.spawn.exists(!_.guaranteed))
        withClue(s"spawns ${d.spawn}, room before ${in.roomBefore}, reclaimed ${reclaimed(in, d)}: ")(
          d.spawn.map(_.demand.boundMb).sum should be <= (in.roomBefore + reclaimed(in, d))
        )
    }
  }

  test("where used memory is not room, the decision does not depend on it at all: an empty machine and a full one decide alike") {
    forAll(Runs, Seed + 11) { in =>
      val noRoom = in.copy(view = in.view.copy(roomFromUsed = false))
      val empty = run(noRoom.copy(view = noRoom.view.copy(usedMb = 0L)))
      val full = run(noRoom.copy(view = noRoom.view.copy(usedMb = noRoom.view.physicalMb * 2L)))
      withClue(s"used memory changed the decision on a platform where it is not room: ")(full shouldBe empty)
      // and nothing else loosened: the spawn allowance holds there too
      full.spawn.size should be <= in.params.maxNewForksPerTick
    }
  }

  /** Requests that want a guaranteed fork this tick: no working fork, a ready fork demand. */
  private def wantingGuarantee(in: Inputs): List[Request] =
    in.me.requests.filter { r =>
      !in.me.forks.exists(f => f.owner == r.id && !f.idle) && in.me.ready.exists { case f: ForkDemand => f.request == r.id; case _ => false }
    }

  test("a guaranteed grant goes only to a request without a working fork, at most one per request, whatever the room, lock, pressure or cpu") {
    forAll(Runs, Seed + 1) { in =>
      val d = run(in)
      val wanting = wantingGuarantee(in).map(_.id).toSet
      in.me.requests.foreach { r =>
        val guaranteed = d.reuse.count(x => x.guaranteed && x.demand.request == r.id) + d.spawn.count(x => x.guaranteed && x.demand.request == r.id)
        if (wanting.contains(r.id)) withClue(s"request ${r.id}: ")(guaranteed should be <= 1)
        else withClue(s"request ${r.id}: ")(guaranteed shouldBe 0)
      }
      // Guaranteed reuse is never withheld: a wanting request with an idle fork of its key available gets a guaranteed grant this tick.
      val availableKeys = in.me.forks.filter(_.available).map(_.key).toSet
      wantingGuarantee(in).foreach { r =>
        val first = in.me.ready.collectFirst { case f: ForkDemand if f.request == r.id => f }.get
        if (availableKeys.contains(first.key))
          withClue(s"request ${r.id} with a warm fork of ${first.key}: ")(
            (d.reuse ++ Nil).exists(x => x.guaranteed && x.demand.request == r.id) || d.spawn.exists(x => x.guaranteed && x.demand.request == r.id)
          ) shouldBe true
      }
    }
  }

  test("at most maxNewForksPerTick forks are spawned per tick in total, guaranteed ones included, and under an unavailable lock too") {
    forAll(Runs, Seed + 2) { in =>
      run(in).spawn.size should be <= in.params.maxNewForksPerTick
      run(in.copy(lock = LockState.Unavailable(LockHolder.Announced(7L, 0L, 2000L)))).spawn.size should be <= in.params.maxNewForksPerTick
      run(in.copy(params = in.params.copy(maxNewForksPerTick = 0))).spawn shouldBe Nil
      run(in.copy(params = in.params.copy(maxNewForksPerTick = 3))).spawn.size should be <= 3
    }
  }

  test("a pending guaranteed spawn takes the slot before any non-guaranteed spawn, oldest request first") {
    forAll(Runs, Seed + 13) { in =>
      val d = run(in)
      // Whoever was still without a working fork after guaranteed reuse and got nothing: no younger request, and nobody beyond the guarantee, spawned.
      val reusedGuarantee = d.reuse.filter(_.guaranteed).map(_.demand.request).toSet
      val pending = wantingGuarantee(in).filterNot(r => reusedGuarantee.contains(r.id)).sortBy(r => (r.startedAtMs, r.id.value))
      val spawnedGuarantee = d.spawn.filter(_.guaranteed).map(_.demand.request)
      if (pending.nonEmpty) {
        d.spawn.filter(!_.guaranteed) shouldBe empty
        // The guaranteed spawns are a prefix of the pending requests in age order.
        spawnedGuarantee shouldBe pending.map(_.id).take(spawnedGuarantee.size)
      }
    }
  }

  test("every request gets its guaranteed fork within as many ticks as there are requests ahead of it, under any conditions") {
    forAll(Runs / 4, Seed + 14) { in =>
      val n = 1 + new scala.util.Random(in.view.usedMb).nextInt(5)
      val requests = (0 until n).toList.map(i => Request(RequestId(s"q$i"), RequestKind.Test, startedAtMs = 100L * i))
      val ready =
        requests.map(r => ForkDemand(r.id, TaskId(s"${r.id.value}-t"), ForkKind.TestSuite, ForkKey(s"key-${r.id.value}"), 1024L, cpu = 1, shared = false))
      val hostile = in.copy(
        view = in.view.copy(usedMb = in.view.physicalMb + 1000L, pressure = Pressure.Elevated),
        lock = LockState.Unavailable(LockHolder.Announced(7L, 0L, 2000L)),
        params = in.params.copy(parallelism = 1, maxNewForksPerTick = 1),
        me = MyState.empty.copy(requests = requests, ready = ready, nextForkId = 1L)
      )
      var state = hostile.me
      (1 to n).foreach { tick =>
        val d = Decide.decide(hostile.machine, state, hostile.params, HeapGate.alwaysAdmit, identity)
        d.spawn.map(s => (s.demand.request.value, s.guaranteed)) shouldBe List((s"q${tick - 1}", true))
        d.reuse shouldBe empty
        state = d.next
        state.forks.count(!_.idle) shouldBe tick
      }
      requests.foreach(r => state.forks.exists(f => f.owner == r.id && !f.idle) shouldBe true)
      Decide.decide(hostile.machine, state, hostile.params, HeapGate.alwaysAdmit, identity).spawn shouldBe empty
    }
  }

  test("warm reuse before spawn: no fork is spawned for a key while an idle fork of that key goes unused") {
    forAll(Runs, Seed + 3) { in =>
      val d = run(in)
      val touched = d.reuse.map(_.fork).toSet ++ d.evict.map(_.fork).toSet
      val unusedIdleKeys = in.me.forks.filter(f => f.available && !touched.contains(f.id)).map(_.key).toSet
      d.spawn.foreach(s => withClue(s"spawned $s with an idle fork of its key unused: ")(unusedIdleKeys should not contain s.demand.key))
    }
  }

  test("an idle fork is kept iff its key still has unstarted suites (under Normal pressure, absent a shortage)") {
    forAll(Runs, Seed + 4) { in =>
      val calm = in.copy(view = in.view.copy(pressure = Pressure.Normal))
      val d = run(calm)
      val reused = d.reuse.map(_.fork).toSet
      calm.me.forks.filter(f => f.available && !reused.contains(f.id)).foreach { f =>
        val unstarted = calm.me.unstartedSuitesByKey.getOrElse(f.key, 0)
        val evictedFor = d.evict.find(_.fork == f.id).map(_.reason)
        if (unstarted == 0) withClue(s"fork $f: ")(evictedFor shouldBe Some(Decision.EvictReason.NothingToReuseIt))
        else withClue(s"fork $f: ")(evictedFor should (be(None) or be(Some(Decision.EvictReason.RoomShortage))))
      }
    }
  }

  test("this server's cpu stays within parallelism beyond guarantees") {
    forAll(Runs, Seed + 6) { in =>
      val d = run(in)
      val beyond = d.reuse.exists(!_.guaranteed) || d.spawn.exists(!_.guaranteed) || d.admitInHeap.exists(!_.guaranteed)
      // Guarantees come first, so if anything was admitted beyond them, the total after the tick is within budget.
      if (beyond) d.next.cpuInUse should be <= in.params.parallelism
    }
  }

  test("Elevated pressure spawns nothing beyond guarantees but reuses warm forks as usual; compiles are not held back by it") {
    forAll(Runs, Seed + 7) { in =>
      val elevated = in.copy(view = in.view.copy(pressure = Pressure.Elevated))
      val d = run(elevated)
      d.spawn.foreach(_.guaranteed shouldBe true)
      // With spawning out of the picture, Elevated decides exactly as Normal: reuse, joins, evictions, compiles.
      val noSpawns = in.params.copy(maxNewForksPerTick = 0)
      run(elevated.copy(params = noSpawns)) shouldBe run(in.copy(view = in.view.copy(pressure = Pressure.Normal), params = noSpawns))
      // Pressure governs forks only (design §3.1): with no fork to decide about, the compile decision is the same at every level.
      val noForks = in.copy(me = in.me.copy(ready = in.me.ready.filter(_.isInstanceOf[InHeap])))
      val calm = run(noForks.copy(view = in.view.copy(pressure = Pressure.Normal)))
      run(noForks.copy(view = in.view.copy(pressure = Pressure.Elevated))).admitInHeap shouldBe calm.admitInHeap
      run(noForks.copy(view = in.view.copy(pressure = Pressure.Critical))).admitInHeap shouldBe calm.admitInHeap
    }
  }

  test("without a pressure signal the decision is exactly the decision under Normal: the brake is off, nothing else changes") {
    forAll(Runs, Seed + 15) { in =>
      run(in.copy(view = in.view.copy(pressure = Pressure.NoSignal("no PSI")))) shouldBe run(in.copy(view = in.view.copy(pressure = Pressure.Normal)))
    }
  }

  test("unconstrained is cooperative with unlimited room, no pressure, a held lock and nobody else: nothing else is taken away") {
    forAll(Runs, Seed + 16) { in =>
      val boundless = in.copy(view = in.view.copy(physicalMb = Long.MaxValue / 4, usedMb = 0L, pressure = Pressure.Normal), others = Nil, lock = LockState.Held)
      runUnconstrained(in) shouldBe run(boundless)
    }
  }

  test("unconstrained keeps the local rules: spawns per tick bounded, cpu within parallelism beyond guarantees, warm reuse before spawn, no shortage eviction") {
    forAll(Runs, Seed + 17) { in =>
      val d = runUnconstrained(in)
      d.spawn.size should be <= in.params.maxNewForksPerTick
      val beyond = d.reuse.exists(!_.guaranteed) || d.spawn.exists(!_.guaranteed) || d.admitInHeap.exists(!_.guaranteed)
      if (beyond) d.next.cpuInUse should be <= in.params.parallelism
      d.evict.foreach(_.reason should not be Decision.EvictReason.RoomShortage)
      val touched = d.reuse.map(_.fork).toSet ++ d.evict.map(_.fork).toSet
      val unusedIdleKeys = in.me.forks.filter(f => f.available && !touched.contains(f.id)).map(_.key).toSet
      d.spawn.foreach(s => unusedIdleKeys should not contain s.demand.key)
      val wanting = wantingGuarantee(in).map(_.id).toSet
      in.me.requests.foreach { r =>
        val guaranteed = d.reuse.count(x => x.guaranteed && x.demand.request == r.id) + d.spawn.count(x => x.guaranteed && x.demand.request == r.id)
        if (wanting.contains(r.id)) guaranteed should be <= 1 else guaranteed shouldBe 0
      }
    }
  }

  test("unconstrained: every request gets its guaranteed fork within as many ticks as there are requests ahead of it") {
    forAll(Runs / 4, Seed + 18) { in =>
      val n = 1 + new scala.util.Random(in.view.usedMb).nextInt(5)
      val requests = (0 until n).toList.map(i => Request(RequestId(s"q$i"), RequestKind.Test, startedAtMs = 100L * i))
      val ready =
        requests.map(r => ForkDemand(r.id, TaskId(s"${r.id.value}-t"), ForkKind.TestSuite, ForkKey(s"key-${r.id.value}"), 1024L, cpu = 1, shared = false))
      val params = in.params.copy(parallelism = 1, maxNewForksPerTick = 1)
      var state = MyState.empty.copy(requests = requests, ready = ready, nextForkId = 1L)
      (1 to n).foreach { tick =>
        val d = Decide.decide(Machine.Unconstrained(in.view.nowMs), state, params, HeapGate.alwaysAdmit, identity)
        d.spawn.map(s => (s.demand.request.value, s.guaranteed)) shouldBe List((s"q${tick - 1}", true))
        state = d.next
      }
      requests.foreach(r => state.forks.exists(f => f.owner == r.id && !f.idle) shouldBe true)
    }
  }

  test("Critical pressure evicts every idle fork that no guarantee reuses") {
    forAll(Runs, Seed + 8) { in =>
      val critical = in.copy(view = in.view.copy(pressure = Pressure.Critical))
      val d = run(critical)
      critical.me.forks.filter(_.available).foreach { f =>
        val reusedByGuarantee = d.reuse.exists(r => r.fork == f.id && r.guaranteed)
        val evicted = d.evict.exists(e => e.fork == f.id && e.reason == Decision.EvictReason.CriticalPressure)
        withClue(s"fork $f: ")((reusedByGuarantee || evicted) shouldBe true)
      }
      d.spawn.foreach(_.guaranteed shouldBe true)
      // Nothing idle survives Critical to be reused beyond guarantees; only a busy shared fork — busy before, spawned or taken by a guarantee this tick — is
      // joined.
      d.reuse.filter(!_.guaranteed).foreach { r =>
        val busyBefore = critical.me.forks.find(_.id == r.fork).forall(f => !f.idle)
        val takenByGuarantee = d.reuse.exists(g => g.guaranteed && g.fork == r.fork)
        withClue(s"join $r: ")((busyBefore || takenByGuarantee) shouldBe true)
      }
    }
  }

  test("an unavailable lock allows guarantees, reuse and eviction, and no other spawn") {
    forAll(Runs, Seed + 9) { in =>
      val d = run(in.copy(lock = LockState.Unavailable(LockHolder.Unannounced)))
      d.spawn.foreach(_.guaranteed shouldBe true)
      // Beyond withholding spawns, it changes nothing: the tick decides exactly as one that had nothing to claim.
      d shouldBe run(in.copy(lock = LockState.NotNeeded))
      d.evict.foreach(_.reason should not be Decision.EvictReason.RoomShortage)
    }
  }

  test("compiles never depend on room or the lock") {
    def variants(in: Inputs): List[Inputs] = List(
      in.copy(lock = LockState.Unavailable(LockHolder.Announced(3L, 0L, 5000L))),
      in.copy(lock = LockState.NotNeeded),
      in.copy(view = in.view.copy(usedMb = 0L), others = Nil),
      in.copy(view = in.view.copy(usedMb = in.view.physicalMb * 2))
    )
    forAll(Runs, Seed + 10) { in =>
      // With nothing to fork, room and lock cannot enter the decision at all.
      val noForks = in.copy(me = in.me.copy(ready = in.me.ready.filter(_.isInstanceOf[InHeap])))
      val base = run(noForks)
      variants(noForks).foreach(v => withClue(s"variant lock=${v.lock} used=${v.view.usedMb}: ")(run(v).admitInHeap shouldBe base.admitInHeap))
      // With forks in play, forks and compiles share one cpu budget (design §3.1), so room and lock reach compiles only through the fork grants: whenever
      // those are the same, so are the compiles.
      val full = run(in)
      variants(in).foreach { v =>
        val d = run(v)
        if (d.reuse == full.reuse && d.spawn == full.spawn) withClue(s"variant lock=${v.lock} used=${v.view.usedMb}: ")(d.admitInHeap shouldBe full.admitInHeap)
      }
    }
  }

  test("the published state is the next state: starting forks at their bound, requests and cpu as they are") {
    forAll(Runs, Seed + 11) { in =>
      val d = run(in)
      d.publish.forks shouldBe d.next.forks.map(Decide.toStateFork)
      d.publish.cpuInUse shouldBe d.next.cpuInUse
      d.publish.requests shouldBe in.me.requests.size
      d.publish.wantsMore shouldBe d.next.ready.nonEmpty
      d.publish.shuttingDown shouldBe in.me.shuttingDown
      d.publish.pid shouldBe identity.pid
      d.next.ready
        .map(x => (x.request, x.taskId))
        .toSet
        .intersect((d.reuse.map(_.demand) ++ d.spawn.map(_.demand) ++ d.admitInHeap.map(_.demand)).map(x => (x.request, x.taskId)).toSet) shouldBe empty
      d.spawn.foreach(s => d.next.forks.find(_.id == s.fork).map(_.state) shouldBe Some(ForkState.Starting))
      d.next.forks.map(_.id).distinct.size shouldBe d.next.forks.size
      d.next.forks.map(_.id).toSet should contain allElementsOf in.me.forks.map(_.id) // nothing leaves the state in a decision; only forkExited removes
      d.evict.foreach(e => in.me.forks.find(_.id == e.fork).get.evicting shouldBe false)
    }
  }
}
