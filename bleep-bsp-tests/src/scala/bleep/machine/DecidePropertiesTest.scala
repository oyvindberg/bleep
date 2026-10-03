package bleep.machine

import bleep.machine.DecideGen.{forAll, identity, Inputs}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** The invariants of design §5, checked over generated inputs (design §10 step 5). Each test is one rule; the seed is fixed so a failure reproduces. */
class DecidePropertiesTest extends AnyFunSuite with Matchers {
  private val Runs = 1000
  private val Seed = 20261003L

  private def run(in: Inputs, gate: HeapGate = HeapGate.alwaysAdmit): Decision =
    Decide.decide(in.view, in.others, in.me, in.lock, in.params, gate, identity)

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

  test("every request with no running fork and a ready fork demand gets exactly one guaranteed fork, whatever the room, lock, pressure or cpu") {
    forAll(Runs, Seed + 1) { in =>
      val d = run(in)
      in.me.requests.foreach { r =>
        val hadFork = in.me.forks.exists(f => f.owner == r.id && !f.idle)
        val wantsFork = in.me.ready.exists { case f: ForkDemand => f.request == r.id; case _ => false }
        val guaranteed = d.reuse.filter(x => x.guaranteed && x.demand.request == r.id).size + d.spawn.filter(x => x.guaranteed && x.demand.request == r.id).size
        if (!hadFork && wantsFork) withClue(s"request ${r.id}: ")(guaranteed shouldBe 1)
        else withClue(s"request ${r.id}: ")(guaranteed shouldBe 0)
        // And it has one afterwards, in the published state.
        if (!hadFork && wantsFork) d.next.forks.exists(_.owner == r.id) shouldBe true
      }
    }
  }

  test("under the worst conditions a request with nothing running gets one fork and nothing more") {
    forAll(Runs, Seed + 2) { in =>
      val hostile = in.copy(
        view = in.view.copy(usedMb = in.view.physicalMb + 1000L, pressure = Pressure.Elevated),
        lock = LockState.Unavailable(LockHolder.Announced(7L, 0L, 2000L)),
        params = in.params.copy(parallelism = 1)
      )
      val d = run(hostile)
      hostile.me.requests.foreach { r =>
        val hadFork = hostile.me.forks.exists(f => f.owner == r.id && !f.idle)
        val wantsFork = hostile.me.ready.exists { case f: ForkDemand => f.request == r.id; case _ => false }
        val grants = d.reuse.count(_.demand.request == r.id) + d.spawn.count(_.demand.request == r.id)
        if (!hadFork && wantsFork) grants shouldBe 1 else grants shouldBe 0
      }
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

  test("at most maxNewForksPerTick new forks beyond guarantees") {
    forAll(Runs, Seed + 5) { in =>
      val d = run(in)
      d.spawn.count(!_.guaranteed) should be <= in.params.maxNewForksPerTick
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

  test("Elevated pressure admits no fork beyond guarantees; compiles are not held back by it") {
    forAll(Runs, Seed + 7) { in =>
      val elevated = in.copy(view = in.view.copy(pressure = Pressure.Elevated))
      val d = run(elevated)
      d.reuse.foreach(_.guaranteed shouldBe true)
      d.spawn.foreach(_.guaranteed shouldBe true)
      // Pressure governs forks only (design §3.1): with no fork to decide about, the compile decision is the same at every level.
      val noForks = in.copy(me = in.me.copy(ready = in.me.ready.filter(_.isInstanceOf[InHeap])))
      val calm = run(noForks.copy(view = in.view.copy(pressure = Pressure.Normal)))
      run(noForks.copy(view = in.view.copy(pressure = Pressure.Elevated))).admitInHeap shouldBe calm.admitInHeap
      run(noForks.copy(view = in.view.copy(pressure = Pressure.Critical))).admitInHeap shouldBe calm.admitInHeap
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
