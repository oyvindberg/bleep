package bleep.bsp

import bleep.machine.{HeapUsage, HeapVerdict}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** HeapPressureGate as the scheduler's HeapGate: the same policy, over the scheduler's units. */
class HeapGateAdapterTest extends AnyFunSuite with Matchers {
  private val gate = HeapPressureGate.asHeapGate(() => 0.80)
  private val now = 10_000L

  test("a sole compile is admitted whatever the heap") {
    gate.verdict(HeapUsage(usedMb = 950L, maxMb = 1000L), othersCompiling = false, firstDeferredAtMs = None, nowMs = now) shouldBe HeapVerdict.Admit
  }

  test("with others compiling, a first look staggers by the gate's rule: the stagger scales with how close the heap is to the threshold") {
    gate.verdict(HeapUsage(400L, 1000L), othersCompiling = true, firstDeferredAtMs = None, nowMs = now) shouldBe HeapVerdict.Defer(1000L)
    gate.verdict(HeapUsage(800L, 1000L), othersCompiling = true, firstDeferredAtMs = None, nowMs = now) shouldBe HeapVerdict.Defer(2000L)
    gate.verdict(HeapUsage(10L, 1000L), othersCompiling = true, firstDeferredAtMs = None, nowMs = now) shouldBe HeapVerdict.Defer(200L)
  }

  test("once deferred, it is admitted as soon as the heap is below the threshold, and at the deadline regardless") {
    gate.verdict(HeapUsage(400L, 1000L), othersCompiling = true, firstDeferredAtMs = Some(now - 100L), nowMs = now) shouldBe HeapVerdict.Admit
    gate.verdict(HeapUsage(950L, 1000L), othersCompiling = true, firstDeferredAtMs = Some(now - 100L), nowMs = now) shouldBe HeapVerdict.Defer(2000L)
    gate.verdict(HeapUsage(950L, 1000L), othersCompiling = true, firstDeferredAtMs = Some(now - HeapPressureGate.MaxWaitMs), nowMs = now) shouldBe
      HeapVerdict.Admit
  }

  test("the threshold is read per call") {
    var threshold = 0.80
    val live = HeapPressureGate.asHeapGate(() => threshold)
    live.verdict(HeapUsage(700L, 1000L), othersCompiling = true, firstDeferredAtMs = Some(now - 1L), nowMs = now) shouldBe HeapVerdict.Admit
    threshold = 0.50
    live.verdict(HeapUsage(700L, 1000L), othersCompiling = true, firstDeferredAtMs = Some(now - 1L), nowMs = now) shouldBe HeapVerdict.Defer(2000L)
  }
}
