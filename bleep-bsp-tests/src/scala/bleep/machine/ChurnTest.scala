package bleep.machine

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** The compressor churn rate (design §9), driven with the owner's two overload runs and with the cadences the scheduler samples at. */
class ChurnTest extends AnyFunSuite with Matchers {
  private val t = PressureThresholds.provisional

  /** Feeds (seconds since start, pages/s held over the previous interval) as cumulative counters; returns the pressure after each sample. */
  private def drive(ratesPerInterval: List[(Double, Double)]): List[Pressure] = {
    var state: Option[Churn.State] = None
    var pages = 0L
    var lastS = 0.0
    ratesPerInterval.map { case (atS, rate) =>
      pages += math.round(rate * (atS - lastS))
      lastS = atS
      val next = Churn.update(state, math.round(atS * 1000.0), pages)
      state = Some(next)
      Pressure.normalise(RawPressure.MacOs(1, pages, 0L, 0L, 0L), t, next.rate)
    }
  }

  test("the first sample has no rate; the second has") {
    val s1 = Churn.update(None, 1000L, 10_000L)
    s1.rate shouldBe Churn.Rate.Unknown
    val s2 = Churn.update(Some(s1), 2000L, 30_000L)
    s2.rate shouldBe Churn.Rate.PagesPerSecond(20_000.0)
  }

  test("run 2: every row before free memory ran out is Normal, and the row where compression started is Elevated") {
    // held GB → churn/s per 7 s interval: 0.0 1.5k, 1.0 5.3k, 2.5 1.1k, 3.5 6.6k, 4.0 2.9k (free 0.06 GB), 4.5 40.5k (compression has started)
    val pressures = drive(List(0.0 -> 0.0, 7.0 -> 1_500.0, 14.0 -> 5_300.0, 21.0 -> 1_100.0, 28.0 -> 6_600.0, 35.0 -> 2_900.0, 42.0 -> 40_500.0))
    pressures.slice(1, 6) should contain only Pressure.Normal
    pressures.last shouldBe Pressure.Elevated
  }

  test("run 1: the cliff — 20k/s one interval, 243k/s the next — is Critical at the very next sample, not after a long average") {
    val pressures = drive(List(0.0 -> 0.0, 7.0 -> 20_000.0, 14.0 -> 243_000.0))
    pressures(1) shouldBe Pressure.Normal
    pressures(2) shouldBe Pressure.Critical
  }

  test("the first 90 % crossing, 98k/s, is Elevated; it is Critical from 100k/s") {
    drive(List(0.0 -> 0.0, 3.0 -> 98_000.0)).last shouldBe Pressure.Elevated
    drive(List(0.0 -> 0.0, 3.0 -> 100_000.0)).last shouldBe Pressure.Critical
  }

  test("at 10 ms ticks the rate is the mean over the window: a steady rate reads exactly, and a single burst does not lift a calm machine over the threshold") {
    var state: Option[Churn.State] = None
    var pages = 0L
    (0 to 300).foreach { i => pages += 50L; state = Some(Churn.update(state, i * 10L, pages)) } // 5k pages/s for 3 s
    val Churn.Rate.PagesPerSecond(steady) = state.get.rate: @unchecked
    steady shouldBe 5_000.0 +- 50.0
    state.get.samples.size should be <= 30 // thinned, not one per tick
    // One 10 ms burst of 20k pages (a 320 MB compression) on top: averaged over the window, still under 25k/s.
    pages += 20_000L
    state = Some(Churn.update(state, 3010L, pages))
    val Churn.Rate.PagesPerSecond(afterBurst) = state.get.rate: @unchecked
    afterBurst should be < 25_000.0
    afterBurst should be > 5_000.0
  }

  test("3 s slow checks, wider than the window, still rate every interval: the previous sample is the anchor") {
    var state: Option[Churn.State] = None
    (0 to 10).foreach { i => state = Some(Churn.update(state, i * 3000L, i * 300_000L)) } // 100k pages/s, every 3 s for 30 s
    val Churn.Rate.PagesPerSecond(slow) = state.get.rate: @unchecked
    slow shouldBe 100_000.0 +- 1.0
    state.get.samples.size shouldBe 2
  }

  test("a counter that went backwards is a new baseline: no rate until the next sample; a repeated instant rates nothing") {
    val s1 = Churn.update(None, 1000L, 50_000L)
    val s2 = Churn.update(Some(s1), 2000L, 60_000L)
    val wrapped = Churn.update(Some(s2), 3000L, 100L)
    wrapped.rate shouldBe Churn.Rate.Unknown
    Churn.update(Some(wrapped), 4000L, 10_100L).rate shouldBe Churn.Rate.PagesPerSecond(10_000.0)
    Churn.update(Some(s2), 2000L, 60_000L).rate shouldBe s2.rate
  }
}
