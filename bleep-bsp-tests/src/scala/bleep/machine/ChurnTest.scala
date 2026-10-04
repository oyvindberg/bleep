package bleep.machine

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** The compressor churn average (design §9), driven with the readings from the owner's overload test and with the cadences the scheduler samples at. */
class ChurnTest extends AnyFunSuite with Matchers {
  private val t = PressureThresholds.provisional

  /** Feeds a sequence of (seconds since start, pages/s held over the previous interval) as cumulative counters; returns the pressure after each sample. */
  private def drive(ratesPerInterval: List[(Long, Double)]): List[Pressure] = {
    var state: Option[Churn.State] = None
    var pages = 0L
    var lastS = 0L
    ratesPerInterval.map { case (atS, rate) =>
      pages += math.round(rate * (atS - lastS))
      lastS = atS
      val next = Churn.update(state, atS * 1000L, pages)
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

  test("a calm machine stays Normal: the owner's calm readings, sampled every seven seconds") {
    // calm: 6k–50k pages/s, level 1
    val samples = List(0L -> 0.0, 7L -> 6_000.0, 14L -> 50_000.0, 21L -> 30_000.0, 28L -> 48_000.0, 35L -> 12_000.0, 42L -> 50_000.0)
    drive(samples).drop(1) should contain only Pressure.Normal
  }

  test("the first overload reading, 98k pages/s, becomes Elevated within a few samples and stays there") {
    val samples = List(0L -> 0.0, 7L -> 40_000.0, 14L -> 98_000.0, 21L -> 98_000.0, 28L -> 98_000.0, 35L -> 98_000.0)
    val pressures = drive(samples)
    pressures.last shouldBe Pressure.Elevated
    // Smoothing: one 7 s interval at 98k over a 40k baseline does not yet cross 75k (alpha ≈ 0.50 → ≈69k); the next does.
    pressures(2) shouldBe Pressure.Normal
    pressures(3) shouldBe Pressure.Elevated
  }

  test("a machine well past overload — 236k and up — is Critical, and the kernel's level 2 arriving late changes nothing") {
    val samples = List(0L -> 0.0, 7L -> 50_000.0, 14L -> 236_000.0, 21L -> 338_000.0, 28L -> 411_000.0)
    drive(samples).last shouldBe Pressure.Critical
  }

  test("the average is weighted by the interval, so 10 ms ticks and 3 s slow checks agree on a steady rate") {
    var fast: Option[Churn.State] = None
    var slow: Option[Churn.State] = None
    (0 to 3000).foreach { i => fast = Some(Churn.update(fast, i * 10L, i * 1_000L)) } // 100k pages/s, every 10 ms for 30 s
    (0 to 10).foreach { i => slow = Some(Churn.update(slow, i * 3000L, i * 300_000L)) } // 100k pages/s, every 3 s for 30 s
    val Churn.Rate.PagesPerSecond(f) = fast.get.rate: @unchecked
    val Churn.Rate.PagesPerSecond(s) = slow.get.rate: @unchecked
    f shouldBe 100_000.0 +- 1.0
    s shouldBe 100_000.0 +- 1.0
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
