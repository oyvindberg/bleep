package bleep.machine

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** The per-OS mapping of raw pressure to Normal/Elevated/Critical (design §9). */
class PressureTest extends AnyFunSuite with Matchers {
  private val t = PressureThresholds(
    macOsChurnElevatedPagesPerSecond = 75_000.0,
    macOsChurnCriticalPagesPerSecond = 150_000.0,
    linuxPsiSomeAvg10ElevatedPercent = 10.0,
    windowsMemoryLoadElevatedPercent = 90,
    windowsCommitElevatedFraction = 0.9
  )
  private val calm = Churn.Rate.PagesPerSecond(20_000.0)

  test("macOS: churn at or above the thresholds is Elevated then Critical, whatever the kernel's level says") {
    // The one-hour log's medians: calm 3k (p90 41k), kernel_task 50–90 % 125k, above 90 % 249k.
    Pressure.normalise(RawPressure.MacOs(1, 0L, 0L, 0L, 0L), t, Churn.Rate.PagesPerSecond(3_000.0)) shouldBe Pressure.Normal
    Pressure.normalise(RawPressure.MacOs(1, 0L, 0L, 0L, 0L), t, Churn.Rate.PagesPerSecond(41_000.0)) shouldBe Pressure.Normal
    Pressure.normalise(RawPressure.MacOs(1, 0L, 0L, 0L, 0L), t, Churn.Rate.PagesPerSecond(74_999.0)) shouldBe Pressure.Normal
    Pressure.normalise(RawPressure.MacOs(1, 0L, 0L, 0L, 0L), t, Churn.Rate.PagesPerSecond(75_000.0)) shouldBe Pressure.Elevated
    Pressure.normalise(RawPressure.MacOs(1, 0L, 0L, 0L, 0L), t, Churn.Rate.PagesPerSecond(125_000.0)) shouldBe Pressure.Elevated
    Pressure.normalise(RawPressure.MacOs(1, 0L, 0L, 0L, 0L), t, Churn.Rate.PagesPerSecond(149_999.0)) shouldBe Pressure.Elevated
    Pressure.normalise(RawPressure.MacOs(1, 0L, 0L, 0L, 0L), t, Churn.Rate.PagesPerSecond(150_000.0)) shouldBe Pressure.Critical
    Pressure.normalise(RawPressure.MacOs(1, 0L, 0L, 0L, 0L), t, Churn.Rate.PagesPerSecond(249_000.0)) shouldBe Pressure.Critical
    // The level is a floor: 2 keeps Elevated under calm churn, and does not hold Critical churn down.
    Pressure.normalise(RawPressure.MacOs(2, 0L, 0L, 0L, 0L), t, Churn.Rate.PagesPerSecond(236_000.0)) shouldBe Pressure.Critical
  }

  test("macOS: with one sample the churn has no rate — warming up, not Normal — unless the level already says more") {
    Pressure.normalise(RawPressure.MacOs(1, 0L, 0L, 0L, 0L), t, Churn.Rate.Unknown) shouldBe a[Pressure.Warming]
    Pressure.withholdsNewForks(Pressure.normalise(RawPressure.MacOs(1, 0L, 0L, 0L, 0L), t, Churn.Rate.Unknown)) shouldBe false
    Pressure.normalise(RawPressure.MacOs(2, 0L, 0L, 0L, 0L), t, Churn.Rate.Unknown) shouldBe Pressure.Elevated
    Pressure.normalise(RawPressure.MacOs(4, 0L, 0L, 0L, 0L), t, Churn.Rate.Unknown) shouldBe Pressure.Critical
  }

  test("macOS: level 1 is Normal, 2 is Elevated, 4 is Critical") {
    Pressure.normalise(RawPressure.MacOs(1, 0L, 0L, 0L, 0L), t, calm) shouldBe Pressure.Normal
    Pressure.normalise(RawPressure.MacOs(2, 0L, 0L, 0L, 0L), t, calm) shouldBe Pressure.Elevated
    Pressure.normalise(RawPressure.MacOs(4, 0L, 0L, 0L, 0L), t, calm) shouldBe Pressure.Critical
  }

  test("macOS: an undocumented level throws rather than being guessed at") {
    List(0, 3, 5, -1).foreach { level =>
      an[IllegalArgumentException] should be thrownBy Pressure.normalise(RawPressure.MacOs(level, 0L, 0L, 0L, 0L), t, calm)
    }
  }

  test("Linux: `full avg10` above zero is Critical regardless of `some`") {
    Pressure.normalise(RawPressure.LinuxPsi(someAvg10 = 0.0, fullAvg10 = 0.01), t, Churn.Rate.Unknown) shouldBe Pressure.Critical
    Pressure.normalise(RawPressure.LinuxPsi(someAvg10 = 50.0, fullAvg10 = 3.0), t, Churn.Rate.Unknown) shouldBe Pressure.Critical
  }

  test("Linux: `some avg10` strictly above the threshold is Elevated, at or below is Normal") {
    Pressure.normalise(RawPressure.LinuxPsi(someAvg10 = 10.0, fullAvg10 = 0.0), t, Churn.Rate.Unknown) shouldBe Pressure.Normal
    Pressure.normalise(RawPressure.LinuxPsi(someAvg10 = 10.01, fullAvg10 = 0.0), t, Churn.Rate.Unknown) shouldBe Pressure.Elevated
    Pressure.normalise(RawPressure.LinuxPsi(someAvg10 = 0.0, fullAvg10 = 0.0), t, Churn.Rate.Unknown) shouldBe Pressure.Normal
  }

  test("Linux: the threshold is a parameter, not a constant") {
    val strict = t.copy(linuxPsiSomeAvg10ElevatedPercent = 1.0)
    Pressure.normalise(RawPressure.LinuxPsi(someAvg10 = 5.0, fullAvg10 = 0.0), strict, Churn.Rate.Unknown) shouldBe Pressure.Elevated
    Pressure.normalise(RawPressure.LinuxPsi(someAvg10 = 5.0, fullAvg10 = 0.0), t, Churn.Rate.Unknown) shouldBe Pressure.Normal
  }

  test("Linux: a PSI value outside 0–100 throws") {
    an[IllegalArgumentException] should be thrownBy Pressure.normalise(RawPressure.LinuxPsi(someAvg10 = 101.0, fullAvg10 = 0.0), t, Churn.Rate.Unknown)
    an[IllegalArgumentException] should be thrownBy Pressure.normalise(RawPressure.LinuxPsi(someAvg10 = -1.0, fullAvg10 = 0.0), t, Churn.Rate.Unknown)
    an[IllegalArgumentException] should be thrownBy Pressure.normalise(RawPressure.LinuxPsi(someAvg10 = Double.NaN, fullAvg10 = 0.0), t, Churn.Rate.Unknown)
  }

  test("Windows: the low-memory notification is Critical regardless of load") {
    Pressure.normalise(
      RawPressure.Windows(memoryLoadPercent = 10, commitTotalMb = 100, commitLimitMb = 1000, lowMemory = true),
      t,
      Churn.Rate.Unknown
    ) shouldBe Pressure.Critical
  }

  test("Windows: memory load above the threshold is Elevated") {
    Pressure.normalise(
      RawPressure.Windows(memoryLoadPercent = 91, commitTotalMb = 100, commitLimitMb = 1000, lowMemory = false),
      t,
      Churn.Rate.Unknown
    ) shouldBe Pressure.Elevated
    Pressure.normalise(
      RawPressure.Windows(memoryLoadPercent = 90, commitTotalMb = 100, commitLimitMb = 1000, lowMemory = false),
      t,
      Churn.Rate.Unknown
    ) shouldBe Pressure.Normal
  }

  test("Windows: commit near the limit is Elevated") {
    Pressure.normalise(
      RawPressure.Windows(memoryLoadPercent = 50, commitTotalMb = 950, commitLimitMb = 1000, lowMemory = false),
      t,
      Churn.Rate.Unknown
    ) shouldBe Pressure.Elevated
    Pressure.normalise(
      RawPressure.Windows(memoryLoadPercent = 50, commitTotalMb = 900, commitLimitMb = 1000, lowMemory = false),
      t,
      Churn.Rate.Unknown
    ) shouldBe Pressure.Normal
  }

  test("Windows: an impossible reading throws") {
    an[IllegalArgumentException] should be thrownBy Pressure.normalise(RawPressure.Windows(101, 1, 10, lowMemory = false), t, Churn.Rate.Unknown)
    an[IllegalArgumentException] should be thrownBy Pressure.normalise(RawPressure.Windows(50, 1, 0, lowMemory = false), t, Churn.Rate.Unknown)
    an[IllegalArgumentException] should be thrownBy Pressure.normalise(RawPressure.Windows(50, -1, 10, lowMemory = false), t, Churn.Rate.Unknown)
  }

  test("a platform without a pressure source normalises to NoSignal with its reason, and only Elevated and Critical withhold new forks") {
    Pressure.normalise(RawPressure.Unavailable("kernel without PSI"), t, Churn.Rate.Unknown) shouldBe Pressure.NoSignal("kernel without PSI")
    Pressure.withholdsNewForks(Pressure.NoSignal("kernel without PSI")) shouldBe false
    Pressure.withholdsNewForks(Pressure.Normal) shouldBe false
    Pressure.withholdsNewForks(Pressure.Elevated) shouldBe true
    Pressure.withholdsNewForks(Pressure.Critical) shouldBe true
  }

  test("thresholds that are not percentages or fractions are rejected on construction") {
    an[IllegalArgumentException] should be thrownBy PressureThresholds(75_000.0, 150_000.0, 101.0, 90, 0.9)
    an[IllegalArgumentException] should be thrownBy PressureThresholds(75_000.0, 150_000.0, 10.0, 101, 0.9)
    an[IllegalArgumentException] should be thrownBy PressureThresholds(75_000.0, 150_000.0, 10.0, 90, 1.5)
    // The churn thresholds must be positive and ordered, or Critical could never follow Elevated.
    an[IllegalArgumentException] should be thrownBy PressureThresholds(0.0, 150_000.0, 10.0, 90, 0.9)
    an[IllegalArgumentException] should be thrownBy PressureThresholds(150_000.0, 75_000.0, 10.0, 90, 0.9)
  }
}
