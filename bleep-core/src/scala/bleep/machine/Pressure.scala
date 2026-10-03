package bleep.machine

/** The OS's memory pressure, normalised across platforms. See `machine-scheduler-design.md` §9, "Pressure, normalised".
  *
  * `usedMb` against the ceiling predicts trouble; this catches the case where the prediction is fine but the machine is already reclaiming.
  */
sealed trait Pressure
object Pressure {

  /** Nothing to do. */
  case object Normal extends Pressure

  /** The OS is reclaiming. No admissions beyond guarantees. */
  case object Elevated extends Pressure

  /** The OS is stalling on memory. Additionally evict every idle fork. */
  case object Critical extends Pressure

  /** Maps a platform's raw pressure reading to [[Pressure]]. A reading outside what the platform documents throws: a scheduler guessing at an unknown level is
    * how the machine gets overcommitted.
    */
  def normalise(raw: RawPressure, thresholds: PressureThresholds): Pressure =
    raw match {
      case RawPressure.MacOs(level) =>
        // `kern.memorystatus_vm_pressure_level`: 1 normal, 2 warning, 4 critical. No thresholds — the kernel has already judged.
        level match {
          case 1     => Normal
          case 2     => Elevated
          case 4     => Critical
          case other => throw new IllegalArgumentException(s"macOS memory pressure level $other is not one of 1 (normal), 2 (warning), 4 (critical)")
        }

      case RawPressure.LinuxPsi(someAvg10, fullAvg10) =>
        requirePercent("PSI some avg10", someAvg10)
        requirePercent("PSI full avg10", fullAvg10)
        // `full` is the share of time in which *every* non-idle task was stalled on memory: the machine is doing nothing but reclaiming.
        if (fullAvg10 > 0.0) Critical
        else if (someAvg10 > thresholds.linuxPsiSomeAvg10ElevatedPercent) Elevated
        else Normal

      case RawPressure.Windows(memoryLoadPercent, commitTotalMb, commitLimitMb, lowMemory) =>
        if (memoryLoadPercent < 0 || memoryLoadPercent > 100)
          throw new IllegalArgumentException(s"Windows memory load $memoryLoadPercent% is outside 0–100")
        if (commitLimitMb <= 0L)
          throw new IllegalArgumentException(s"Windows commit limit ${commitLimitMb}MB is not positive")
        if (commitTotalMb < 0L)
          throw new IllegalArgumentException(s"Windows commit total ${commitTotalMb}MB is negative")
        // The low-memory resource notification is the kernel's own "critical", analogous to macOS level 4 and PSI `full`.
        if (lowMemory) Critical
        else {
          val commitFraction = commitTotalMb.toDouble / commitLimitMb.toDouble
          if (memoryLoadPercent > thresholds.windowsMemoryLoadElevatedPercent || commitFraction > thresholds.windowsCommitElevatedFraction) Elevated
          else Normal
        }
    }

  private def requirePercent(what: String, value: Double): Unit =
    if (value.isNaN || value < 0.0 || value > 100.0) throw new IllegalArgumentException(s"$what is $value, not a percentage in 0–100")
}

/** The thresholds [[Pressure.normalise]] applies where the platform does not judge for us. macOS has none: the kernel reports a level.
  *
  * Every value here is OPEN (design §11): none has been measured on a real machine yet. They are parameters precisely so that the unmeasured numbers are
  * visible at every call site and in `top`, rather than constants buried in a match.
  *
  * @param linuxPsiSomeAvg10ElevatedPercent
  *   Linux: `some avg10` above this is `Elevated`. Design §9 estimates ≈10 %, to be measured.
  * @param windowsMemoryLoadElevatedPercent
  *   Windows: `dwMemoryLoad` above this is `Elevated`. Design §9 estimates ≈90 %.
  * @param windowsCommitElevatedFraction
  *   Windows: committed / commit limit above this is `Elevated` ("commit near limit"). Unmeasured.
  */
case class PressureThresholds(
    linuxPsiSomeAvg10ElevatedPercent: Double,
    windowsMemoryLoadElevatedPercent: Int,
    windowsCommitElevatedFraction: Double
) {
  require(
    linuxPsiSomeAvg10ElevatedPercent >= 0.0 && linuxPsiSomeAvg10ElevatedPercent <= 100.0,
    s"PSI threshold $linuxPsiSomeAvg10ElevatedPercent is not a percentage"
  )
  require(
    windowsMemoryLoadElevatedPercent >= 0 && windowsMemoryLoadElevatedPercent <= 100,
    s"memory load threshold $windowsMemoryLoadElevatedPercent is not a percentage"
  )
  require(
    windowsCommitElevatedFraction >= 0.0 && windowsCommitElevatedFraction <= 1.0,
    s"commit fraction threshold $windowsCommitElevatedFraction is not in 0–1"
  )
}

object PressureThresholds {

  /** The design's estimates, pending measurement (§11). Named "provisional" so no call site mistakes them for tuned values. */
  val provisional: PressureThresholds = PressureThresholds(
    linuxPsiSomeAvg10ElevatedPercent = 10.0,
    windowsMemoryLoadElevatedPercent = 90,
    windowsCommitElevatedFraction = 0.90
  )
}
