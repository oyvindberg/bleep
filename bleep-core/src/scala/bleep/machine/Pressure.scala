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

  /** The platform has no pressure source (`RawPressure.Unavailable`): the brake is off, nothing else changes (design §9.1). A case of [[Pressure]] rather than
    * a separate flag, so that every match on the pressure is forced to say what it does without a signal, and no flag can disagree with a level. `reason` is
    * for the one-time startup warning and for `top`.
    */
  case class NoSignal(reason: String) extends Pressure

  /** The signal exists but has not settled yet: macOS's churn needs two samples before it is a rate. The brake is off, as under `NoSignal`, but this is a
    * statement about the first second of a server's life, not about the platform — so it is its own case and never warns.
    */
  case class Warming(reason: String) extends Pressure

  /** The level as `top`, the log and `state.json` readers spell it. */
  def name(p: Pressure): String = p match {
    case Normal      => "normal"
    case Elevated    => "elevated"
    case Critical    => "critical"
    case NoSignal(_) => "no signal"
    case Warming(_)  => "warming up"
  }

  /** Whether the brake is on at all: `Elevated` and `Critical` withhold new forks beyond guarantees. */
  def withholdsNewForks(p: Pressure): Boolean = p match {
    case Normal | NoSignal(_) | Warming(_) => false
    case Elevated | Critical               => true
  }

  private def rank(p: Pressure): Int = p match {
    case Warming(_) | NoSignal(_) => 0
    case Normal                   => 1
    case Elevated                 => 2
    case Critical                 => 3
  }

  /** Maps a platform's raw pressure reading to [[Pressure]]. A reading outside what the platform documents throws: a scheduler guessing at an unknown level is
    * how the machine gets overcommitted.
    */
  def normalise(raw: RawPressure, thresholds: PressureThresholds, churn: Churn.Rate): Pressure =
    raw match {
      case RawPressure.Unavailable(reason) => NoSignal(reason)

      case mac: RawPressure.MacOs =>
        // The kernel's level is a floor — 2 is at least Elevated, 4 is Critical — and the compressor's churn is what moves first (design §9): either is
        // enough. With one sample the churn has no rate yet and only the level speaks; at level 1 that is "warming up", not "normal".
        val fromLevel = mac.level match {
          case 1     => None
          case 2     => Some(Elevated)
          case 4     => Some(Critical)
          case other => throw new IllegalArgumentException(s"macOS memory pressure level $other is not one of 1 (normal), 2 (warning), 4 (critical)")
        }
        val fromChurn = churn match {
          case Churn.Rate.Unknown                                                               => Warming("compressor churn needs two samples to be a rate")
          case Churn.Rate.PagesPerSecond(r) if r >= thresholds.macOsChurnCriticalPagesPerSecond => Critical
          case Churn.Rate.PagesPerSecond(r) if r >= thresholds.macOsChurnElevatedPagesPerSecond => Elevated
          case Churn.Rate.PagesPerSecond(_)                                                     => Normal
        }
        fromLevel match {
          case Some(level) => if (rank(level) >= rank(fromChurn)) level else fromChurn
          case None        => fromChurn
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

/** The thresholds [[Pressure.normalise]] applies where the platform does not judge for us.
  *
  * The macOS churn values are CALIBRATED FROM ONE TEST — the owner's 48 GB Mac driven to overload, with kernel_task CPU as the ground truth (design §9): churn
  * 98k pages/s at 95 % kernel_task, 236k at 111 %, 338k at 160 %, under 50k calm. The others are OPEN (design §11): not measured on a real machine yet. All are
  * parameters precisely so that the numbers are visible at every call site and in `top`, rather than constants buried in a match.
  *
  * @param macOsChurnElevatedPagesPerSecond
  *   macOS: compressions + decompressions per second at or above this is `Elevated` — below the 98k/s seen at the first overload reading
  * @param macOsChurnCriticalPagesPerSecond
  *   macOS: at or above this is `Critical` — between the 98k/s of a just-overloaded machine and the 236k/s of one well past it
  *
  * @param linuxPsiSomeAvg10ElevatedPercent
  *   Linux: `some avg10` above this is `Elevated`. Design §9 estimates ≈10 %, to be measured.
  * @param windowsMemoryLoadElevatedPercent
  *   Windows: `dwMemoryLoad` above this is `Elevated`. Design §9 estimates ≈90 %.
  * @param windowsCommitElevatedFraction
  *   Windows: committed / commit limit above this is `Elevated` ("commit near limit"). Unmeasured.
  */
case class PressureThresholds(
    macOsChurnElevatedPagesPerSecond: Double,
    macOsChurnCriticalPagesPerSecond: Double,
    linuxPsiSomeAvg10ElevatedPercent: Double,
    windowsMemoryLoadElevatedPercent: Int,
    windowsCommitElevatedFraction: Double
) {
  require(macOsChurnElevatedPagesPerSecond > 0.0, s"macOS churn elevated threshold $macOsChurnElevatedPagesPerSecond must be positive")
  require(
    macOsChurnCriticalPagesPerSecond > macOsChurnElevatedPagesPerSecond,
    s"macOS churn critical threshold $macOsChurnCriticalPagesPerSecond must exceed the elevated one $macOsChurnElevatedPagesPerSecond"
  )
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
    macOsChurnElevatedPagesPerSecond = 75_000.0,
    macOsChurnCriticalPagesPerSecond = 200_000.0,
    linuxPsiSomeAvg10ElevatedPercent = 10.0,
    windowsMemoryLoadElevatedPercent = 90,
    windowsCommitElevatedFraction = 0.90
  )
}
