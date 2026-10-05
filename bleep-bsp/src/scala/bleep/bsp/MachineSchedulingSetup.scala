package bleep.bsp

import bleep.machine.{FileMachineLock, MachineLock, PressureThresholds, Probes, ServerDiscovery, ServerIdentity, Ticker}
import bleep.model.{BspServerConfig, MachineScheduling}
import bleep.UserPaths
import ryddig.Logger

import java.nio.file.Path

/** How this compile server takes part in machine-wide scheduling: the user's `machineScheduling` setting, and whether bleep can measure this machine at all
  * (design §9.1).
  */
object MachineSchedulingSetup {

  /** Where available memory is a measure of room (Linux, Windows — `RoomBasis.AvailableMemory`): what of it is never handed to forks (design §5 rule 1), the
    * OS's own cushion before it starts reclaiming. PROVISIONAL and UNMEASURED on those platforms: one gigabyte, carried over from the macOS allocation runs
    * that have since shown available to be no measure at all there (§9). One named value, no user setting; not consulted on macOS.
    */
  val ProvisionalReserveMb: Long = 1024L

  /** Where available memory is no measure of room (macOS — `RoomBasis.StartingForksCap`): how many forks may be unmeasured at once across every server before a
    * fork beyond the guarantee waits (design §5 rule 1). ONE: a fork is measured a second after it starts, and the compressor's churn — the brake that does
    * work on macOS — needs that second to show what the last fork cost before the next is let in. A count rather than megabytes because there is no one
    * "largest fork bound": bounds come from each project's test heap and jvmOptions, so a megabyte cap would admit two small forks or no large one, and neither
    * is the intent. Calibrated from the owner's one-hour log (§9); one named value, no user setting; not consulted where room is measured.
    */
  val ProvisionalMaxStartingForks: Int = 1

  /** How long a claiming tick waits for `machine.lock` before deciding with `LockState.Unavailable` (design §8 point 5). */
  val LockWaitMs: Long = 1000L

  /** How long the tick's discovery trusts its listing of socket directories (design §6). */
  val DiscoveryListingTtlMs: Long = 1000L

  /** @param mode
    *   what the ticker runs with
    * @param reserveMb
    *   for `Params`: [[ProvisionalReserveMb]], consulted where the probe says available memory measures room
    * @param maxStartingForks
    *   for `Params`: [[ProvisionalMaxStartingForks]], consulted where it says it does not
    * @param reason
    *   why the server runs unconstrained, when it does: the user's config, or the OS/architecture bleep cannot measure
    */
  case class Selected(mode: Ticker.SchedulingMode, reserveMb: Long, maxStartingForks: Int, reason: Option[String])

  /** Choose the mode for this server.
    *
    * `cooperative` in the config means cooperative if the probes can run here. Where they cannot — no probe library for this OS and architecture, or a library
    * that fails its first reading — the server runs unconstrained and says so loudly, because a server that refused to start would be worse than one that
    * schedules on its own, and a server that pretended to measure would be worse than both. A missing pressure source alone does not change the mode: the
    * probes report it and the scheduler runs without the brake (`Pressure.NoSignal`).
    *
    * @param ownSocketDir
    *   this daemon's socket directory, where its `state.json` goes
    */
  def select(config: BspServerConfig, userPaths: UserPaths, ownSocketDir: Path, identity: ServerIdentity, logger: Logger): Selected =
    config.effectiveMachineScheduling match {
      case MachineScheduling.Unconstrained =>
        val reason = "machineScheduling is `unconstrained` in the user config"
        Selected(Ticker.SchedulingMode.Unconstrained(reason), ProvisionalReserveMb, ProvisionalMaxStartingForks, Some(reason))

      case MachineScheduling.Cooperative =>
        val probes: Either[Throwable, Probes] =
          try Right(Probes.forThisMachine(userPaths.cacheDir.resolve("machine-probes")))
          catch { case t: Throwable => Left(t) }
        probes match {
          case Right(p) =>
            val mode = Ticker.SchedulingMode.Cooperative(
              machineProbe = p.machine,
              forkProbe = p.fork,
              thresholds = PressureThresholds.provisional,
              lock = new FileMachineLock(MachineLock.path(userPaths), identity, logger),
              lockWaitMs = LockWaitMs,
              ownSocketDir = ownSocketDir,
              discovery = new ServerDiscovery(userPaths.bspSocketDir, identity, () => System.currentTimeMillis(), DiscoveryListingTtlMs)
            )
            Selected(mode, ProvisionalReserveMb, ProvisionalMaxStartingForks, None)
          case Left(t) =>
            val reason = s"bleep cannot measure this machine (${t.getClass.getSimpleName}: ${t.getMessage}), so this server schedules unconstrained: " +
              "its forks are bounded by parallelism alone, not by the machine's memory, and it does not coordinate with other bleep servers"
            logger.warn(reason)
            Selected(Ticker.SchedulingMode.Unconstrained(reason), ProvisionalReserveMb, ProvisionalMaxStartingForks, Some(reason))
        }
    }
}
