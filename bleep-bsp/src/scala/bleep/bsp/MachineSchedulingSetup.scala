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

  /** The ceiling's headroom: `ceiling = physical − headroom` (design §5 rule 1).
    *
    * PROVISIONAL and owner-undecided (design §11, "Headroom"): `max(4 GB, RAM/8)` — enough for an OS, a browser and an IDE on a small machine, and a
    * proportionate share on a large one. The design's single tunable input, deliberately not a user setting yet. Defined here and nowhere else, so that
    * deciding it later is a one-line change.
    */
  /** What of the machine's available memory is never handed to forks (design §5 rule 1): the OS's own cushion before it starts compressing or swapping.
    * PROVISIONAL, CALIBRATED FROM ONE MACHINE: the owner's 48 GB Mac had 3–4 GB free + speculative at calm, and compression took off within one 512 MB step of
    * free pages reaching zero (§9). One gigabyte leaves the kernel that step. One named value, no user setting.
    */
  val ProvisionalReserveMb: Long = 1024L

  /** How long a claiming tick waits for `machine.lock` before deciding with `LockState.Unavailable` (design §8 point 5). */
  val LockWaitMs: Long = 1000L

  /** How long the tick's discovery trusts its listing of socket directories (design §6). */
  val DiscoveryListingTtlMs: Long = 1000L

  /** @param mode
    *   what the ticker runs with
    * @param headroomMb
    *   for `Params`: from the probes' physical memory in cooperative mode, from the JDK's figure otherwise (where it only feeds a display)
    * @param reason
    *   why the server runs unconstrained, when it does: the user's config, or the OS/architecture bleep cannot measure
    */
  case class Selected(mode: Ticker.SchedulingMode, reserveMb: Long, reason: Option[String])

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
        Selected(Ticker.SchedulingMode.Unconstrained(reason), ProvisionalReserveMb, Some(reason))

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
            Selected(mode, ProvisionalReserveMb, None)
          case Left(t) =>
            val reason = s"bleep cannot measure this machine (${t.getClass.getSimpleName}: ${t.getMessage}), so this server schedules unconstrained: " +
              "its forks are bounded by parallelism alone, not by the machine's memory, and it does not coordinate with other bleep servers"
            logger.warn(reason)
            Selected(Ticker.SchedulingMode.Unconstrained(reason), ProvisionalReserveMb, Some(reason))
        }
    }
}
