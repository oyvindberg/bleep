package bleep.bsp

import bleep.machine.{FileMachineLock, MachineLock, PressureThresholds, Probes, ServerDiscovery, ServerIdentity, Ticker}
import bleep.model.{BspServerConfig, MachineScheduling}
import bleep.{MemorySizes, UserPaths}
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
  def provisionalHeadroomMb(physicalMb: Long): Long = math.max(4096L, physicalMb / 8L)

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
  case class Selected(mode: Ticker.SchedulingMode, headroomMb: Long, reason: Option[String])

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
        Selected(Ticker.SchedulingMode.Unconstrained(reason), provisionalHeadroomMb(MemorySizes.physicalMemoryMb(fallbackMb = 0L)), Some(reason))

      case MachineScheduling.Cooperative =>
        val probes: Either[Throwable, Probes] =
          try Right(Probes.forThisMachine(userPaths.cacheDir.resolve("machine-probes")))
          catch { case t: Throwable => Left(t) }
        probes match {
          case Right(p) =>
            val physicalMb = p.machine.sample().physicalMb
            val mode = Ticker.SchedulingMode.Cooperative(
              machineProbe = p.machine,
              forkProbe = p.fork,
              thresholds = PressureThresholds.provisional,
              lock = new FileMachineLock(MachineLock.path(userPaths), identity, logger),
              lockWaitMs = LockWaitMs,
              ownSocketDir = ownSocketDir,
              discovery = new ServerDiscovery(userPaths.bspSocketDir, identity, () => System.currentTimeMillis(), DiscoveryListingTtlMs)
            )
            Selected(mode, provisionalHeadroomMb(physicalMb), None)
          case Left(t) =>
            val reason = s"bleep cannot measure this machine (${t.getClass.getSimpleName}: ${t.getMessage}), so this server schedules unconstrained: " +
              "its forks are bounded by parallelism alone, not by the machine's memory, and it does not coordinate with other bleep servers"
            logger.warn(reason)
            Selected(Ticker.SchedulingMode.Unconstrained(reason), provisionalHeadroomMb(MemorySizes.physicalMemoryMb(fallbackMb = 0L)), Some(reason))
        }
    }
}
