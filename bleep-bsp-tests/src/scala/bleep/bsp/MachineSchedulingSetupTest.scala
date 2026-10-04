package bleep.bsp

import bleep.UserPaths
import bleep.machine.{FileMachineLock, StateFile, Ticker}
import bleep.model.{BspServerConfig, MachineScheduling}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import ryddig.TypedLogger

import java.nio.file.Files

/** Which scheduling mode a server starts in (design §9.1). */
class MachineSchedulingSetupTest extends AnyFunSuite with Matchers {
  private def userPaths = {
    val root = Files.createTempDirectory("bleep-scheduling-setup")
    UserPaths(cacheDir = root.resolve("cache"), configDir = root.resolve("config"))
  }
  private val identity = StateFile.selfIdentity("test")

  test("the provisional headroom is max(4 GB, RAM/8), in one place") {
    MachineSchedulingSetup.provisionalHeadroomMb(8 * 1024L) shouldBe 4096L
    MachineSchedulingSetup.provisionalHeadroomMb(48 * 1024L) shouldBe 6144L
    MachineSchedulingSetup.provisionalHeadroomMb(256 * 1024L) shouldBe 32768L
  }

  test("`unconstrained` in the config runs unconstrained, and says why") {
    val selected =
      MachineSchedulingSetup.select(
        BspServerConfig.default.copy(machineScheduling = Some(MachineScheduling.Unconstrained)),
        userPaths,
        Files.createTempDirectory("own"),
        identity,
        TypedLogger.DevNull
      )
    selected.mode shouldBe a[Ticker.SchedulingMode.Unconstrained]
    selected.reason.get should include("user config")
  }

  test("`cooperative` on a machine bleep can measure runs cooperatively, with the lock in the cache dir and the probes' physical memory behind the headroom") {
    val paths = userPaths
    val selected = MachineSchedulingSetup.select(BspServerConfig.default, paths, Files.createTempDirectory("own"), identity, TypedLogger.DevNull)
    selected.reason shouldBe None
    selected.mode match {
      case coop: Ticker.SchedulingMode.Cooperative =>
        try {
          val physical = coop.machineProbe.sample().physicalMb
          selected.headroomMb shouldBe MachineSchedulingSetup.provisionalHeadroomMb(physical)
          Files.exists(paths.cacheDir.resolve("machine.lock")) shouldBe true
          coop.forkProbe.footprintMb(ProcessHandle.current().pid()).isDefined shouldBe true
        } finally coop.lock.asInstanceOf[FileMachineLock].close()
      case other => fail(s"expected Cooperative on this machine, got $other")
    }
  }
}
