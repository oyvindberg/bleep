package bleep

import bleep.testing.ForkCostModel
import cats.effect.unsafe.implicits.global
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

/** Charging a fork what forks of its kind were measured to cost, instead of the ceiling its `-Xmx` allows.
  *
  * The gap is not marginal: measured on a real build, the median fork cost 610MB while being charged 2560MB, so the budget "filled up" at roughly a quarter of
  * the machine's real capacity.
  */
class ForkCostModelTest extends AnyFunSuite with Matchers {

  test("an unmeasured fork is charged its bound's footprint; a measured one is charged what it cost") {
    val prog = for {
      m <- ForkCostModel.create
      // Nothing observed yet — fall back to the ceiling the JVM is held to.
      cold <- m.estimateMb("suite-a", heapBoundMb = 2048)
      _ = cold shouldBe MachineResources.forkFootprintMb(2048)
      _ = cold shouldBe 2560L
      // One fork of this kind ran and really cost 610MB.
      _ <- m.observe("suite-a", 610)
      warm <- m.estimateMb("suite-a", heapBoundMb = 2048)
    } yield {
      warm shouldBe 610L
      warm should be < cold // the whole point: ~4x more forks fit
    }
    prog.timeout(10.seconds).unsafeRunSync()
  }

  test("estimates only ever go up") {
    // Deliberate asymmetry: under-charging risks over-committing the machine, over-charging costs
    // only some parallelism. A suite that spiked once is assumed able to spike again.
    val prog = for {
      m <- ForkCostModel.create
      _ <- m.observe("k", 800)
      _ <- m.observe("k", 3000)
      _ <- m.observe("k", 500) // a lighter run must not erase the spike
      est <- m.estimateMb("k", heapBoundMb = 2048)
    } yield est shouldBe 3000L
    prog.timeout(10.seconds).unsafeRunSync()
  }

  test("a fork that measured tiny is still charged the floor") {
    // A JVM that looked small once can still spike, and admitting unboundedly many "free" forks
    // would defeat the budget entirely.
    val prog = for {
      m <- ForkCostModel.create
      _ <- m.observe("tiny", 12)
      est <- m.estimateMb("tiny", heapBoundMb = 2048)
    } yield est shouldBe ForkCostModel.FloorMb
    prog.timeout(10.seconds).unsafeRunSync()
  }

  test("nonsense observations are ignored rather than poisoning the estimate") {
    val prog = for {
      m <- ForkCostModel.create
      _ <- m.observe("k", 700)
      _ <- m.observe("k", 0) // process already gone / platform said nothing
      _ <- m.observe("k", -5)
      est <- m.estimateMb("k", heapBoundMb = 2048)
    } yield est shouldBe 700L
    prog.timeout(10.seconds).unsafeRunSync()
  }

  test("the static model keeps the pre-measurement behaviour, for platforms that can't measure") {
    val prog = for {
      _ <- ForkCostModel.static.observe("k", 610)
      est <- ForkCostModel.static.estimateMb("k", heapBoundMb = 2048)
      learned <- ForkCostModel.static.learned
    } yield {
      est shouldBe MachineResources.forkFootprintMb(2048) // observation changed nothing
      learned shouldBe empty
    }
    prog.timeout(10.seconds).unsafeRunSync()
  }

  test("macOS footprint output parses, including units other than MB") {
    ProcessMemory.MacOs.parseSize("    phys_footprint: 542 MB") shouldBe Some(542L)
    ProcessMemory.MacOs.parseSize("phys_footprint_peak: 1645 MB") shouldBe Some(1645L)
    ProcessMemory.MacOs.parseSize("phys_footprint: 1.6 GB") shouldBe Some(1638L)
    ProcessMemory.MacOs.parseSize("phys_footprint: 8192 KB") shouldBe Some(8L)
    ProcessMemory.MacOs.parseSize("phys_footprint: not-a-number") shouldBe None
  }

  test("the platform reader measures a real process, or honestly declines to") {
    // Against this very JVM. Either the platform can measure it — in which case the number must be
    // plausible, not a parse artifact — or it says None, which is the contract on Windows.
    val self = ProcessHandle.current().pid()
    ProcessMemory.system.footprintMb(self) match {
      case Some(mb) =>
        mb should be > 0L
        mb should be < 200000L // sanity: not a misparsed byte count
      case None =>
        ProcessMemory.system shouldBe ProcessMemory.Unavailable
    }
  }
}
