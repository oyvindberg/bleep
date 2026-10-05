package bleep.analysis

import bleep.bsp.TaskDag
import bleep.bsp.TaskDag.*
import bleep.machine.{ForkDemand, ForkKind, InHeap, InHeapKind, RequestId}
import bleep.model.{CrossProjectName, ProjectName}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** What each task asks the scheduler for follows one rule: work in the server's own process counts like a compile (a cpu slot, no machine room); a real process
  * is a fork that reports its pid and is measured. Links and discovery differ by platform, so the shape is checked per platform here, where it is a pure
  * function of the task.
  */
class DemandShapeTest extends AnyFunSuite with Matchers {
  private val project = CrossProjectName(ProjectName("app"), None)
  private val request = RequestId("r")
  private val heaps = ForkHeaps(sourcegenMb = 1000L, kspMb = 1100L, linkMb = 1200L)

  private val scalaJs = LinkPlatform.ScalaJs("1.16.0", "3.3.3", ScalaJsLinkConfig.Debug)
  private val scalaNative = LinkPlatform.ScalaNative("0.5.0", "3.3.3", ScalaNativeLinkConfig.Debug)
  private val kotlinJs =
    LinkPlatform.KotlinJs(
      "2.0.0",
      KotlinJsConfig(bleep.model.KotlinJsModuleKind.CommonJS, None, true, None, bleep.model.KotlinJsSourceMapEmbedSources.Never, false, false)
    )
  private val kotlinNative = LinkPlatform.KotlinNative("2.0.0", KotlinNativeConfig("linux-x64", true, false, false))

  private def linkDemand(platform: LinkPlatform) = TaskDag.demandFor(LinkTask(project, platform, releaseMode = false, isTest = false), heaps, request)
  private def discoverDemand(platform: Option[LinkPlatform]) = TaskDag.demandFor(DiscoverTask(project, platform), heaps, request)

  test("the Scala.js and Kotlin/JS linkers run in the server's heap: in-heap link demands") {
    List(scalaJs, kotlinJs).foreach { platform =>
      withClue(s"$platform: ") {
        linkDemand(platform) shouldBe Some(InHeap(request, bleep.machine.TaskId(s"link:${project.value}"), InHeapKind.Link, cpu = 1))
      }
    }
  }

  test("Kotlin/Native and Scala Native links run processes: fork demands at the link heap") {
    List(scalaNative, kotlinNative).foreach { platform =>
      withClue(s"$platform: ") {
        linkDemand(platform) match {
          case Some(d: ForkDemand) =>
            d.kind shouldBe ForkKind.Link
            d.boundMb shouldBe heaps.linkMb
            d.cpu shouldBe 1
          case other => fail(s"expected a fork demand, got $other")
        }
      }
    }
  }

  test("a JVM 'link' is a no-op in the server's heap") {
    linkDemand(LinkPlatform.Jvm) shouldBe Some(InHeap(request, bleep.machine.TaskId(s"link:${project.value}"), InHeapKind.Link, cpu = 1))
  }

  test("discovery by reflection — JVM, Scala.js, Scala Native — is in-heap work") {
    List(None, Some(LinkPlatform.Jvm), Some(scalaJs), Some(scalaNative)).foreach { platform =>
      withClue(s"$platform: ") {
        discoverDemand(platform) shouldBe Some(InHeap(request, bleep.machine.TaskId(s"discover:${project.value}"), InHeapKind.Discover, cpu = 1))
      }
    }
  }

  test("Kotlin/JS and Kotlin/Native discovery run the linked artifact: a fork, charged the listing-process bound until measured") {
    List(kotlinJs, kotlinNative).foreach { platform =>
      withClue(s"$platform: ") {
        discoverDemand(Some(platform)) match {
          case Some(d: ForkDemand) =>
            d.kind shouldBe ForkKind.Discover
            d.boundMb shouldBe TaskDag.ListingProcessBoundMb
            d.shared shouldBe false
          case other => fail(s"expected a fork demand, got $other")
        }
      }
    }
  }

  test("a grant of the wrong shape is a loud error, not a silent fallback") {
    an[IllegalStateException] should be thrownBy TaskGrant.forkFor(TaskGrant.InHeap, "a Kotlin/Native link")
  }
}
