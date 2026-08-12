package bleep.analysis

import bleep.bsp.protocol.BleepBspProtocol
import bleep.requests.{RequestDiff, TranscriptStore}
import bleep.{model, BuildPaths}
import org.scalatest.concurrent.TimeLimits
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Seconds, Span}

import java.nio.file.Files

/** End-to-end proof that the daemon records request transcripts: two compiles through the REAL server (the same `MultiWorkspaceBspServer` production runs),
  * over the real protocol, must leave `requests/1.json` and `requests/2.json` in the workspace, return their ids in the responses, and — the flagship
  * determinism property — mechanically diff as identical, because the second compile is a noop and durations never enter the mechanical comparison.
  */
class TranscriptStoreIntegrationTest extends AnyFunSuite with Matchers with TimeLimits {

  val mediumTimeout: Span = Span(120, Seconds)

  test("daemon writes a transcript per compile request; responses carry the id; noop rerun diffs as identical") {
    failAfter(mediumTimeout) {
      val workspace = Files.createTempDirectory("bsp-transcript-store-")
      try {
        Files.createDirectories(workspace.resolve("src"))
        Files.writeString(
          workspace.resolve("src/Hello.scala"),
          """object Hello {
            |  def greeting: String = "Hello, transcripts!"
            |}
            |""".stripMargin
        )

        val config = BspTestHarness.ProjectConfig.scala(
          name = "myproject",
          sources = Set(workspace.resolve("src")),
          scalaVersion = "3.3.3",
          classpath = CompilerResolver.resolveScalaLibrary("3.3.3").toList,
          isTest = false
        )

        // Same derivation the server uses for this workspace (see BspTestBuild.payload).
        val buildPaths = BuildPaths(
          cwd = workspace,
          bleepYamlFile = workspace.resolve(bleep.BuildLoader.BuildFileName),
          variant = model.BuildVariant.Normal,
          wantedBleepVersion = Some(model.BleepVersion.current)
        )

        def requestIdOf(result: ch.epfl.scala.bsp.CompileResult): Long = {
          result.dataKind shouldBe Some(BleepBspProtocol.RequestIdDataKind)
          val raw = result.data.getOrElse(fail("compile response carried no data despite the request-id dataKind"))
          BleepBspProtocol.RequestIdPayload.decode(new String(raw.value, "UTF-8")) match {
            case Right(payload) => payload.requestId
            case Left(err)      => fail(s"could not decode request-id payload: ${err.getMessage}")
          }
        }

        BspTestHarness.withProject(workspace, config) { client =>
          client.initialize()
          val targets = client.buildTargets().targets.map(_.id)

          val first = client.compile(targets)
          first.statusCode shouldBe ch.epfl.scala.bsp.StatusCode.Ok
          requestIdOf(first) shouldBe 1L

          val second = client.compile(targets)
          second.statusCode shouldBe ch.epfl.scala.bsp.StatusCode.Ok
          requestIdOf(second) shouldBe 2L

          val third = client.compile(targets)
          third.statusCode shouldBe ch.epfl.scala.bsp.StatusCode.Ok
          requestIdOf(third) shouldBe 3L
        }

        // The files exist where every client will look for them.
        Files.isRegularFile(buildPaths.requestsDir.resolve("1.json")) shouldBe true
        Files.isRegularFile(buildPaths.requestsDir.resolve("2.json")) shouldBe true
        TranscriptStore.list(buildPaths) shouldBe List(1L, 2L, 3L)

        // Roundtrip through the store.
        val t1 = TranscriptStore.read(buildPaths, 1L)
        val t2 = TranscriptStore.read(buildPaths, 2L)
        val t3 = TranscriptStore.read(buildPaths, 3L)
        t1.id shouldBe 1L
        t2.id shouldBe 2L
        t1.mode shouldBe "compile"
        t1.targets shouldBe List("myproject")
        t1.client shouldBe "BspTestClient" // the displayName the harness sends in build/initialize
        t1.workspace shouldBe buildPaths.buildDir.toString

        // The first transcript really is the record of a compile: it explains why it compiled and how it ended.
        t1.events.collect { case e: BleepBspProtocol.Event.CompilationReason if e.project.value == "myproject" => e } should not be empty
        val finished1 = t1.events.collect { case e: BleepBspProtocol.Event.CompileFinished if e.project.value == "myproject" => e }
        finished1 should not be empty

        // A clean build and a noop are logically DIFFERENT runs, and the mechanical diff says so — as exactly one fact, the reason transition.
        val cleanVsNoop = RequestDiff.mechanical(t1, t2)
        cleanVsNoop.hcursor.get[Boolean]("identical") shouldBe Right(false)
        val reasonChange = cleanVsNoop.hcursor.downField("changed").downArray.downField("reason")
        reasonChange.get[String]("to") shouldBe Right("up-to-date")

        // Flagship determinism property, end-to-end through the real daemon: two noop runs have the same logical outcome,
        // so their mechanical diff is identical even though every duration differs between them.
        val noopVsNoop = RequestDiff.mechanical(t2, t3)
        noopVsNoop.hcursor.get[Boolean]("identical") shouldBe Right(true)
      } finally deleteRecursively(workspace)
    }
  }
}
