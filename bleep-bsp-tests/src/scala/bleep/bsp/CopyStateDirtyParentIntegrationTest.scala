package bleep.bsp

import bleep.analysis.{deleteRecursively, BspTestBuild, BspTestHarness, CompilerResolver, ScalaConfig}
import bleep.bsp.protocol.{BleepServerAdmin, CopyStateResponse}
import bleep.model
import org.scalatest.concurrent.TimeLimits
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Seconds, Span}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

/** `bleep/copyState` with a DIRTY parent, end-to-end through real compiles.
  *
  * The scenario copy-state exists for is seeding a fresh worktree from its parent — but the parent workspace is a working tree, and its compiled state can be
  * ahead of what git (and therefore the fresh worktree) has: an uncommitted edit that was compiled. Copy-state clones that state as-is, dirty output included.
  * The whole design rests on zinc then reconciling the target's ACTUAL sources against the inherited analysis on the target's first compile: recompile what
  * differs, noop what doesn't, and never trust inherited classfiles it cannot vouch for.
  *
  * Concretely: workspace A compiles `core` and `app`, then `app` gets an uncommitted marker method ([[Marker]]) and is recompiled — A's classfiles and analysis
  * now contain the marker. Workspace B is created with the ORIGINAL (pre-edit) sources, copy-state clones A's state into B (the marker is now sitting in B's
  * classfiles), and B compiles. Assertions:
  *
  *   1. `app` — whose source differs from the inherited analysis — actually recompiles, while `core` — whose source matches — noops (classfile untouched).
  *   1. after B's compile the marker is GONE from B's classfiles. This is the load-bearing assertion: the inherited `App.class` byte-for-byte contains a method
  *      B's sources never declared, and code compiled against stale inherited output would link against phantom API. Zinc must reconcile to B's sources, never
  *      trust inherited output.
  *   1. B's compile succeeds.
  *
  * Driven the way production is driven: real `build/initialize` + `buildTarget/compile` through [[BspTestHarness]], and `bleep/copyState` as a raw JSON-RPC
  * admin request (it needs no BSP handshake — see CopyStateEndpointTest for the endpoint's own contract tests).
  */
class CopyStateDirtyParentIntegrationTest extends AnyFunSuite with Matchers with TimeLimits {

  private val Marker = "copyStateDirtyMarker12345"
  private val ScalaV = "3.3.3"

  private lazy val scalaLib: List[Path] = CompilerResolver.resolveScalaLibrary(ScalaV).toList

  private val coreSource =
    """package corelib
      |
      |class Core {
      |  def coreValue: Int = 1
      |}
      |""".stripMargin

  private val appSourceOriginal =
    """package applib
      |
      |class App(val c: corelib.Core) {
      |  def appValue: Int = c.coreValue
      |}
      |""".stripMargin

  private val appSourceDirty =
    s"""package applib
       |
       |class App(val c: corelib.Core) {
       |  def appValue: Int = c.coreValue
       |  def $Marker(): Int = 42
       |}
       |""".stripMargin

  /** A workspace with the original (pre-edit) sources on disk. The stub bleep.yaml marks the directory as a workspace root for copy-state's `BuildLoader.find`;
    * the harness itself is payload-driven and never reads it.
    */
  private def createWorkspace(prefix: String): Path = {
    val ws = Files.createTempDirectory(prefix).toRealPath()
    Files.writeString(ws.resolve("bleep.yaml"), "$schema: fake\n")
    Files.createDirectories(ws.resolve("core/src"))
    Files.createDirectories(ws.resolve("app/src"))
    Files.writeString(ws.resolve("core/src/Core.scala"), coreSource)
    Files.writeString(ws.resolve("app/src/App.scala"), appSourceOriginal)
    ws
  }

  private def configs(ws: Path): List[BspTestHarness.ProjectConfig] = List(
    BspTestHarness.ProjectConfig(
      name = "core",
      sources = Set(ws.resolve("core/src")),
      classpath = scalaLib,
      languageConfig = ScalaConfig(ScalaV, Nil),
      dependsOn = Set.empty,
      isTest = false
    ),
    BspTestHarness.ProjectConfig(
      name = "app",
      sources = Set(ws.resolve("app/src")),
      classpath = BspTestBuild.classesDirFor(ws, "core", isTest = false) :: scalaLib,
      languageConfig = ScalaConfig(ScalaV, Nil),
      dependsOn = Set("core"),
      isTest = false
    )
  )

  private def appClass(ws: Path): Path = BspTestBuild.classesDirFor(ws, "app", isTest = false).resolve("applib/App.class")
  private def coreClass(ws: Path): Path = BspTestBuild.classesDirFor(ws, "core", isTest = false).resolve("corelib/Core.class")

  /** Method names live in the classfile constant pool as UTF-8, so an ASCII marker is byte-searchable. ISO-8859-1 maps bytes 1:1 to chars. */
  private def classBytesContain(classFile: Path, needle: String): Boolean =
    new String(Files.readAllBytes(classFile), StandardCharsets.ISO_8859_1).contains(needle)

  private def compileAll(client: BspTestHarness.BspClient): Int = {
    val targets = client.buildTargets().targets.map(_.id)
    targets should have size 2
    client.compile(targets).statusCode.value
  }

  test("dirty parent: seeded state recompiles where sources differ, noops where they match, and zinc purges the inherited marker") {
    failAfter(Span(300, Seconds)) {
      val wsA = createWorkspace("copy-state-dirty-a-")
      val wsB = createWorkspace("copy-state-dirty-b-")
      try {
        // Phase A: compile clean, then compile an uncommitted edit — A's state is now "dirty": ahead of what B's sources will say.
        BspTestHarness.withProjects(wsA, configs(wsA)) { client =>
          client.initialize()
          compileAll(client) shouldBe 1

          Files.writeString(wsA.resolve("app/src/App.scala"), appSourceDirty)
          compileAll(client) shouldBe 1
        }
        withClue("precondition: A's recompile must have baked the marker method into App.class: ") {
          classBytesContain(appClass(wsA), Marker) shouldBe true
        }

        // Phase B: a fresh workspace with the ORIGINAL sources inherits A's dirty state, then compiles.
        BspTestHarness.withProjects(wsB, configs(wsB)) { client =>
          val copyParams = {
            def q(p: Path) = io.circe.Json.fromString(p.toString).noSpaces
            s"""{"from":${q(wsA)},"to":${q(wsB)},"variant":"normal"}"""
          }
          val resultBytes = client.rawRequest(BleepServerAdmin.CopyStateMethod, copyParams)
          val response = io.circe.parser
            .decode[CopyStateResponse](new String(resultBytes, StandardCharsets.UTF_8))
            .fold(err => fail(s"could not decode CopyStateResponse: $err"), identity)
          response.projects shouldBe List("app", "core")

          withClue("copy-state must clone A's classfiles verbatim, dirty marker included: ") {
            classBytesContain(appClass(wsB), Marker) shouldBe true
          }
          withClue("the noop manifest must not be inherited — it would validate against A's paths and fake a noop for the dirty project: ") {
            val toPaths = bleep.BuildPaths(wsB, bleep.BuildLoader.find(wsB), model.BuildVariant.Normal)
            Files.exists(toPaths.zincDir(model.CrossProjectName.fromString("app").get).resolve("noop-manifest.bin")) shouldBe false
          }

          // Captured AFTER the copy: any rewrite by B's compile moves the mtime past this.
          val coreMtimeAfterCopy = Files.getLastModifiedTime(coreClass(wsB))
          val coreBytesAfterCopy = Files.readAllBytes(coreClass(wsB))

          client.initialize()
          withClue("(3) B's compile over inherited state must succeed: ") {
            compileAll(client) shouldBe 1
          }

          withClue(
            "(2) LOAD-BEARING: zinc must reconcile to B's actual sources — B never declared the marker method, so a compile that leaves it in App.class is trusting inherited output: "
          ) {
            classBytesContain(appClass(wsB), Marker) shouldBe false
          }
          withClue("(1) core's source matches the inherited analysis, so it must noop — classfile untouched by B's compile: ") {
            Files.getLastModifiedTime(coreClass(wsB)) shouldBe coreMtimeAfterCopy
            Files.readAllBytes(coreClass(wsB)) shouldBe coreBytesAfterCopy
          }
        }
      } finally {
        deleteRecursively(wsA)
        deleteRecursively(wsB)
      }
    }
  }
}
