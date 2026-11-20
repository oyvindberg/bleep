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
  private val ApiMarker = "coreApiMarker67890"
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

  /** Dirty variant of `core` with an API change `app` is sensitive to: an added overload of `coreValue`, the very name `app` calls. Zinc's name-hashing
    * invalidation only invalidates dependents that USE an affected name, so a marker method alone would recompile `core` without touching `app` — the overload
    * is what makes the edit propagate.
    */
  private val coreSourceDirty =
    s"""package corelib
       |
       |class Core {
       |  def coreValue: Int = 1
       |  def coreValue(bump: Int): Int = $ApiMarker + bump
       |  def $ApiMarker: Int = 2
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

  /** Same shape as [[configs]], but with `-sourceroot <ws>` on every project — the real `-sourceroot` injection `ResolveProjects.sourceRoot` does for every
    * Scala 3 project, unconditionally, pointing at that build's own workspace root. Kept as a separate helper rather than changing [[configs]] itself, so the
    * two tests above (which exercise a genuinely DIRTY parent) stay exactly as they were.
    */
  private def configsWithSourceRoot(ws: Path): List[BspTestHarness.ProjectConfig] = List(
    BspTestHarness.ProjectConfig(
      name = "core",
      sources = Set(ws.resolve("core/src")),
      classpath = scalaLib,
      languageConfig = ScalaConfig(ScalaV, List("-sourceroot", ws.toString)),
      dependsOn = Set.empty,
      isTest = false
    ),
    BspTestHarness.ProjectConfig(
      name = "app",
      sources = Set(ws.resolve("app/src")),
      classpath = BspTestBuild.classesDirFor(ws, "core", isTest = false) :: scalaLib,
      languageConfig = ScalaConfig(ScalaV, List("-sourceroot", ws.toString)),
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

  private def copyState(client: BspTestHarness.BspClient, from: Path, to: Path): CopyStateResponse = {
    def q(p: Path) = io.circe.Json.fromString(p.toString).noSpaces
    val resultBytes = client.rawRequest(BleepServerAdmin.CopyStateMethod, s"""{"from":${q(from)},"to":${q(to)},"variant":"normal"}""")
    io.circe.parser
      .decode[CopyStateResponse](new String(resultBytes, StandardCharsets.UTF_8))
      .fold(err => fail(s"could not decode CopyStateResponse: $err"), identity)
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
          copyState(client, wsA, wsB).projects shouldBe List("app", "core")

          withClue("copy-state must clone A's classfiles verbatim, dirty marker included: ") {
            classBytesContain(appClass(wsB), Marker) shouldBe true
          }
          val toPaths = bleep.BuildPaths(wsB, bleep.BuildLoader.find(wsB), model.BuildVariant.Normal)
          withClue(
            "A's own noop-manifest.bin must never be inherited verbatim — it would validate against A's paths and fake a noop. Regenerating one for `app` " +
              "specifically must also fail: its rebased source content (B's original App.scala) does not match what A's manifest recorded (B's App.scala " +
              "plus the marker edit) — exactly the divergence the regeneration's content re-verification exists to catch: "
          ) {
            Files.exists(toPaths.zincDir(model.CrossProjectName.fromString("app").get).resolve("noop-manifest.bin")) shouldBe false
          }
          withClue(
            "but `core`'s source is identical between A and B, so a freshly regenerated manifest for it must exist — proving this isn't just \"never write one\": "
          ) {
            Files.exists(toPaths.zincDir(model.CrossProjectName.fromString("core").get).resolve("noop-manifest.bin")) shouldBe true
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

  /** The inverse direction of the test above: the uncommitted edit is in the UPSTREAM project. The first test pins "dirty project recompiles, clean dependency
    * noops"; this one pins that the invalidation PROPAGATES — B's `core` recompile changes `Core`'s API back (the overload disappears), and `app`, whose own
    * source is identical to what the inherited analysis recorded, must be recompiled anyway because the API it was compiled against no longer exists. If the
    * copied analyses did not carry cross-project (external) API hashes correctly, `app` would silently keep linking against the phantom overload.
    *
    * `app`'s recompile is asserted via mtime, not bytes: its source never changed, so a correct recompile produces byte-identical output — the rewrite itself
    * is the observable.
    */
  test("dirty upstream: an API edit in core propagates through the copied dependency analyses and recompiles the dependent in B") {
    failAfter(Span(300, Seconds)) {
      val wsA = createWorkspace("copy-state-dirty-up-a-")
      val wsB = createWorkspace("copy-state-dirty-up-b-")
      try {
        // Phase A: compile clean, then compile an uncommitted API edit in core — app recompiles against the dirty API there too.
        BspTestHarness.withProjects(wsA, configs(wsA)) { client =>
          client.initialize()
          compileAll(client) shouldBe 1

          Files.writeString(wsA.resolve("core/src/Core.scala"), coreSourceDirty)
          compileAll(client) shouldBe 1
        }
        withClue("precondition: A's recompile must have baked the API marker into Core.class: ") {
          classBytesContain(coreClass(wsA), ApiMarker) shouldBe true
        }

        // Phase B: original sources inherit A's dirty state.
        BspTestHarness.withProjects(wsB, configs(wsB)) { client =>
          copyState(client, wsA, wsB).projects shouldBe List("app", "core")

          withClue("copy-state must clone A's dirty core classfiles verbatim: ") {
            classBytesContain(coreClass(wsB), ApiMarker) shouldBe true
          }

          val appMtimeAfterCopy = Files.getLastModifiedTime(appClass(wsB))

          client.initialize()
          withClue("B's compile over inherited state must succeed: ") {
            compileAll(client) shouldBe 1
          }

          withClue("zinc must reconcile core to B's actual sources — the API marker (and the overload) must be purged from Core.class: ") {
            classBytesContain(coreClass(wsB), ApiMarker) shouldBe false
          }
          withClue(
            "app must be recompiled in B even though its own source matches the inherited analysis — the core API it was compiled against changed back: "
          ) {
            Files.getLastModifiedTime(appClass(wsB)) should not be appMtimeAfterCopy
          }
        }
      } finally {
        deleteRecursively(wsA)
        deleteRecursively(wsB)
      }
    }
  }

  /** Regression test for a real bug: `-sourceroot` — the absolute path `ResolveProjects.sourceRoot` injects into every Scala 3 project's scalac options, for
    * Scala 3's TASTy portability — was the one path-bearing field `PortableAnalysisMappers` left as plain identity, never rebased between workspaces. Zinc's
    * `MiniSetup` equivalence check compares scalac options verbatim, so inheriting ANY project's analysis into a workspace at a different absolute path than it
    * was compiled under failed that check unconditionally, discarding the whole previous analysis and forcing a full recompile — regardless of whether anything
    * the project actually depends on changed.
    *
    * This hit copy-state harder than remote-cache: copy-state never ships `noop-manifest.bin` (see the clue above, "the noop manifest must not be inherited"),
    * so there is no fast path to mask it — every single copy-state use, for every Scala 3 project, was affected. The two tests above never caught this because
    * neither passes any scalac options at all (`ScalaConfig(ScalaV, Nil)`), so there was nothing for the broken mapper to get wrong.
    *
    * Unlike the dirty-parent tests, nothing here is actually dirty: A and B have byte-identical sources. The only thing that differs between the two workspaces
    * is their own absolute path — exactly the scenario `copy-state` exists for (a freshly created worktree, same commit as its parent) — so if the fix is doing
    * its job, both projects should noop in B, not just compile successfully.
    */
  test("sourceroot is rebased per workspace: inheriting state at a different absolute path must still noop when nothing actually changed") {
    failAfter(Span(300, Seconds)) {
      val wsA = createWorkspace("copy-state-sourceroot-a-")
      val wsB = createWorkspace("copy-state-sourceroot-b-")
      try {
        // Phase A: compile clean, with -sourceroot pointing at wsA's own path — exactly what a real Scala 3 project's build would inject.
        BspTestHarness.withProjects(wsA, configsWithSourceRoot(wsA)) { client =>
          client.initialize()
          compileAll(client) shouldBe 1
        }

        // Phase B: a fresh workspace with IDENTICAL sources inherits A's state via copy-state, then compiles. Its own -sourceroot points at wsB, not wsA.
        BspTestHarness.withProjects(wsB, configsWithSourceRoot(wsB)) { client =>
          copyState(client, wsA, wsB).projects shouldBe List("app", "core")

          val toPaths = bleep.BuildPaths(wsB, bleep.BuildLoader.find(wsB), model.BuildVariant.Normal)
          withClue(
            "both projects' sources are byte-identical between A and B, so copy-state's manifest regeneration must succeed for both — rebased to B's own " +
              "absolute paths, not inherited verbatim from A's. This is what lets the compile below skip zinc's own invalidation entirely, rather than " +
              "merely surviving it: "
          ) {
            Files.exists(toPaths.zincDir(model.CrossProjectName.fromString("core").get).resolve("noop-manifest.bin")) shouldBe true
            Files.exists(toPaths.zincDir(model.CrossProjectName.fromString("app").get).resolve("noop-manifest.bin")) shouldBe true
          }

          val coreMtimeAfterCopy = Files.getLastModifiedTime(coreClass(wsB))
          val coreBytesAfterCopy = Files.readAllBytes(coreClass(wsB))
          val appMtimeAfterCopy = Files.getLastModifiedTime(appClass(wsB))
          val appBytesAfterCopy = Files.readAllBytes(appClass(wsB))

          client.initialize()
          withClue("B's compile over inherited state must succeed: ") {
            compileAll(client) shouldBe 1
          }

          withClue(
            "LOAD-BEARING: neither source changed between A and B, so both must noop. If -sourceroot's embedded absolute path (A's) is not rebased to B's " +
              "own workspace on load, zinc's MiniSetup equivalence check fails and the whole inherited analysis is discarded, forcing a full recompile of " +
              "everything despite nothing having actually changed: "
          ) {
            Files.getLastModifiedTime(coreClass(wsB)) shouldBe coreMtimeAfterCopy
            Files.readAllBytes(coreClass(wsB)) shouldBe coreBytesAfterCopy
            Files.getLastModifiedTime(appClass(wsB)) shouldBe appMtimeAfterCopy
            Files.readAllBytes(appClass(wsB)) shouldBe appBytesAfterCopy
          }
        }
      } finally {
        deleteRecursively(wsA)
        deleteRecursively(wsB)
      }
    }
  }
}
