package bleep

import bleep.analysis.{NoopManifestStore, OutputDeterminants, ProjectLanguage}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.Path

/** Regression test for a real bug found while benchmarking `copy-state` on a production monorepo: every Scala 3 project gets `-sourceroot <absolute path to its
  * own build dir>` injected into `scalaOptions` (`ResolveProjects.sourceRoot`, for TASTy path portability — see its own doc comment). Zinc's own incremental
  * check is already made tolerant of this via `PortableAnalysisMappers`'s marker-based rebasing, but `computeOptionsHash` — bleep's OWN, separate, faster
  * pre-Zinc noop check — was hashing `scalaOptions` verbatim, absolutized `-sourceroot` included. Two sibling workspaces at different absolute paths therefore
  * NEVER matched on options hash for ANY Scala 3 project, regardless of how identical their sources were — "compiler options changed" was the single most
  * common reason `copy-state`'s regenerated manifest was rejected on the very first real compile.
  *
  * Confirmed on the production repo: `dparsegen/main`'s options hash differed only because its injected `-sourceroot` read the source workspace's own absolute
  * path on one side and the fork's on the other.
  */
class NoopManifestOptionsHashTest extends AnyFunSuite with Matchers {

  private def scalaJava(scalaOptions: List[String]): ProjectLanguage.ScalaJava =
    ProjectLanguage.ScalaJava(
      scalaVersion = "3.4.0",
      scalaOptions = scalaOptions,
      javaOptions = Nil,
      ecjVersion = None,
      compileOrder = model.CompileOrder.Mixed,
      compilerProject = None
    )

  private val determinants = OutputDeterminants.none

  test("two sibling workspaces at different absolute paths hash equal despite each one's own injected -sourceroot") {
    val buildDirA = Path.of("/Users/oyvind/pr/dlab")
    val buildDirB = Path.of("/Users/oyvind/pr/dlab/worktrees/bleepbench-seeded")

    val optionsA = List("-encoding", "utf8", "-feature", "-sourceroot", buildDirA.toString, "-unchecked")
    val optionsB = List("-encoding", "utf8", "-feature", "-sourceroot", buildDirB.toString, "-unchecked")

    val hashA = NoopManifestStore.computeOptionsHash(scalaJava(optionsA), None, determinants, buildDirA)
    val hashB = NoopManifestStore.computeOptionsHash(scalaJava(optionsB), None, determinants, buildDirB)

    withClue("LOAD-BEARING: identical options modulo each workspace's own build-dir-relative -sourceroot must hash equal: ") {
      hashA shouldBe hashB
    }
  }

  test("the colon-joined form (-sourceroot:<path>, the semanticdb plugin's own syntax) is rebased the same way") {
    val buildDirA = Path.of("/ws/a")
    val buildDirB = Path.of("/ws/b/fork")

    val hashA = NoopManifestStore.computeOptionsHash(scalaJava(List(s"-sourceroot:$buildDirA")), None, determinants, buildDirA)
    val hashB = NoopManifestStore.computeOptionsHash(scalaJava(List(s"-sourceroot:$buildDirB")), None, determinants, buildDirB)

    hashA shouldBe hashB
  }

  test("a path under the build dir, not equal to it, is rebased relative to the build dir — not just the exact root") {
    val buildDirA = Path.of("/ws/a")
    val buildDirB = Path.of("/ws/b/fork")

    // e.g. a subdirectory-rooted option, the same shape Scala Native's position-relativization paths use.
    val hashA =
      NoopManifestStore.computeOptionsHash(scalaJava(List("-sourceroot", buildDirA.resolve("src/main/scala").toString)), None, determinants, buildDirA)
    val hashB =
      NoopManifestStore.computeOptionsHash(scalaJava(List("-sourceroot", buildDirB.resolve("src/main/scala").toString)), None, determinants, buildDirB)

    hashA shouldBe hashB
  }

  test("a path OUTSIDE the build dir (e.g. a coursier-cached plugin jar, the same on both workspaces on one machine) is left untouched") {
    val buildDirA = Path.of("/ws/a")
    val buildDirB = Path.of("/ws/b/fork")
    val sameMachinePluginJar = "/Users/oyvind/.cache/coursier/v1/some-plugin.jar"

    val hashA = NoopManifestStore.computeOptionsHash(scalaJava(List(s"-Xplugin:$sameMachinePluginJar")), None, determinants, buildDirA)
    val hashB = NoopManifestStore.computeOptionsHash(scalaJava(List(s"-Xplugin:$sameMachinePluginJar")), None, determinants, buildDirB)

    hashA shouldBe hashB
  }

  test("a genuine difference in options must still be caught — this isn't a license to ignore real changes") {
    val buildDir = Path.of("/ws/a")

    val hashUnchecked = NoopManifestStore.computeOptionsHash(scalaJava(List("-unchecked")), None, determinants, buildDir)
    val hashDeprecation = NoopManifestStore.computeOptionsHash(scalaJava(List("-deprecation")), None, determinants, buildDir)

    hashUnchecked should not be hashDeprecation
  }
}
