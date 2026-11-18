package bleep

import bleep.commands.RemoteCache
import bleep.internal.FileUtils

import java.nio.file.{Files, Path}

/** The remote cache's generator-output cache: `bleep remote-cache pull`/`push` populating/publishing a sourcegen declaration's `generated-sources` output
  * independently of (and before) any consuming project's own classes digest — see [[ProjectDigest.computeSourcegenDigests]] and the class doc comment on
  * [[RemoteCache]].
  *
  * The generator here deliberately embeds a random marker on every real invocation, so a test can tell "the generator actually ran again" apart from "its
  * cached output was restored byte-for-byte" without depending on log wording.
  */
class SourcegenRemoteCacheIT extends IntegrationTestHarness {
  private val Yaml = """projects:
                 |  a:
                 |    extends: common
                 |    sourcegen: scripts/testscripts.SourceGen
                 |  scripts:
                 |    extends: common
                 |    dependencies: build.bleep::bleep-core:${BLEEP_VERSION}
                 |templates:
                 |  common:
                 |    platform:
                 |      name: jvm
                 |      jvmRuntimeOptions: -Xmx256m -Xms32m
                 |    scala:
                 |      version: 3.9.0
                 |""".stripMargin

  private val SourceGen = """package testscripts
                      |
                      |import bleep.*
                      |import java.nio.file.Files
                      |
                      |object SourceGen extends BleepCodegenScript("SourceGen") {
                      |  def run(started: Started, commands: Commands, targets: List[Target], args: List[String]): Unit =
                      |    targets.foreach { target =>
                      |      val targetFile = target.sources / "testgenerated" / "GeneratedSource.scala"
                      |      Files.createDirectories(targetFile.getParent)
                      |      Files.writeString(
                      |        targetFile,
                      |        "package testgenerated\n" +
                      |        "object GeneratedSource { val marker = \"" + java.util.UUID.randomUUID().toString + "\" }\n"
                      |      )
                      |    }
                      |}
                      |""".stripMargin

  private def writeFiles(ws: Workspace): Unit = {
    ws.yaml(Yaml)
    ws.file("a/src/scala/test/Main.scala", "package test\nobject Main { def main(args: Array[String]): Unit = () }\n")
    ws.file("scripts/src/scala/testscripts/SourceGen.scala", SourceGen)
  }

  /** `-c user.*`/`commit.gpgsign=false` so this doesn't depend on (or pollute) the machine's own git config — same trick as LocalDirCacheIT. */
  private def git(root: Path, args: String*): String =
    scala.sys.process.Process("git" :: args.toList, root.toFile).!!.trim

  private def commitAll(root: Path, message: String): String = {
    git(root, "add", "-A")
    git(root, "-c", "user.email=ci@bleep.build", "-c", "user.name=bleep", "-c", "commit.gpgsign=false", "commit", "-m", message)
    git(root, "rev-parse", "HEAD")
  }

  private val a = model.CrossProjectName(model.ProjectName("a"), None)
  private val generatorMain = "testscripts.SourceGen"

  private def generatedFile(buildPaths: BuildPaths): Path =
    buildPaths.generatedSourcesDir(a, generatorMain).resolve("testgenerated/GeneratedSource.scala")

  integrationTest("remote cache: sourcegen output is pulled from the cache instead of running the generator again") { wsA =>
    val cacheDir = Files.createTempDirectory("bleep-sourcegen-cache-")
    val wsB = new Workspace("sourcegen-remote-cache-B", prelude, snippetsRoot, userPaths, logger0, ec, lazyBleepBuild, testConfig)
    try {
      val yamlWithCache =
        s"remote-cache:\n  uri: ${cacheDir.toUri}\n\n" + Yaml

      // Workspace A: compile for real (the generator actually runs, embedding a random marker) and push.
      writeFiles(wsA)
      wsA.yaml(yamlWithCache)
      val (startedA, commandsA, _) = wsA.start()
      commandsA.compile(List(a))

      val markerA = Files.readString(generatedFile(startedA.buildPaths))
      assert(markerA.contains("marker ="), s"generator should have produced a marker: $markerA")

      RemoteCache
        .Push(projects = Array(a), force = false, allowUnsafeSeeded = false)
        .run(startedA)
        .fold(e => fail(s"push failed: ${e.getMessage}"), identity)

      // Workspace B: identical committed content, a different absolute path, never built anything.
      writeFiles(wsB)
      wsB.yaml(yamlWithCache)
      git(wsB.root, "init")
      commitAll(wsB.root, "baseline")

      val (startedB, commandsB, _) = wsB.start()
      val generatedFileB = generatedFile(startedB.buildPaths)
      assert(!Files.exists(generatedFileB), "workspace B must start genuinely cold: sourcegen has never run there")

      RemoteCache.Pull(projects = Array(a), unsafeSeedRef = None).run(startedB).fold(e => fail(s"pull failed: ${e.getMessage}"), identity)

      assert(Files.exists(generatedFileB), "pull should have restored the generator's output")
      val markerAfterPull = Files.readString(generatedFileB)
      assert(
        markerAfterPull == markerA,
        s"pulled output must be byte-identical to what A pushed — a different marker means the generator ran again instead of being served from cache"
      )

      // The subsequent compile must see fresh-enough output and not rerun the generator either.
      commandsB.compile(List(a))
      val markerAfterCompile = Files.readString(generatedFileB)
      assert(markerAfterCompile == markerA, "compile must not have rerun the generator after a cache-restored output")

      succeed
    } finally {
      wsB.cleanup()
      FileUtils.deleteDirectory(cacheDir)
    }
  }

  integrationTest("remote cache: a project with a sourcegen dependency can be cache-served without ever running the generator locally first") { ws =>
    // The friction this feature removes: historically, `remote-cache pull`/`push` needed `bleep sourcegen` to have already run locally, because a project's
    // digest was computed from its generated output's actual bytes. Now the digest is computed from the generator's declared inputs instead (see
    // ProjectDigest.computeGeneratorDigest), so a fresh checkout that pulls BOTH the sourcegen output and the project's own classes in one `pull` — with the
    // generator never run at all — still gets a correct, complete build.
    val cacheDir = Files.createTempDirectory("bleep-sourcegen-cache-full-")
    try {
      writeFiles(ws)
      ws.yaml(s"remote-cache:\n  uri: ${cacheDir.toUri}\n\n" + Yaml)
      val (started, commands, _) = ws.start()
      commands.compile(List(a))

      val projectPaths = started.projectPaths(a)
      RemoteCache
        .Push(projects = Array(a), force = false, allowUnsafeSeeded = false)
        .run(started)
        .fold(e => fail(s"push failed: ${e.getMessage}"), identity)

      // Wipe both the generated output and the compiled classes, simulating a fresh checkout.
      FileUtils.deleteDirectory(started.buildPaths.generatedSourcesDir(a, generatorMain))
      FileUtils.deleteDirectory(projectPaths.targetDir)
      assert(!Files.exists(generatedFile(started.buildPaths)), "wipe should have removed the generated output")
      assert(!Files.isDirectory(projectPaths.classes), "wipe should have removed the compiled classes")

      RemoteCache.Pull(projects = Array(a), unsafeSeedRef = None).run(started).fold(e => fail(s"pull failed: ${e.getMessage}"), identity)

      assert(Files.exists(generatedFile(started.buildPaths)), "pull should have restored the generated output")
      assert(Files.isDirectory(projectPaths.classes) && Files.list(projectPaths.classes).findAny().isPresent, "pull should have restored the classes too")

      succeed
    } finally FileUtils.deleteDirectory(cacheDir)
  }
}
