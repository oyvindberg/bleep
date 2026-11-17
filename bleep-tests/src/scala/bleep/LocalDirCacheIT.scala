package bleep

import bleep.analysis.NoopManifestStore
import bleep.commands.RemoteCache
import bleep.internal.FileUtils

import java.nio.file.{Files, Path}
import scala.jdk.StreamConverters.*

/** The `file://` cache backend: same push/pull semantics as the S3 backend, but keys are files under a local directory and no credentials are required.
  *
  * Deliberately does NOT override `testConfig`: the harness default has no `remoteCacheCredentials`, so these tests prove the local backend never asks for them
  * (the S3 path fails hard without credentials).
  */
class LocalDirCacheIT extends IntegrationTestHarness {

  private def listRelativeFiles(dir: Path): List[String] =
    if (!Files.isDirectory(dir)) Nil
    else
      scala.util
        .Using(Files.walk(dir)) { stream =>
          stream
            .toScala(List)
            .filter(Files.isRegularFile(_))
            .map(p => dir.relativize(p).toString.replace('\\', '/'))
            .sorted
        }
        .getOrElse(Nil)

  integrationTest("local dir cache: push writes archive, pull restores classes + regenerates manifest, no credentials involved") { ws =>
    val cacheDir = Files.createTempDirectory("bleep-local-cache-")
    try {
      ws.yaml(
        s"""remote-cache:
           |  uri: ${cacheDir.toUri}
           |
           |projects:
           |  greeter:
           |    platform:
           |      name: jvm
           |    scala:
           |      version: 3.3.3
           |""".stripMargin
      )
      ws.file(
        "greeter/src/scala/com/test/Greeter.scala",
        """package com.test
          |object Greeter { def hello: String = "hi" }
          |""".stripMargin
      )

      val (started, _, _) = ws.start()
      val greeter = model.CrossProjectName(model.ProjectName("greeter"), None)
      ws.compileAll()

      val projectPaths = started.projectPaths(greeter)
      val analysisFile = projectPaths.targetDir.resolve(".zinc/analysis.zip")
      val classFile = projectPaths.classes.resolve("com/test/Greeter.class")
      val noopManifest = NoopManifestStore.manifestPath(analysisFile)

      assert(Files.exists(classFile), s"compile should have produced $classFile")
      assert(Files.exists(analysisFile), s"compile should have produced $analysisFile")

      // === PUSH ===
      RemoteCache.Push(projects = Array.empty, force = false, allowUnsafeSeeded = false).run(started).fold(e => fail(s"push failed: ${e.getMessage}"), identity)

      // Cache dir should contain exactly one object: <project>/<digest>.tar.gz (no prefix for file backend).
      val cached = listRelativeFiles(cacheDir)
      assert(cached.size == 1, s"expected 1 cached object, got $cached")
      val cacheKey = cached.head
      assert(cacheKey.startsWith("greeter/"), s"unexpected cache key: $cacheKey")
      assert(cacheKey.endsWith(".tar.gz"), s"unexpected cache key extension: $cacheKey")
      assert(!cached.exists(_.endsWith(".tmp")), s"temp files must not survive an atomic put: $cached")

      // Archive must ship classes + analysis but never the per-machine noop manifest.
      val unpackDir = Files.createTempDirectory("bleep-local-cache-inspect-")
      try {
        TarGz.unpack(Files.readAllBytes(cacheDir.resolve(cacheKey)), unpackDir)
        val archived = listRelativeFiles(unpackDir)
        assert(archived.exists(_.endsWith("Greeter.class")), s"archive missing classes: $archived")
        assert(archived.exists(_ == ".zinc/analysis.zip"), s"archive missing analysis: $archived")
        assert(!archived.exists(_.endsWith("noop-manifest.bin")), s"noop-manifest.bin must not be shipped, got $archived")
      } finally FileUtils.deleteDirectory(unpackDir)

      // Second push without --force skips: headObject sees the existing file.
      RemoteCache
        .Push(projects = Array.empty, force = false, allowUnsafeSeeded = false)
        .run(started)
        .fold(e => fail(s"second push failed: ${e.getMessage}"), identity)
      assert(listRelativeFiles(cacheDir) == cached, "second push should not have written new keys")

      // === WIPE LOCAL, PULL ===
      FileUtils.deleteDirectory(projectPaths.targetDir)
      assert(!Files.exists(classFile), "wipe should have removed local classes")

      RemoteCache.Pull(projects = Array.empty, unsafeSeedRef = None).run(started).fold(e => fail(s"pull failed: ${e.getMessage}"), identity)

      assert(Files.exists(classFile), s"pull should have restored $classFile")
      assert(Files.exists(analysisFile), s"pull should have restored $analysisFile")
      if (NoopManifestStore.ctimeAvailable) {
        assert(Files.exists(noopManifest), s"pull should have regenerated $noopManifest")
        val loaded = NoopManifestStore.load(analysisFile).getOrElse(fail("regenerated manifest must load"))
        assert(loaded.cachedResult.outputDir == projectPaths.classes, s"manifest outputDir wrong: ${loaded.cachedResult.outputDir}")
      }
      succeed
    } finally FileUtils.deleteDirectory(cacheDir)
  }

  /** `-c user.*`/`commit.gpgsign=false` so this doesn't depend on (or pollute) the machine's own git config — same trick as StampsIT/CiCommandIT. */
  private def git(root: Path, args: String*): String =
    scala.sys.process.Process("git" :: args.toList, root.toFile).!!.trim

  private def commitAll(root: Path, message: String): String = {
    git(root, "add", "-A")
    git(root, "-c", "user.email=ci@bleep.build", "-c", "user.name=bleep", "-c", "commit.gpgsign=false", "commit", "-m", message)
    git(root, "rev-parse", "HEAD")
  }

  integrationTest("local dir cache: --unsafe-seed-ref seeds an exact-digest miss, and push refuses the result without --allow-unsafe-seeded") { ws =>
    val cacheDir = Files.createTempDirectory("bleep-local-cache-seed-")
    try {
      ws.yaml(
        s"""remote-cache:
           |  uri: ${cacheDir.toUri}
           |
           |projects:
           |  lib:
           |    platform:
           |      name: jvm
           |    scala:
           |      version: 3.3.3
           |  app:
           |    dependsOn: lib
           |    platform:
           |      name: jvm
           |    scala:
           |      version: 3.3.3
           |""".stripMargin
      )
      ws.file("lib/src/scala/Lib.scala", "object Lib { val v = 1 }")
      ws.file("app/src/scala/App.scala", "object App { def main(args: Array[String]): Unit = println(Lib.v) }")

      git(ws.root, "init")
      val commit1 = commitAll(ws.root, "base")

      val (started, _, _) = ws.start()
      val lib = model.CrossProjectName(model.ProjectName("lib"), None)
      val app = model.CrossProjectName(model.ProjectName("app"), None)
      ws.compileAll()

      // Push both under commit1's digest.
      RemoteCache.Push(projects = Array.empty, force = false, allowUnsafeSeeded = false).run(started).fold(e => fail(s"push failed: ${e.getMessage}"), identity)

      // Move `lib` forward. `app` never touches its own source — but depends on `lib`, so `app`'s digest moves too.
      ws.file("lib/src/scala/Lib.scala", "object Lib { val v = 2 }")
      commitAll(ws.root, "lib v2")

      val libPaths = started.projectPaths(lib)
      val appPaths = started.projectPaths(app)
      FileUtils.deleteDirectory(libPaths.targetDir)
      FileUtils.deleteDirectory(appPaths.targetDir)

      // Exact digest misses for both (neither was ever pushed under its post-v2 digest). --unsafe-seed-ref should
      // find commit1's entry for each and seed from it instead.
      RemoteCache
        .Pull(projects = Array.empty, unsafeSeedRef = Some(commit1))
        .run(started)
        .fold(e => fail(s"seeded pull failed: ${e.getMessage}"), identity)

      assert(Files.isDirectory(libPaths.classes) && Files.list(libPaths.classes).findAny().isPresent, "lib should have been seeded")
      assert(Files.isDirectory(appPaths.classes) && Files.list(appPaths.classes).findAny().isPresent, "app should have been seeded")
      assert(Files.exists(libPaths.targetDir.resolve(".zinc/unsafe-seed")), "lib should carry the unsafe-seed marker")
      assert(Files.exists(appPaths.targetDir.resolve(".zinc/unsafe-seed")), "app should carry the unsafe-seed marker")

      // Zinc must be able to reconcile the seed against the real, current sources.
      ws.compileAll()
      assert(Files.exists(libPaths.targetDir.resolve(".zinc/unsafe-seed")), "the marker must survive a normal compile on top of the seed")

      // Push without --allow-unsafe-seeded refuses both seeded projects.
      val refused = RemoteCache.Push(projects = Array.empty, force = false, allowUnsafeSeeded = false).run(started)
      assert(refused.isLeft, "push must refuse projects whose compiled state came from an unsafe seed")
      refused.left.foreach(e => assert(e.getMessage.contains("unsafe seed"), s"refusal should name the reason: ${e.getMessage}"))

      // Push with --allow-unsafe-seeded succeeds.
      RemoteCache
        .Push(projects = Array.empty, force = false, allowUnsafeSeeded = true)
        .run(started)
        .fold(e => fail(s"push with --allow-unsafe-seeded should have succeeded: ${e.getMessage}"), identity)

      succeed
    } finally FileUtils.deleteDirectory(cacheDir)
  }
}
