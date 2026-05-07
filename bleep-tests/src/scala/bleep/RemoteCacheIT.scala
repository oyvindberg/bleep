package bleep

import bleep.analysis.NoopManifestStore
import bleep.commands.RemoteCache
import bleep.internal.FileUtils

import java.nio.file.{Files, Path}
import scala.jdk.StreamConverters.*

class RemoteCacheIT extends IntegrationTestHarness {

  /** S3 client picks credentials up from `Started.config.remoteCacheCredentials` (or env vars). The local `S3LikeServer` ignores AWS SigV4 entirely, so any
    * non-empty pair works; the value is exercised through the real codepath that builds the Authorization header so cred-plumbing regressions still surface.
    *
    * Reconstructed in full because the harness exposes `testConfig` as a `val` — `super.testConfig` is not a legal Scala expression.
    */
  override val testConfig: model.BleepConfig = model.BleepConfig(
    compileServerMode = Some(model.CompileServerMode.NewEachInvocation),
    authentications = None,
    logTiming = None,
    bspServerConfig = Some(model.BspServerConfig.default.copy(testRunnerMaxMemory = Some("512m"))),
    remoteCacheCredentials = Some(model.RemoteCacheCredentials(accessKeyId = "test-access-key", secretAccessKey = "test-secret-key"))
  )

  /** Read every file under `dir`, returning relative paths sorted. Mirrors HttpMavenRepoServer.storedFiles for archive contents. */
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

  integrationTest("remote cache: push uploads archive without noop-manifest, pull restores classes + regenerates manifest") { ws =>
    S3LikeServer.withServer("test-bucket") { server =>
      ws.yaml(
        s"""remote-cache:
           |  uri: ${server.baseUri}/cache-prefix
           |  region: us-east-1
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

      // Sanity: compile produced what we expect to ship + a per-machine manifest we expect to NOT ship.
      assert(Files.exists(classFile), s"compile should have produced $classFile")
      assert(Files.exists(analysisFile), s"compile should have produced $analysisFile")
      if (NoopManifestStore.ctimeAvailable)
        assert(Files.exists(noopManifest), s"compile should have produced $noopManifest on macOS/Linux")

      // === PUSH ===
      RemoteCache.Push(projects = Array.empty, force = false).run(started).fold(e => fail(s"push failed: ${e.getMessage}"), identity)

      // Server should have exactly one object: <prefix>/<project>/<digest>.tar.gz.
      assert(server.size == 1, s"expected 1 cached object, got ${server.keys}")
      val cacheKey = server.keys.head
      assert(cacheKey.startsWith("cache-prefix/greeter/"), s"unexpected cache key: $cacheKey")
      assert(cacheKey.endsWith(".tar.gz"), s"unexpected cache key extension: $cacheKey")

      // Inspect archive contents: classes + analysis must be present, noop-manifest.bin must NOT be.
      val uploaded = server.get(cacheKey).getOrElse(fail(s"object missing for $cacheKey"))
      val unpackDir = Files.createTempDirectory("bleep-cache-inspect-")
      try {
        TarGz.unpack(uploaded, unpackDir)
        val archived = listRelativeFiles(unpackDir)
        assert(archived.exists(_.endsWith("Greeter.class")), s"archive missing classes: $archived")
        assert(archived.exists(_ == ".zinc/analysis.zip"), s"archive missing analysis: $archived")
        assert(!archived.exists(_.endsWith("noop-manifest.bin")), s"noop-manifest.bin must not be shipped, got $archived")
      } finally FileUtils.deleteDirectory(unpackDir)

      // === WIPE LOCAL ===
      FileUtils.deleteDirectory(projectPaths.targetDir)
      assert(!Files.exists(classFile), "wipe should have removed local classes")
      assert(!Files.exists(analysisFile), "wipe should have removed local analysis")

      // === PULL ===
      RemoteCache.Pull(projects = Array.empty).run(started).fold(e => fail(s"pull failed: ${e.getMessage}"), identity)

      // Classes + analysis restored, noop manifest regenerated locally.
      assert(Files.exists(classFile), s"pull should have restored $classFile")
      assert(Files.exists(analysisFile), s"pull should have restored $analysisFile")
      if (NoopManifestStore.ctimeAvailable) {
        assert(Files.exists(noopManifest), s"pull should have regenerated $noopManifest")

        // The regenerated manifest must be loadable and reference local paths (not the source machine's).
        val loaded = NoopManifestStore.load(analysisFile).getOrElse(fail("regenerated manifest must load"))
        assert(loaded.cachedResult.outputDir == projectPaths.classes, s"manifest outputDir wrong: ${loaded.cachedResult.outputDir}")
        assert(loaded.cachedResult.classFiles.exists(_.endsWith("Greeter.class")), s"manifest classFiles missing Greeter: ${loaded.cachedResult.classFiles}")
        assert(
          loaded.sourceStats.keySet.exists(_.toString.endsWith("Greeter.scala")),
          s"manifest sourceStats missing Greeter.scala: ${loaded.sourceStats.keys}"
        )
      }
      succeed
    }
  }

  integrationTest("remote cache: pull skips when classes already present locally") { ws =>
    S3LikeServer.withServer("test-bucket") { server =>
      ws.yaml(
        s"""remote-cache:
             |  uri: ${server.baseUri}/cache-prefix
             |  region: us-east-1
             |
             |projects:
             |  hello:
             |    platform:
             |      name: jvm
             |    scala:
             |      version: 3.3.3
             |""".stripMargin
      )
      ws.file(
        "hello/src/scala/Hello.scala",
        """object Hello { def main(args: Array[String]): Unit = println("hi") }
            |""".stripMargin
      )

      val (started, _, _) = ws.start()
      ws.compileAll()

      // Nothing has been pushed; pull should be a no-op (classes already exist locally).
      RemoteCache.Pull(projects = Array.empty).run(started).fold(e => fail(s"pull failed: ${e.getMessage}"), identity)
      assert(server.size == 0, "no pull traffic should have hit the server when classes are present")
      succeed
    }
  }

  integrationTest("remote cache: push of an already-cached digest is skipped (without --force)") { ws =>
    S3LikeServer.withServer("test-bucket") { server =>
      ws.yaml(
        s"""remote-cache:
             |  uri: ${server.baseUri}/cache-prefix
             |  region: us-east-1
             |
             |projects:
             |  echo:
             |    platform:
             |      name: jvm
             |    scala:
             |      version: 3.3.3
             |""".stripMargin
      )
      ws.file("echo/src/scala/Echo.scala", "object Echo")

      val (started, _, _) = ws.start()
      ws.compileAll()

      // First push uploads.
      RemoteCache.Push(projects = Array.empty, force = false).run(started).fold(e => fail(s"first push failed: ${e.getMessage}"), identity)
      assert(server.size == 1, s"expected 1 object after first push, got ${server.keys}")
      val firstKeys = server.keys

      // Second push without --force: head returns 200, push skips.
      RemoteCache.Push(projects = Array.empty, force = false).run(started).fold(e => fail(s"second push failed: ${e.getMessage}"), identity)
      assert(server.keys == firstKeys, "second push should not have written a new key")
      succeed
    }
  }
}
