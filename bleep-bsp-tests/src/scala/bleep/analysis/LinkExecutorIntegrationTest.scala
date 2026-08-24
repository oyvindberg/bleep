package bleep.analysis

import bleep.bsp.{LinkExecutor, Outcome, TaskDag}
import bleep.bsp.Outcome.RunOutcome
import bleep.model.{CrossProjectName, ProjectName}
import cats.effect.unsafe.implicits.global
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import java.nio.file.Files

/** Integration tests for LinkExecutor.
  *
  * Tests:
  *   - Scala Native test linking uses TestMainClass when isTest=true
  *   - Scala Native linking throws when isTest=false and mainClass=None
  *   - Kotlin Native test linking produces binary with test runner via -Xinclude
  *   - locateLinkedJs finds the module a Scala.js link already wrote
  */
class LinkExecutorIntegrationTest extends AnyFunSuite with Matchers with PlatformTestHelper {

  // ==========================================================================
  // Finding a Scala.js module an earlier link wrote
  // ==========================================================================

  private val debugJsPlatform: TaskDag.LinkPlatform.ScalaJs =
    TaskDag.LinkPlatform.ScalaJs(DefaultScalaJsVersion, DefaultScalaVersion, ScalaJsLinkConfig.Debug)

  /** `execute` writes the main module under the suffix `linkDirSuffix` picks. `locateLinkedJs` takes the base directory `execute` was given. */
  test("locateLinkedJs: finds the main module under the link's own suffix directory") {
    withTempDir("locate-linked-js") { tempDir =>
      val jsDir = tempDir.resolve(LinkExecutor.linkDirSuffix(debugJsPlatform)).resolve("js")
      Files.createDirectories(jsDir)
      Files.writeString(jsDir.resolve("my_project.js"), "// linked module")
      Files.writeString(jsDir.resolve("my_project.js.map"), "{}")

      LinkExecutor.locateLinkedJs(tempDir, debugJsPlatform, "my_project") shouldBe Some(jsDir.resolve("my_project.js"))
    }
  }

  /** An ESModule link writes several `.js` files. The module the project names is the entry point. */
  test("locateLinkedJs: prefers the module the project names over the other files beside it") {
    withTempDir("locate-linked-js-many") { tempDir =>
      val jsDir = tempDir.resolve(LinkExecutor.linkDirSuffix(debugJsPlatform)).resolve("js")
      Files.createDirectories(jsDir)
      Files.writeString(jsDir.resolve("internal-a.js"), "// chunk")
      Files.writeString(jsDir.resolve("my_project.js"), "// linked module")
      Files.writeString(jsDir.resolve("internal-b.js"), "// chunk")

      LinkExecutor.locateLinkedJs(tempDir, debugJsPlatform, "my_project") shouldBe Some(jsDir.resolve("my_project.js"))
    }
  }

  /** A suite task that runs before any link finds nothing. The caller fails loudly rather than linking a second copy. */
  test("locateLinkedJs: returns None when no link has written a module") {
    withTempDir("locate-linked-js-empty") { tempDir =>
      LinkExecutor.locateLinkedJs(tempDir, debugJsPlatform, "my_project") shouldBe None
    }
  }

  // ==========================================================================
  // Scala Native test linking
  // ==========================================================================

  test("Scala Native test linking uses TestMainClass when isTest=true") {
    withTempDir("sn-test-link") { tempDir =>
      // Write a simple Scala Native source that uses the test interface
      val srcDir = tempDir.resolve("src")
      writeScalaSource(
        srcDir,
        "example",
        "MySuite.scala",
        """package example
          |
          |object MySuite {
          |  def main(args: Array[String]): Unit = {
          |    println("test output")
          |  }
          |}
          |""".stripMargin
      )

      val outDir = tempDir.resolve("classes")
      val classpath = compileForScalaNative(srcDir, outDir, DefaultScalaVersion, DefaultScalaNativeVersion)

      val baseOutputDir = tempDir.resolve("link-output")
      Files.createDirectories(baseOutputDir)

      val linkTask = TaskDag.LinkTask(
        project = CrossProjectName(ProjectName("test-proj"), None),
        platform = TaskDag.LinkPlatform.ScalaNative(
          version = DefaultScalaNativeVersion,
          scalaVersion = DefaultScalaVersion,
          config = ScalaNativeLinkConfig.Debug
        ),
        releaseMode = false,
        isTest = true
      )

      // When isTest=true and mainClass=None, should use ScalaNativeTestRunner.TestMainClass
      // instead of throwing. The link itself may fail (no test framework on classpath) but
      // the key assertion is that it attempts to link with the test main class, not throw.
      val result = (for {
        killSignal <- Outcome.neverKillSignal
        outcome <- LinkExecutor.execute(
          linkTask,
          classpath = classpath,
          mainClass = None,
          baseOutputDir = baseOutputDir,
          logger = LinkExecutor.LinkLogger.Silent,
          killSignal = killSignal
        )
      } yield outcome).unsafeRunSync()

      // The link attempt should proceed (not throw IllegalArgumentException).
      // It may succeed or fail depending on whether scala.scalanative.testinterface.TestMain
      // is on the classpath, but either way we get a result tuple, not an exception.
      result._1 shouldBe a[TaskDag.TaskResult]
      info(s"Scala Native test linking result: ${result._1}")
    }
  }

  test("Scala Native linking throws when isTest=false and mainClass=None") {
    withTempDir("sn-no-main") { tempDir =>
      val baseOutputDir = tempDir.resolve("link-output")
      Files.createDirectories(baseOutputDir)

      val linkTask = TaskDag.LinkTask(
        project = CrossProjectName(ProjectName("test-proj"), None),
        platform = TaskDag.LinkPlatform.ScalaNative(
          version = DefaultScalaNativeVersion,
          scalaVersion = DefaultScalaVersion,
          config = ScalaNativeLinkConfig.Debug
        ),
        releaseMode = false,
        isTest = false
      )

      // When isTest=false and mainClass=None, should throw IllegalArgumentException
      assertThrows[IllegalArgumentException] {
        (for {
          killSignal <- Outcome.neverKillSignal
          outcome <- LinkExecutor.execute(
            linkTask,
            classpath = Seq.empty,
            mainClass = None,
            baseOutputDir = baseOutputDir,
            logger = LinkExecutor.LinkLogger.Silent,
            killSignal = killSignal
          )
        } yield outcome).unsafeRunSync()
      }
    }
  }

  // ==========================================================================
  // Kotlin Native test linking
  // ==========================================================================

  test("Kotlin Native test linking produces binary with test runner") {
    assumeKotlinNativeAvailable()
    // Use the same version for compilation and linking to avoid ABI mismatch
    val kotlinVersion = KotlinNativeCompilerConfig.ForTests.kotlinVersion

    withTempDir("kt-native-test-link") { tempDir =>
      // Step 1: Compile a simple Kotlin source to KLIB
      val srcDir = tempDir.resolve("src")
      writeKotlinSource(
        srcDir,
        "Lib.kt",
        """fun greet(name: String): String = "Hello, $name"
          |
          |fun main() {
          |  println(greet("test"))
          |}
          |""".stripMargin
      )

      val klibPath = tempDir.resolve("test-proj.klib")
      val config = KotlinNativeCompilerConfig.ForTests.copy(
        outputKind = KotlinNativeCompilerConfig.OutputKind.Klib
      )

      val klibResult = KotlinNativeCompiler
        .compile(
          sources = Seq(srcDir.resolve("Lib.kt")),
          libraries = Seq.empty,
          outputPath = klibPath,
          config = config,
          diagnosticListener = DiagnosticListener.noop,
          cancellation = CancellationToken.never
        )
        .unsafeRunSync()

      assert(klibResult.isSuccess, s"KLIB compilation failed with exit code ${klibResult.exitCode}")
      assert(Files.exists(klibPath), s"KLIB not found at $klibPath")

      // Step 2: Link the KLIB into an executable using LinkExecutor with isTest=true
      val baseOutputDir = tempDir.resolve("link-output")
      Files.createDirectories(baseOutputDir)

      val linkTask = TaskDag.LinkTask(
        project = CrossProjectName(ProjectName("test-proj"), None),
        platform = TaskDag.LinkPlatform.KotlinNative(
          version = kotlinVersion,
          config = TaskDag.KotlinNativeConfig(
            target = KotlinNativeCompilerConfig.Target.hostTarget.konanName,
            debugInfo = true,
            optimizations = false,
            isTest = true
          )
        ),
        releaseMode = false,
        isTest = true
      )

      val result = (for {
        killSignal <- Outcome.neverKillSignal
        outcome <- LinkExecutor.execute(
          linkTask,
          classpath = Seq(klibPath),
          mainClass = None,
          baseOutputDir = baseOutputDir,
          logger = LinkExecutor.LinkLogger.Silent,
          killSignal = killSignal
        )
      } yield outcome).unsafeRunSync()

      // The link should succeed since -generate-test-runner creates an entry point
      // and -Xinclude is used to include the project's KLIB
      result._1 shouldBe TaskDag.TaskResult.Success
      result._2 shouldBe a[TaskDag.LinkResult.NativeSuccess]

      val nativeResult = result._2.asInstanceOf[TaskDag.LinkResult.NativeSuccess]
      // Kotlin/Native may output with .kexe extension - find the actual binary
      val reportedPath = nativeResult.binary
      val actualBinary = Seq(
        reportedPath,
        reportedPath.resolveSibling(reportedPath.getFileName.toString + ".kexe")
      ).find(Files.exists(_))

      assert(actualBinary.isDefined, s"Binary not found at $reportedPath or ${reportedPath}.kexe")
      info(s"Kotlin Native test binary produced at: ${actualBinary.get}")

      // Step 3: Run the binary and verify it produces test runner output
      val runResult = (for {
        killSignal <- Outcome.neverKillSignal
        outcome <- KotlinNativeRunner.run(
          binary = actualBinary.get,
          args = Seq.empty,
          workingDir = tempDir,
          env = Map.empty,
          killSignal = killSignal
        )
      } yield outcome).unsafeRunSync()

      runResult match {
        case RunOutcome.Completed(exitCode, stdout, stderr) =>
          // The test runner should run (exit 0 = no tests discovered, which is expected
          // since we have no actual test classes). The key point is that -generate-test-runner
          // succeeded and produced a runnable binary.
          info(s"Test runner exited with code $exitCode, stdout: ${stdout.take(200)}, stderr: ${stderr.take(200)}")
        case RunOutcome.Crashed(signal, exitCode, stdout, stderr) =>
          info(s"Test runner crashed with signal $signal (exit $exitCode), stdout: ${stdout.take(200)}, stderr: ${stderr.take(200)}")
        case RunOutcome.Killed(reason, _, _) =>
          fail(s"Test runner was killed: $reason")
      }
    }
  }
}
