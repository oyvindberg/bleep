package bleep.analysis

import bleep.bsp.{KotlinTestRunner, Outcome, TestRunnerTypes}
import bleep.bsp.protocol.{OutputChannel, TestStatus}
import cats.effect.unsafe.implicits.global
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}
import scala.collection.mutable

/** Env forwarding on the Kotlin platforms.
  *
  * `bleep test` gives every platform the same environment, so these runners must honor their `env` argument exactly as the JVM and Scala.js ones do. Each probe
  * is run twice — with the variable and without — because a runner that ignored `env` entirely would still pass a one-sided assertion if the value happened to
  * be inherited from this JVM.
  */
class KotlinTestRunnerEnvTest extends AnyFunSuite with Matchers {
  private val VarName = "BLEEP_KOTLIN_ENV_PROBE"

  private def createTempDir(prefix: String): Path = Files.createTempDirectory(prefix)

  private def deleteRecursively(path: Path): Unit =
    if (Files.exists(path)) {
      if (Files.isDirectory(path)) {
        import scala.jdk.StreamConverters._
        Files.list(path).toScala(List).foreach(deleteRecursively)
      }
      Files.delete(path)
    }

  private class RecordingEventHandler extends TestRunnerTypes.TestEventHandler {
    val outputs: mutable.ArrayBuffer[String] = mutable.ArrayBuffer.empty
    def onTestStarted(suite: String, test: String): Unit = ()
    def onTestFinished(suite: String, test: String, status: TestStatus, durationMs: Long, message: Option[String]): Unit = ()
    def onSuiteStarted(suite: String): Unit = ()
    def onSuiteFinished(suite: String, passed: Int, failed: Int, skipped: Int): Unit = ()
    def onOutput(suite: String, line: String, channel: OutputChannel): Unit = outputs += line
    override def onRunnerEvent(event: TestRunnerTypes.RunnerEvent): Unit = ()
  }

  // ==========================================================================
  // Kotlin/Native — GoogleTest-style output from a mock binary
  // ==========================================================================

  /** A stand-in for a Kotlin/Native test binary. Reports OK or FAILED purely from the environment it was given. */
  private val nativeProbeScript =
    s"""#!/bin/bash
       |echo "[----------] 1 tests from EnvSuite"
       |echo "[ RUN      ] EnvSuite.env"
       |if [ "$$$VarName" = "from-client" ]; then
       |  echo "[       OK ] EnvSuite.env (0 ms)"
       |else
       |  echo "[  FAILED  ] EnvSuite.env (0 ms)"
       |fi
       |echo "[==========] 1 tests ran"
       |exit 0
       |""".stripMargin

  private def runNativeProbe(env: Map[String, String]): TestRunnerTypes.TestResult = {
    val tempDir = createTempDir("kotlin-native-env")
    try {
      val binary = tempDir.resolve("test-binary")
      Files.writeString(binary, nativeProbeScript)
      binary.toFile.setExecutable(true): Unit
      (for {
        killSignal <- Outcome.neverKillSignal
        res <- KotlinTestRunner.Native.runTests(binary, List.empty, new RecordingEventHandler(), env, tempDir, killSignal)
      } yield res).unsafeRunSync()
    } finally deleteRecursively(tempDir)
  }

  test("Kotlin/Native: forwards env vars to the test binary") {
    assume(!scala.util.Properties.isWin, "mock binary is a shell script")
    assert(sys.env.get(VarName).isEmpty, "probe var must not be set in this JVM or the test proves nothing")
    val result = runNativeProbe(Map(VarName -> "from-client"))
    result.passed shouldBe 1
    result.failed shouldBe 0
  }

  test("Kotlin/Native: the env probe genuinely fails without the var (control)") {
    assume(!scala.util.Properties.isWin, "mock binary is a shell script")
    val result = runNativeProbe(Map.empty)
    result.passed shouldBe 0
    result.failed shouldBe 1
  }

  // ==========================================================================
  // Kotlin/JS — a module registering against the runner's QUnit shim
  // ==========================================================================

  /** Stands in for a linked Kotlin/JS module. The runner defines `global.QUnit` before requiring this, so registering here is exactly what real Kotlin/JS
    * output does via the kotlin-test QUnit adapter. The nested-callback form of `module` is required: the shim restores the previous module name when called
    * without one, so tests registered after a bare `QUnit.module('X')` would land in the default suite.
    */
  private val jsProbeModule =
    s"""
       |QUnit.module('EnvSuite', function() {
       |  QUnit.test('env', function(assert) {
       |    assert.equal(process.env['$VarName'], 'from-client', 'env var should reach the node process');
       |  });
       |});
       |""".stripMargin

  private def runJsProbe(env: Map[String, String]): TestRunnerTypes.TestResult = {
    val tempDir = createTempDir("kotlin-js-env")
    try {
      val jsFile = tempDir.resolve("module.js")
      Files.writeString(jsFile, jsProbeModule)
      (for {
        killSignal <- Outcome.neverKillSignal
        res <- KotlinTestRunner.Js.runTests(
          jsFile,
          List(TestRunnerTypes.TestSuite("EnvSuite", "EnvSuite")),
          new RecordingEventHandler(),
          PlatformTestHelper.nodeBinary,
          env,
          killSignal
        )
      } yield res).unsafeRunSync()
    } finally deleteRecursively(tempDir)
  }

  test("Kotlin/JS: forwards env vars to the node process") {
    assert(sys.env.get(VarName).isEmpty, "probe var must not be set in this JVM or the test proves nothing")
    val result = runJsProbe(Map(VarName -> "from-client"))
    result.passed shouldBe 1
    result.failed shouldBe 0
  }

  test("Kotlin/JS: the env probe genuinely fails without the var (control)") {
    val result = runJsProbe(Map.empty)
    result.passed shouldBe 0
    result.failed shouldBe 1
  }
}
