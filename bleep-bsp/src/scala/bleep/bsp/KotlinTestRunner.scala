package bleep.bsp

import bleep.bsp.Outcome.KillReason
import bleep.bsp.TestRunnerTypes.{StderrBuffer, TerminationReason, TestEventHandler, TestResult, TestSuite}
import bleep.bsp.protocol.{OutputChannel, TestStatus}
import cats.effect.{Deferred, IO, Ref}
import cats.syntax.all._
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters._

/** Runners for Kotlin/JS and Kotlin/Native tests.
  *
  * Kotlin tests use the kotlin.test framework which provides:
  *   - Annotations: @Test, @Ignore, @BeforeTest, @AfterTest
  *   - Assertions: assertEquals, assertTrue, assertFails, etc.
  *
  * Kotlin/JS: Runs tests via Node.js with kotlin-test-js Kotlin/Native: Runs tests as native binary with kotlin-test-native
  */
object KotlinTestRunner {

  // ==========================================================================
  // Kotlin/JS Test Runner
  // ==========================================================================

  object Js {

    /** Discover test suites from Kotlin/JS output.
      *
      * @param jsOutput
      *   the compiled JavaScript file
      * @param killSignal
      *   Deferred to complete to request termination
      * @return
      *   discovery result
      */
    def discoverSuites(
        jsOutput: Path,
        killSignal: Deferred[IO, KillReason]
    ): IO[ProcessRunner.DiscoveryResult[List[TestSuite]]] =
      killSignal.tryGet.flatMap {
        case Some(reason) => IO.pure(ProcessRunner.DiscoveryResult.Killed(reason))
        case None         =>
          IO.blocking {
            val discoveryScript = createDiscoveryScript(jsOutput)
            val scriptPath = Files.createTempFile("kotlin-js-discover-", ".js")
            Files.writeString(scriptPath, discoveryScript)
            scriptPath
          }.flatMap { scriptPath =>
            val pb = new ProcessBuilder("node", scriptPath.toAbsolutePath.toString)
              .directory(jsOutput.getParent.toFile)
              .redirectErrorStream(true)

            val work = ProcessRunner.start(pb).use { process =>
              ProcessRunner.lines(process.getInputStream).compile.toList.flatMap { outputLines =>
                IO.blocking(process.waitFor()).map { exitCode =>
                  val output = outputLines.mkString("\n")
                  if (exitCode == 0) {
                    ProcessRunner.DiscoveryResult.Found(parseDiscoveryOutput(output))
                  } else {
                    ProcessRunner.DiscoveryResult.Failed(
                      s"Kotlin/JS test discovery failed (exit code $exitCode): $output"
                    )
                  }
                }
              }
            }

            Outcome
              .raceKill(killSignal)(work)
              .map {
                case Left(result)  => result
                case Right(reason) => ProcessRunner.DiscoveryResult.Killed(reason)
              }
              .guarantee(IO.blocking(Files.deleteIfExists(scriptPath)).void)
          }
      }

    /** Run Kotlin/JS tests via Node.js.
      *
      * @param jsOutput
      *   the compiled JavaScript file
      * @param suites
      *   the test suites to run
      * @param eventHandler
      *   handler for test events
      * @param env
      *   environment variables
      * @param cancellation
      *   cancellation token
      * @return
      *   test result summary
      */
    def runTests(
        jsOutput: Path,
        suites: List[TestSuite],
        eventHandler: TestEventHandler,
        env: Map[String, String],
        killSignal: Deferred[IO, KillReason]
    ): IO[TestResult] =
      killSignal.tryGet.flatMap {
        case Some(reason) => IO.pure(TestResult(0, 0, 0, 0, TerminationReason.Killed(reason)))
        case None         =>
          IO.blocking {
            val runnerScript = createTestRunnerScript(jsOutput, suites)
            val scriptPath = Files.createTempFile("kotlin-js-test-", ".js")
            Files.writeString(scriptPath, runnerScript)
            scriptPath
          }.flatMap { scriptPath =>
            val pb = new ProcessBuilder("node", scriptPath.toAbsolutePath.toString)
              .directory(jsOutput.getParent.toFile)
            env.foreach { case (k, v) => pb.environment().put(k, v) }

            (
              Ref.of[IO, ProcessTestRunner.RunState](ProcessTestRunner.RunState(0, 0, 0, 0, None)),
              StderrBuffer.create(eventHandler)
            ).flatMapN { (stateRef, stderrBuffer) =>
              ProcessTestRunner.run(
                ProcessTestRunner.Config(
                  processBuilder = pb,
                  handleStdoutLine = { line =>
                    parseTestEvent(line) match {
                      case Some(TestEvent.SuiteStarted(suite)) =>
                        stateRef.update(_.copy(currentSuite = Some(suite))) >>
                          stderrBuffer.drain(suite) >>
                          IO.delay(eventHandler.onSuiteStarted(suite))

                      case Some(TestEvent.SuiteFinished(suite, p, f, s)) =>
                        IO.delay(eventHandler.onSuiteFinished(suite, p, f, s)) >>
                          stateRef.update(st =>
                            st.copy(
                              passed = st.passed + p,
                              failed = st.failed + f,
                              skipped = st.skipped + s,
                              currentSuite = None
                            )
                          )

                      case Some(TestEvent.TestStarted(suite, test)) =>
                        IO.delay(eventHandler.onTestStarted(suite, test))

                      case Some(TestEvent.TestFinished(suite, test, status, duration, msg)) =>
                        val testStatus = TestStatus.fromString(status)
                        IO.delay(eventHandler.onTestFinished(suite, test, testStatus, duration, msg))

                      case None =>
                        stateRef.get.flatMap { state =>
                          state.currentSuite match {
                            case Some(suite) => IO.delay(eventHandler.onOutput(suite, line, OutputChannel.Stdout))
                            case None        => IO.unit
                          }
                        }
                    }
                  },
                  handleStderrLine = { line =>
                    stateRef.get.flatMap { state =>
                      state.currentSuite match {
                        case Some(suite) => IO.delay(eventHandler.onOutput(suite, line, OutputChannel.Stderr))
                        case None        => stderrBuffer.buffer(line)
                      }
                    }
                  },
                  getRunState = stateRef.get,
                  eventHandler = eventHandler,
                  killSignal = killSignal,
                  killDescendants = false,
                  preRun = IO.unit,
                  onNormalExit = IO.unit,
                  cleanup = IO.blocking(Files.deleteIfExists(scriptPath)).void
                )
              )
            }
          }
      }

    private def createDiscoveryScript(jsOutput: Path): String =
      s"""
         |const fs = require('fs');
         |const vm = require('vm');
         |const path = require('path');
         |
         |const jsPath = '${jsOutput.toAbsolutePath.toString.replace("\\", "\\\\").replace("'", "\\'")}';
         |const jsCode = fs.readFileSync(jsPath, 'utf-8');
         |
         |const sandbox = {
         |  require: require,
         |  console: console,
         |  process: process,
         |  module: { exports: {} },
         |  exports: {}
         |};
         |
         |try {
         |  vm.runInNewContext(jsCode, sandbox);
         |  const exports = sandbox.module.exports || sandbox.exports || sandbox;
         |
         |  const suites = [];
         |  function findTests(obj, prefix) {
         |    if (!obj || typeof obj !== 'object') return;
         |    for (const key of Object.keys(obj)) {
         |      const val = obj[key];
         |      const fullName = prefix ? prefix + '.' + key : key;
         |      // Look for classes with test methods
         |      if (typeof val === 'function' && hasTestMethods(val)) {
         |        suites.push({ name: key, fullyQualifiedName: fullName });
         |      } else if (typeof val === 'object') {
         |        findTests(val, fullName);
         |      }
         |    }
         |  }
         |
         |  function hasTestMethods(ctor) {
         |    if (!ctor.prototype) return false;
         |    const proto = ctor.prototype;
         |    return Object.getOwnPropertyNames(proto).some(name =>
         |      name.startsWith('test') || name.endsWith('_test') ||
         |      (typeof proto[name] === 'function' && proto[name].$$testMarker)
         |    );
         |  }
         |
         |  findTests(exports, '');
         |  console.log(JSON.stringify(suites));
         |} catch (err) {
         |  console.error('Discovery failed:', err.message);
         |  console.log('[]');
         |}
         |""".stripMargin

    private def createTestRunnerScript(jsOutput: Path, suites: List[TestSuite]): String = {
      // Kotlin/JS kotlin.test uses QUnit adapter by default, so we need to provide QUnit stubs
      // that capture and run the registered tests
      s"""
         |const PREFIX = '##kotlin-test##';
         |
         |// Exit when parent dies (stdin closes)
         |process.stdin.on('end', () => process.exit(1));
         |process.stdin.resume();
         |
         |function emit(event, ...args) {
         |  console.log(PREFIX + event + '|' + args.join('|'));
         |}
         |
         |// QUnit mock that captures registered tests
         |const registeredTests = [];
         |let currentModule = '';
         |
         |global.QUnit = {
         |  module: function(name, fn) {
         |    const prevModule = currentModule;
         |    currentModule = currentModule ? currentModule + '.' + name : name;
         |    if (typeof fn === 'function') fn();
         |    currentModule = prevModule;
         |  },
         |  test: function(name, fn) {
         |    registeredTests.push({ module: currentModule, name: name, fn: fn, skipped: false });
         |  },
         |  skip: function(name, fn) {
         |    registeredTests.push({ module: currentModule, name: name, fn: fn, skipped: true });
         |  }
         |};
         |
         |// Assert object for QUnit-style assertions
         |function createAssert() {
         |  return {
         |    ok: function(value, message) {
         |      if (!value) throw new Error(message || 'Assertion failed: expected truthy value');
         |    },
         |    equal: function(actual, expected, message) {
         |      if (actual !== expected) throw new Error(message || 'Assertion failed: ' + actual + ' !== ' + expected);
         |    },
         |    deepEqual: function(actual, expected, message) {
         |      if (JSON.stringify(actual) !== JSON.stringify(expected)) {
         |        throw new Error(message || 'Assertion failed: deep equality');
         |      }
         |    },
         |    expect: function(count) { /* no-op */ }
         |  };
         |}
         |
         |async function runTests() {
         |  const jsPath = '${jsOutput.toAbsolutePath.toString.replace("\\", "\\\\").replace("'", "\\'")}';
         |
         |  // Load the Kotlin/JS module - this registers tests via QUnit.test() calls
         |  require(jsPath);
         |
         |  // Group tests by module
         |  const testsByModule = {};
         |  for (const test of registeredTests) {
         |    const moduleName = test.module || 'default';
         |    if (!testsByModule[moduleName]) testsByModule[moduleName] = [];
         |    testsByModule[moduleName].push(test);
         |  }
         |
         |  // Run tests
         |  for (const [moduleName, tests] of Object.entries(testsByModule)) {
         |    emit('suite-started', moduleName);
         |    let passed = 0, failed = 0, skipped = 0;
         |
         |    for (const test of tests) {
         |      const testName = test.name;
         |      emit('test-started', moduleName, testName);
         |      const start = Date.now();
         |
         |      if (test.skipped) {
         |        emit('test-finished', moduleName, testName, 'skipped', Date.now() - start, '');
         |        skipped++;
         |        continue;
         |      }
         |
         |      try {
         |        const assert = createAssert();
         |        const result = test.fn(assert);
         |        if (result && typeof result.then === 'function') {
         |          await result;
         |        }
         |        emit('test-finished', moduleName, testName, 'passed', Date.now() - start, '');
         |        passed++;
         |      } catch (err) {
         |        emit('test-finished', moduleName, testName, 'failed', Date.now() - start, err.message || String(err));
         |        failed++;
         |      }
         |    }
         |
         |    emit('suite-finished', moduleName, passed, failed, skipped);
         |  }
         |
         |  process.exit(0);
         |}
         |
         |runTests().catch(err => {
         |  emit('output', 'runner', 'Fatal error: ' + err.message, 'true');
         |  process.exit(1);
         |});
         |""".stripMargin
    }
  }

  // ==========================================================================
  // Kotlin/Native Test Runner
  // ==========================================================================

  object Native {

    /** Discover test suites from Kotlin/Native binary.
      *
      * Runs binary with --ktest_list_tests. Non-zero exit means listing not supported; returns Found(empty) so the caller can run all tests.
      *
      * @param binary
      *   the compiled native binary
      * @param killSignal
      *   Deferred to complete to request termination
      * @return
      *   discovery result
      */
    def discoverSuites(
        binary: Path,
        killSignal: Deferred[IO, KillReason]
    ): IO[ProcessRunner.DiscoveryResult[List[TestSuite]]] =
      killSignal.tryGet.flatMap {
        case Some(reason)                        => IO.pure(ProcessRunner.DiscoveryResult.Killed(reason))
        case None if !Files.isExecutable(binary) =>
          IO.pure(ProcessRunner.DiscoveryResult.Failed(s"Kotlin/Native binary is not executable: $binary"))
        case None =>
          val pb = new ProcessBuilder(binary.toAbsolutePath.toString, "--ktest_list_tests")
            .redirectErrorStream(true)

          // --ktest_list_tests is not supported by all binaries; non-zero exit = run all
          val work = ProcessRunner
            .start(pb)
            .use { process =>
              ProcessRunner.lines(process.getInputStream).compile.toList.flatMap { outputLines =>
                IO.blocking(process.waitFor()).map { exitCode =>
                  if (exitCode == 0) {
                    Right(
                      outputLines
                        .filter(_.nonEmpty)
                        .map { line =>
                          val name = line.split('.').lastOption.getOrElse(line)
                          TestSuite(name, line.trim)
                        }
                    )
                  } else {
                    // Non-zero exit is expected when --ktest_list_tests is not supported
                    // Return empty list meaning "run all tests"
                    Right(List.empty[TestSuite])
                  }
                }
              }
            }
            .handleErrorWith { err =>
              IO.pure(Left(s"Failed to discover Kotlin/Native test suites: ${err.getMessage}"))
            }

          Outcome.raceKill(killSignal)(work).map {
            case Left(Right(result))  => ProcessRunner.DiscoveryResult.Found(result)
            case Left(Left(errorMsg)) => ProcessRunner.DiscoveryResult.Failed(errorMsg)
            case Right(reason)        => ProcessRunner.DiscoveryResult.Killed(reason)
          }
      }

    /** Run Kotlin/Native tests.
      *
      * @param binary
      *   the compiled native binary
      * @param suites
      *   the test suites to run (empty = all)
      * @param eventHandler
      *   handler for test events
      * @param env
      *   environment variables
      * @param workingDir
      *   working directory for execution
      * @param cancellation
      *   cancellation token
      * @return
      *   test result summary
      */
    /** Internal state for Kotlin/Native test output parsing. */
    private case class NativeRunState(
        passed: Int,
        failed: Int,
        skipped: Int,
        ignored: Int,
        currentSuite: Option[String],
        suitePassed: Int,
        suiteFailed: Int,
        suiteSkipped: Int
    )

    private object NativeRunState {
      val empty: NativeRunState = NativeRunState(0, 0, 0, 0, None, 0, 0, 0)
    }

    // Kotlin/Native test output patterns (Google Test format)
    // Example output:
    //   [==========] Running 5 tests from 1 test cases.
    //   [----------] 5 tests from example.KotlinNativeTest
    //   [ RUN      ] example.KotlinNativeTest.testAddItem
    //   [       OK ] example.KotlinNativeTest.testAddItem (0 ms)
    //   [  PASSED  ] 5 tests.
    private val suiteStartPattern = """^\[----------\]\s+\d+\s+tests?\s+from\s+(.+)$""".r
    private val testRunPattern = """^\[\s+RUN\s+\]\s+(.+)$""".r
    private val testOkPattern = """^\[\s+OK\s+\]\s+(.+?)(?:\s+\(\d+\s+ms\))?$""".r
    private val testFailedPattern = """^\[\s+FAILED\s+\]\s+(.+?)(?:\s+\(\d+\s+ms\))?$""".r
    private val summaryPattern = """^\[==========\]\s+(\d+)\s+tests?.*ran""".r

    def runTests(
        binary: Path,
        suites: List[TestSuite],
        eventHandler: TestEventHandler,
        env: Map[String, String],
        workingDir: Path,
        killSignal: Deferred[IO, KillReason]
    ): IO[TestResult] =
      killSignal.tryGet.flatMap {
        case Some(reason) => IO.pure(TestResult(0, 0, 0, 0, TerminationReason.Killed(reason)))
        case None         =>
          IO.blocking {
            if (!Files.isExecutable(binary)) {
              binary.toFile.setExecutable(true): Unit
            }
          } >> {
            val command = if (suites.isEmpty) {
              Seq(binary.toAbsolutePath.toString)
            } else {
              Seq(binary.toAbsolutePath.toString, "--ktest_filter=" + suites.map(_.fullyQualifiedName).mkString(":"))
            }

            val pb = new ProcessBuilder(command.asJava)
              .directory(workingDir.toFile)
            env.foreach { case (k, v) => pb.environment().put(k, v) }

            (Ref.of[IO, NativeRunState](NativeRunState.empty), StderrBuffer.create(eventHandler)).flatMapN { (stateRef, stderrBuffer) =>
              def finishCurrentSuite: IO[Unit] =
                stateRef
                  .modify { st =>
                    st.currentSuite match {
                      case Some(s) =>
                        val newSt = st.copy(
                          passed = st.passed + st.suitePassed,
                          failed = st.failed + st.suiteFailed,
                          skipped = st.skipped + st.suiteSkipped,
                          currentSuite = None,
                          suitePassed = 0,
                          suiteFailed = 0,
                          suiteSkipped = 0
                        )
                        (newSt, Some((s, st.suitePassed, st.suiteFailed, st.suiteSkipped)))
                      case None =>
                        (st, None)
                    }
                  }
                  .flatMap {
                    case Some((s, p, f, sk)) => IO.delay(eventHandler.onSuiteFinished(s, p, f, sk))
                    case None                => IO.unit
                  }

              ProcessTestRunner.run(
                ProcessTestRunner.Config(
                  processBuilder = pb,
                  handleStdoutLine = { line =>
                    line match {
                      case suiteStartPattern(suite) =>
                        finishCurrentSuite >>
                          stateRef.update(_.copy(currentSuite = Some(suite), suitePassed = 0, suiteFailed = 0, suiteSkipped = 0)) >>
                          IO.delay(eventHandler.onSuiteStarted(suite)) >>
                          stderrBuffer.drain(suite)

                      case testRunPattern(test) =>
                        stateRef.get.flatMap { st =>
                          st.currentSuite.traverse_ { suite =>
                            val testName = test.split('.').lastOption.getOrElse(test)
                            IO.delay(eventHandler.onTestStarted(suite, testName))
                          }
                        }

                      case testOkPattern(test) =>
                        stateRef.get.flatMap { st =>
                          st.currentSuite.traverse_ { suite =>
                            val testName = test.split('.').lastOption.getOrElse(test)
                            IO.delay(eventHandler.onTestFinished(suite, testName, TestStatus.Passed, 0, None))
                          }
                        } >> stateRef.update(st => st.copy(suitePassed = st.suitePassed + 1))

                      case testFailedPattern(test) =>
                        stateRef.get.flatMap { st =>
                          st.currentSuite.traverse_ { suite =>
                            val testName = test.split('.').lastOption.getOrElse(test)
                            IO.delay(eventHandler.onTestFinished(suite, testName, TestStatus.Failed, 0, None))
                          }
                        } >> stateRef.update(st => st.copy(suiteFailed = st.suiteFailed + 1))

                      case summaryPattern(_) =>
                        finishCurrentSuite

                      case _ =>
                        stateRef.get.flatMap { st =>
                          st.currentSuite.traverse_ { suite =>
                            if (line.trim.nonEmpty) IO.delay(eventHandler.onOutput(suite, line, OutputChannel.Stdout))
                            else IO.unit
                          }
                        }
                    }
                  },
                  handleStderrLine = { line =>
                    stateRef.get.flatMap { st =>
                      st.currentSuite match {
                        case Some(suite) => IO.delay(eventHandler.onOutput(suite, line, OutputChannel.Stderr))
                        case None        => stderrBuffer.buffer(line)
                      }
                    }
                  },
                  getRunState = stateRef.get.map { state =>
                    ProcessTestRunner.RunState(
                      state.passed + state.suitePassed,
                      state.failed + state.suiteFailed,
                      state.skipped + state.suiteSkipped,
                      state.ignored,
                      state.currentSuite
                    )
                  },
                  eventHandler = eventHandler,
                  killSignal = killSignal,
                  killDescendants = false,
                  preRun = IO.unit,
                  onNormalExit = IO.unit,
                  cleanup = IO.unit
                )
              )
            }
          }
      }
  }

  // ==========================================================================
  // Shared Utilities
  // ==========================================================================

  /** Parsed test event from runner output. */
  private sealed trait TestEvent
  private object TestEvent {
    case class SuiteStarted(suite: String) extends TestEvent
    case class SuiteFinished(suite: String, passed: Int, failed: Int, skipped: Int) extends TestEvent
    case class TestStarted(suite: String, test: String) extends TestEvent
    case class TestFinished(suite: String, test: String, status: String, durationMs: Long, message: Option[String]) extends TestEvent
  }

  /** Parse a line of output for test events. */
  private def parseTestEvent(line: String): Option[TestEvent] = {
    val prefix = "##kotlin-test##"
    if (!line.startsWith(prefix)) return None

    val parts = line.substring(prefix.length).split("\\|", -1)
    if (parts.isEmpty) return None

    parts(0) match {
      case "suite-started" if parts.length >= 2 =>
        Some(TestEvent.SuiteStarted(parts(1)))

      case "suite-finished" if parts.length >= 5 =>
        Some(
          TestEvent.SuiteFinished(
            parts(1),
            parts(2).toInt,
            parts(3).toInt,
            parts(4).toInt
          )
        )

      case "test-started" if parts.length >= 3 =>
        Some(TestEvent.TestStarted(parts(1), parts(2)))

      case "test-finished" if parts.length >= 5 =>
        val message = if (parts.length > 5 && parts(5).nonEmpty) Some(parts(5)) else None
        Some(
          TestEvent.TestFinished(
            parts(1),
            parts(2),
            parts(3),
            parts(4).toLong,
            message
          )
        )

      case _ => None
    }
  }

  /** Parse discovery output JSON.
    *
    * @throws IllegalArgumentException
    *   if the output format is completely unexpected
    */
  private def parseDiscoveryOutput(output: String): List[TestSuite] = {
    val trimmed = output.trim
    // Empty output or non-JSON output means no tests discovered
    if (trimmed.isEmpty || !trimmed.startsWith("[")) return List.empty

    // Simple regex-based JSON parsing
    val suitePattern = """\{\s*"name"\s*:\s*"([^"]+)"\s*,\s*"fullyQualifiedName"\s*:\s*"([^"]+)"\s*\}""".r
    suitePattern
      .findAllMatchIn(trimmed)
      .map { m =>
        TestSuite(m.group(1), m.group(2))
      }
      .toList
  }
}
