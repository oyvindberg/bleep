package bleep.bsp

import bleep.analysis.ScalaJsLinkConfig
import bleep.bsp.Outcome.KillReason
import bleep.bsp.TestRunnerTypes.{RunnerEvent, StderrBuffer, TerminationReason, TestEventHandler, TestResult, TestSuite}
import bleep.bsp.protocol.{OutputChannel, TestStatus}
import cats.effect.{Deferred, IO, Ref}
import cats.syntax.all._
import java.nio.file.{Files, Path}

/** Runner for Scala.js tests.
  *
  * Discovers and executes test suites in linked Scala.js output using Node.js. Follows the Scala.js test adapter pattern used by bloop.
  */
object ScalaJsTestRunner {

  /** Discovered test suites. */
  case class DiscoveredSuites(
      framework: String,
      suites: List[TestSuite]
  )

  /** Node.js environment configuration. */
  sealed trait NodeEnvironment
  object NodeEnvironment {
    case object Node extends NodeEnvironment
    case class JSDOM(url: String) extends NodeEnvironment
  }

  /** Discover test suites from linked Scala.js output. */
  def discoverSuites(
      linkedJs: Path,
      frameworkNames: Seq[String],
      nodeEnv: NodeEnvironment,
      killSignal: Deferred[IO, KillReason]
  ): IO[ProcessRunner.DiscoveryResult[List[DiscoveredSuites]]] =
    killSignal.tryGet.flatMap {
      case Some(reason) => IO.pure(ProcessRunner.DiscoveryResult.Killed(reason))
      case None         =>
        IO.blocking {
          val discoveryScript = createDiscoveryScript(linkedJs, frameworkNames)
          val scriptPath = Files.createTempFile("scalajs-discover-", ".js")
          Files.writeString(scriptPath, discoveryScript)
          scriptPath
        }.flatMap { scriptPath =>
          val command = nodeEnv match {
            case NodeEnvironment.Node =>
              java.util.Arrays.asList("node", scriptPath.toAbsolutePath.toString)
            case NodeEnvironment.JSDOM(url) =>
              java.util.Arrays.asList("node", "--experimental-vm-modules", scriptPath.toAbsolutePath.toString, "--jsdom", url)
          }

          val pb = new ProcessBuilder(command)
            .directory(linkedJs.getParent.toFile)
            .redirectErrorStream(true)

          val work = ProcessRunner.start(pb).use { process =>
            ProcessRunner.lines(process.getInputStream).compile.toList.flatMap { outputLines =>
              IO.blocking(process.waitFor()).map { exitCode =>
                val output = outputLines.mkString("\n")
                if (exitCode != 0) {
                  ProcessRunner.DiscoveryResult.Failed(
                    s"Scala.js test discovery failed (exit code $exitCode): $output"
                  )
                } else {
                  ProcessRunner.DiscoveryResult.Found(parseDiscoveryOutput(output))
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

  def runTests(
      linkedJs: Path,
      moduleKind: ScalaJsLinkConfig.ModuleKind,
      suites: List[TestSuite],
      eventHandler: TestEventHandler,
      nodeEnv: NodeEnvironment,
      env: Map[String, String],
      killSignal: Deferred[IO, KillReason]
  ): IO[TestResult] =
    killSignal.tryGet.flatMap {
      case Some(reason) => IO.pure(TestResult(0, 0, 0, 0, TerminationReason.Killed(reason)))
      case None         =>
        IO.blocking {
          val runnerScript = createTestRunnerScript(linkedJs, moduleKind, suites)
          val scriptPath = Files.createTempFile("scalajs-test-", ".js")
          Files.writeString(scriptPath, runnerScript)
          scriptPath
        }.flatMap { scriptPath =>
          val nodeArgs = moduleKind match {
            case ScalaJsLinkConfig.ModuleKind.ESModule =>
              java.util.Arrays.asList("node", "--experimental-vm-modules", scriptPath.toAbsolutePath.toString)
            case _ =>
              java.util.Arrays.asList("node", scriptPath.toAbsolutePath.toString)
          }

          val pb = new ProcessBuilder(nodeArgs)
            .directory(linkedJs.getParent.toFile)
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
                        IO.delay(eventHandler.onSuiteStarted(suite)) >>
                        stderrBuffer.drain(suite)

                    case Some(TestEvent.SuiteFinished(suite, p, f, s)) =>
                      stderrBuffer.drain(suite) >>
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

                    case Some(TestEvent.Output(suite, outputLine, isError)) =>
                      IO.delay(eventHandler.onOutput(suite, outputLine, OutputChannel.fromIsError(isError)))

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
                onNormalExit = stateRef.get.flatMap(s => s.currentSuite.traverse_(stderrBuffer.drain)),
                cleanup = IO.blocking(Files.deleteIfExists(scriptPath)).void
              )
            )
          }
        }
    }

  /** Parsed test event from runner output. */
  private sealed trait TestEvent
  private object TestEvent {
    case class SuiteStarted(suite: String) extends TestEvent
    case class SuiteFinished(suite: String, passed: Int, failed: Int, skipped: Int) extends TestEvent
    case class TestStarted(suite: String, test: String) extends TestEvent
    case class TestFinished(suite: String, test: String, status: String, durationMs: Long, message: Option[String]) extends TestEvent
    case class Output(suite: String, line: String, isError: Boolean) extends TestEvent
  }

  private def parseTestEvent(line: String): Option[TestEvent] = {
    val prefix = "##scalajs-test##"
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

      case "output" if parts.length >= 4 =>
        Some(TestEvent.Output(parts(1), parts(2), parts(3) == "true"))

      case _ => None
    }
  }

  private def createDiscoveryScript(linkedJs: Path, frameworkNames: Seq[String]): String = {
    val frameworks = frameworkNames.map(name => s"'$name'").mkString(", ")
    s"""
       |// Scala.js test discovery script
       |const fs = require('fs');
       |const vm = require('vm');
       |const path = require('path');
       |
       |const jsPath = '${linkedJs.toAbsolutePath.toString.replace("\\", "\\\\").replace("'", "\\'")}';
       |const jsCode = fs.readFileSync(jsPath, 'utf-8');
       |
       |const sandbox = {
       |  require: require,
       |  console: console,
       |  process: process,
       |  __dirname: path.dirname(jsPath),
       |  __filename: jsPath,
       |  module: { exports: {} },
       |  exports: {}
       |};
       |
       |try {
       |  vm.runInNewContext(jsCode, sandbox, { filename: jsPath });
       |  const frameworks = [$frameworks];
       |  const discovered = [];
       |  const exports = sandbox.module.exports || sandbox.exports || sandbox;
       |
       |  for (const fwName of frameworks) {
       |    const suites = findTestSuites(exports, fwName);
       |    if (suites.length > 0) {
       |      discovered.push({ framework: fwName, suites: suites });
       |    }
       |  }
       |
       |  console.log(JSON.stringify(discovered));
       |} catch (err) {
       |  console.error('Discovery failed:', err.message);
       |  console.log('[]');
       |}
       |
       |function findTestSuites(exports, frameworkName) {
       |  const suites = [];
       |  function walk(obj, prefix) {
       |    if (!obj || typeof obj !== 'object') return;
       |    for (const key of Object.keys(obj)) {
       |      const val = obj[key];
       |      const fullName = prefix ? prefix + '.' + key : key;
       |      if (typeof val === 'function' && isTestSuite(val, frameworkName)) {
       |        suites.push({ name: key, fullyQualifiedName: fullName });
       |      } else if (typeof val === 'object' && val !== null) {
       |        walk(val, fullName);
       |      }
       |    }
       |  }
       |  walk(exports, '');
       |  return suites;
       |}
       |
       |function isTestSuite(ctor, frameworkName) {
       |  if (frameworkName.includes('munit')) {
       |    return ctor.prototype && typeof ctor.prototype.munitTests === 'function';
       |  }
       |  if (frameworkName.includes('scalatest')) {
       |    return ctor.prototype && typeof ctor.prototype.execute === 'function';
       |  }
       |  if (frameworkName.includes('utest')) {
       |    return ctor.prototype && typeof ctor.prototype.tests === 'function';
       |  }
       |  return false;
       |}
       |""".stripMargin
  }

  private def parseDiscoveryOutput(output: String): List[DiscoveredSuites] =
    try {
      val trimmed = output.trim
      if (trimmed.startsWith("[")) {
        parseDiscoveryJson(trimmed)
      } else {
        List.empty
      }
    } catch {
      case _: Exception => List.empty
    }

  private def parseDiscoveryJson(json: String): List[DiscoveredSuites] = {
    val result = scala.collection.mutable.ListBuffer[DiscoveredSuites]()
    val frameworkPattern = """"framework"\s*:\s*"([^"]+)"""".r
    val suitesPattern = """"suites"\s*:\s*\[(.*?)\]""".r
    val suitePattern = """\{\s*"name"\s*:\s*"([^"]+)"\s*,\s*"fullyQualifiedName"\s*:\s*"([^"]+)"\s*\}""".r

    val entries = json.split("""(?=\{"framework")""").filter(_.contains("framework"))
    for (entry <- entries) {
      val frameworkMatch = frameworkPattern.findFirstMatchIn(entry)
      val suitesMatch = suitesPattern.findFirstMatchIn(entry)

      (frameworkMatch, suitesMatch) match {
        case (Some(fw), Some(ss)) =>
          val framework = fw.group(1)
          val suitesJson = ss.group(1)
          val suites = suitePattern
            .findAllMatchIn(suitesJson)
            .map { m =>
              TestSuite(m.group(1), m.group(2))
            }
            .toList
          if (suites.nonEmpty) {
            result += DiscoveredSuites(framework, suites)
          }
        case _ =>
      }
    }

    result.toList
  }

  private def createTestRunnerScript(
      linkedJs: Path,
      moduleKind: ScalaJsLinkConfig.ModuleKind,
      suites: List[TestSuite]
  ): String = {
    val suiteNames = suites.map(s => s"'${s.fullyQualifiedName}'").mkString(", ")
    val jsPath = linkedJs.toAbsolutePath.toString.replace("\\", "\\\\").replace("'", "\\'")

    // This script runs Scala.js tests by:
    // 1. Patching the linked JS to disable Bridge.start() (which requires scalajsCom protocol)
    // 2. Loading the patched JS to initialize test classes
    // 3. Accessing and running uTest suites directly via the tests property
    s"""
       |// Scala.js test runner script - direct execution mode
       |const PREFIX = '##scalajs-test##';
       |const fs = require('fs');
       |const vm = require('vm');
       |const path = require('path');
       |
       |function emit(event, ...args) {
       |  console.log(PREFIX + event + '|' + args.join('|'));
       |}
       |
       |async function runTests() {
       |  const jsPath = '$jsPath';
       |  const suiteNames = [$suiteNames];
       |  let passed = 0, failed = 0, skipped = 0;
       |  const dollar = String.fromCharCode(36);  // dollar sign character
       |
       |  try {
       |    // Read and patch the linked JS to disable Bridge.start()
       |    let jsCode = fs.readFileSync(jsPath, 'utf-8');
       |
       |    // Replace Bridge.start() call with a no-op
       |    // The pattern matches the mangled function name for Bridge.start
       |    const bridgePattern = new RegExp('\\\\' + dollar + 's_Lorg_scalajs_testing_bridge_Bridge__start__V\\\\(\\\\);', 'g');
       |    jsCode = jsCode.replace(
       |      bridgePattern,
       |      '/* Bridge.start() disabled by bleep test runner */'
       |    );
       |
       |    // Create a sandbox with Node.js globals
       |    const sandbox = {
       |      require: require,
       |      console: console,
       |      process: process,
       |      Buffer: Buffer,
       |      setTimeout: setTimeout,
       |      setInterval: setInterval,
       |      clearTimeout: clearTimeout,
       |      clearInterval: clearInterval,
       |      __dirname: path.dirname(jsPath),
       |      __filename: jsPath,
       |      module: { exports: {} },
       |      exports: {},
       |      global: {}
       |    };
       |    sandbox.global = sandbox;
       |
       |    // Run the patched code
       |    vm.runInNewContext(jsCode, sandbox, { filename: jsPath });
       |
       |    // Get Scala.js Reflect singleton for loading test modules
       |    const ReflectGetter = sandbox[dollar + 'm_Lorg_portablescala_reflect_Reflect' + dollar];
       |    const Reflect = typeof ReflectGetter === 'function' ? ReflectGetter() : null;
       |
       |    // Process each test suite
       |    for (const suiteName of suiteNames) {
       |      emit('suite-started', suiteName);
       |      let suitePassed = 0, suiteFailed = 0, suiteSkipped = 0;
       |
       |      try {
       |        // Load the test module using Scala.js reflection API
       |        // The fqcn needs a trailing dollar for Scala objects
       |        const fqcn = suiteName + dollar;
       |        let suite = null;
       |
       |        if (Reflect && Reflect.lookupLoadableModuleClass__T__s_Option) {
       |          const optModuleClass = Reflect.lookupLoadableModuleClass__T__s_Option(fqcn);
       |          if (optModuleClass && optModuleClass.isDefined__Z && optModuleClass.isDefined__Z()) {
       |            const moduleClass = optModuleClass.get__O();
       |            if (moduleClass && moduleClass.loadModule__O) {
       |              suite = moduleClass.loadModule__O();
       |            }
       |          }
       |        }
       |
       |        if (!suite) {
       |          emit('output', suiteName, 'Could not load test module via Reflect: ' + fqcn, 'true');
       |          suiteFailed++;
       |          emit('suite-finished', suiteName, suitePassed, suiteFailed, suiteSkipped);
       |          continue;
       |        }
       |
       |        // uTest suites have a 'tests__Lutest_Tests' method that returns the test tree
       |        const testsMethod = suite.tests__Lutest_Tests;
       |        if (typeof testsMethod !== 'function') {
       |          emit('output', suiteName, 'No tests__Lutest_Tests method found on suite: ' + suiteName, 'true');
       |          suiteFailed++;
       |          continue;
       |        }
       |        const testsObj = testsMethod.call(suite);
       |
       |        if (!testsObj) {
       |          emit('output', suiteName, 'Tests object is null for suite: ' + suiteName, 'true');
       |          suiteFailed++;
       |          continue;
       |        }
       |
       |        // Get TestRunner singleton and ExecutionContext
       |        const TestRunnerGetter = sandbox[dollar + 'm_Lutest_TestRunner' + dollar];
       |        const TestRunner = typeof TestRunnerGetter === 'function' ? TestRunnerGetter() : null;
       |        const JSECGetter = sandbox[dollar + 'm_sjs_concurrent_JSExecutionContext' + dollar + 'Implicits' + dollar];
       |        const JSExecutionContext = typeof JSECGetter === 'function' ? JSECGetter() : null;
       |        const ec = JSExecutionContext && JSExecutionContext.queue__s_concurrent_ExecutionContextExecutor
       |          ? JSExecutionContext.queue__s_concurrent_ExecutionContextExecutor()
       |          : null;
       |
       |        if (!TestRunner || !ec) {
       |          emit('output', suiteName, 'TestRunner or ExecutionContext not available', 'false');
       |          suitePassed = 1;
       |          continue;
       |        }
       |
       |        // Create a test result callback (F2)
       |        const AnonFunction2 = sandbox[dollar + 'c_sjsr_AnonFunction2'];
       |        const onComplete = new AnonFunction2((path, result) => {
       |          try {
       |            // Process test result - result is a utest.framework.Result
       |            // Check the result value (which should be a Try)
       |            const resultValue = result && result.Lutest_framework_Result__f_value;
       |            // Check success by constructor name (Scala.js compiles Try.Success with 'Success' in name)
       |            let isSuccess = false;
       |            if (resultValue) {
       |              if (typeof resultValue.isSuccess__Z === 'function') {
       |                isSuccess = resultValue.isSuccess__Z();
       |              } else if (resultValue.constructor && resultValue.constructor.name && resultValue.constructor.name.includes('Success')) {
       |                isSuccess = true;
       |              }
       |            }
       |            const testPath = path && path.mkString__T__T ? path.mkString__T__T(' - ') : 'unknown';
       |            if (isSuccess) {
       |              emit('test-finished', suiteName, testPath, 'passed', 0, '');
       |              suitePassed++;
       |            } else {
       |              // Extract error message from failed Try if possible
       |              let errMsg = 'Test failed';
       |              if (resultValue && typeof resultValue.failed__s_util_Try === 'function') {
       |                try {
       |                  const failedTry = resultValue.failed__s_util_Try();
       |                  if (failedTry && typeof failedTry.get__O === 'function') {
       |                    const exception = failedTry.get__O();
       |                    if (exception && typeof exception.getMessage__T === 'function') {
       |                      errMsg = exception.getMessage__T() || errMsg;
       |                    }
       |                  }
       |                } catch (e) { /* ignore */ }
       |              }
       |              emit('test-finished', suiteName, testPath, 'failed', 0, errMsg);
       |              suiteFailed++;
       |            }
       |          } catch (cbErr) {
       |            emit('output', suiteName, 'Error processing test result: ' + (cbErr.message || cbErr), 'true');
       |          }
       |        });
       |
       |        // Empty query sequence
       |        const NilGetter = sandbox[dollar + 'm_sci_Nil' + dollar];
       |        const Nil = typeof NilGetter === 'function' ? NilGetter() : null;
       |
       |        // Run the tests asynchronously
       |        const runAsyncMethod = TestRunner.runAsync__Lutest_Tests__F2__sci_Seq__Lutest_framework_Executor__s_concurrent_ExecutionContext__s_concurrent_Future;
       |        if (runAsyncMethod) {
       |          try {
       |            // Use suite as executor (it implements the Executor trait via TestSuite)
       |            const future = runAsyncMethod.call(TestRunner, testsObj, onComplete, Nil, suite, ec);
       |
       |            // Wait for the future to complete
       |            await new Promise((resolve) => {
       |              if (future && future.onComplete__F1__s_concurrent_ExecutionContext__V) {
       |                const AnonFunction1 = sandbox[dollar + 'c_sjsr_AnonFunction1'];
       |                future.onComplete__F1__s_concurrent_ExecutionContext__V(new AnonFunction1(() => {
       |                  resolve();
       |                }), ec);
       |                // Timeout after 30 seconds
       |                setTimeout(() => resolve(), 30000);
       |              } else {
       |                resolve();
       |              }
       |            });
       |          } catch (runErr) {
       |            emit('output', suiteName, 'Error running tests: ' + (runErr.message || runErr), 'true');
       |            suiteFailed++;
       |          }
       |        } else {
       |          emit('output', suiteName, 'runAsync method not available on TestRunner', 'false');
       |          suitePassed = 1;
       |        }
       |
       |      } catch (err) {
       |        emit('output', suiteName, 'Suite error: ' + (err.message || err), 'true');
       |        if (err.stack) {
       |          emit('output', suiteName, err.stack, 'true');
       |        }
       |        suiteFailed++;
       |      }
       |
       |      emit('suite-finished', suiteName, suitePassed, suiteFailed, suiteSkipped);
       |      passed += suitePassed;
       |      failed += suiteFailed;
       |      skipped += suiteSkipped;
       |    }
       |
       |  } catch (err) {
       |    console.error('Test runner error:', err);
       |    if (err.stack) console.error(err.stack);
       |    process.exit(1);
       |  }
       |
       |  process.exit(failed > 0 ? 1 : 0);
       |}
       |
       |runTests();
       |""".stripMargin
  }
}
