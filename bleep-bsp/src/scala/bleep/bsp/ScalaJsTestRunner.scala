package bleep.bsp

import bleep.analysis.{CompilerResolver, ScalaJsLinkConfig}
import bleep.bsp.protocol.KillReason
import bleep.bsp.ScalaCollectionReflection.{fromScalaList, fromScalaOption, toScalaList, toScalaMap}
import bleep.bsp.TestRunnerTypes.{frameworkClassNames, StderrBuffer, TerminationReason, TestEventHandler, TestFramework, TestResult, TestSuite}
import bleep.bsp.protocol.{OutputChannel, TestStatus}
import cats.effect.{Deferred, IO, Ref, Resource}
import cats.syntax.all._
import java.nio.file.{Files, Path}

/** Runner for Scala.js tests.
  *
  * Discovers and executes test suites in linked Scala.js output using Node.js. Follows the Scala.js test adapter pattern used by bloop.
  */
object ScalaJsTestRunner {

  /** Run a linked Scala.js test module through `org.scalajs.testing.adapter.TestAdapter`.
    *
    * The adapter starts Node, speaks to the `org.scalajs.testing.bridge.Bridge` the linker made the module's entry point, and hands back an
    * `sbt.testing.Framework`. `SbtTestDriver` takes it from there.
    *
    * @param linkedJs
    *   the linked main module
    * @param moduleKind
    *   the module kind the link config used. The adapter needs the matching `Input` case to load the file.
    * @param suites
    *   the suites to run. An empty list asks the framework to discover its own.
    * @param framework
    *   the framework whose class names the adapter tries
    * @param eventHandler
    *   the handler bleep reports test progress through
    * @param nodeBinary
    *   the node executable
    * @param env
    *   environment variables for the node process
    * @param scalaJsVersion
    *   the Scala.js version, which decides the adapter artifact
    * @param killSignal
    *   completes when the run is cancelled
    * @return
    *   an action that returns the counts the framework reported
    */
  def runTestsViaAdapter(
      linkedJs: Path,
      moduleKind: ScalaJsLinkConfig.ModuleKind,
      suites: List[TestSuite],
      framework: TestFramework,
      eventHandler: TestEventHandler,
      nodeBinary: String,
      env: Map[String, String],
      scalaJsVersion: String,
      killSignal: Deferred[IO, KillReason]
  ): IO[TestResult] =
    killSignal.tryGet.flatMap {
      case Some(reason) => IO.pure(TestResult(0, 0, 0, 0, TerminationReason.Killed(reason)))
      case None         =>
        val loader = CompilerResolver.getScalaJsTestAdapter(scalaJsVersion).loader

        val work = openAdapter(loader, linkedJs, moduleKind, nodeBinary, env).use { adapter =>
          IO.interruptible {
            val sbtFramework = loadFramework(loader, adapter, framework, linkedJs)
            SbtTestDriver.runFramework(sbtFramework, suites, eventHandler, loader)
          }
        }

        Outcome.raceKill(killSignal)(work).map {
          case Left(result)  => result
          case Right(reason) => TestResult(0, 0, 0, 0, TerminationReason.Killed(reason))
        }
    }

  /** A resource for a started `TestAdapter`. Its release closes the adapter, which stops the node process the adapter started. A `close()` that throws fails
    * the run rather than passing quietly.
    */
  private def openAdapter(
      loader: ClassLoader,
      linkedJs: Path,
      moduleKind: ScalaJsLinkConfig.ModuleKind,
      nodeBinary: String,
      env: Map[String, String]
  ): Resource[IO, AnyRef] = {
    val adapterClass = loader.loadClass("org.scalajs.testing.adapter.TestAdapter")

    val acquire = IO.blocking {
      val configClass = loader.loadClass("org.scalajs.testing.adapter.TestAdapter$Config")
      val config = configClass
        .getMethod("withLogger", loader.loadClass("org.scalajs.logging.Logger"))
        .invoke(configClass.getConstructor().newInstance().asInstanceOf[AnyRef], createScalaJsLogger(loader))
        .asInstanceOf[AnyRef]

      adapterClass
        .getConstructor(loader.loadClass("org.scalajs.jsenv.JSEnv"), loader.loadClass("scala.collection.immutable.Seq"), configClass)
        .newInstance(
          createNodeJsEnv(loader, nodeBinary, env).asInstanceOf[AnyRef],
          toScalaList(List(createInput(loader, linkedJs, moduleKind)), loader).asInstanceOf[AnyRef],
          config
        )
        .asInstanceOf[AnyRef]
    }

    Resource.make(acquire)(adapter => IO.blocking(adapterClass.getMethod("close").invoke(adapter): Unit))
  }

  /** Ask the adapter which of the framework's class names the linked module declares.
    *
    * @throws NoScalaJsTestFrameworkException
    *   when the linked module declares none of them
    */
  private def loadFramework(loader: ClassLoader, adapter: AnyRef, framework: TestFramework, linkedJs: Path): sbt.testing.Framework = {
    val classNames = frameworkClassNames(framework)
    val requested = toScalaList(List(toScalaList(classNames, loader)), loader)

    val loaded = loader
      .loadClass("org.scalajs.testing.adapter.TestAdapter")
      .getMethod("loadFrameworks", loader.loadClass("scala.collection.immutable.List"))
      .invoke(adapter, requested.asInstanceOf[AnyRef])

    fromScalaList[Any](loaded, loader)
      .flatMap(one => fromScalaOption[sbt.testing.Framework](one, loader))
      .headOption
      .getOrElse(throw NoScalaJsTestFrameworkException(framework.name, classNames, linkedJs))
  }

  /** Build the `org.scalajs.jsenv.Input` case that matches the module kind the link used. Reading the module kind from the link config keeps the two from
    * drifting.
    */
  private def createInput(loader: ClassLoader, linkedJs: Path, moduleKind: ScalaJsLinkConfig.ModuleKind): Any = {
    val inputClassName = moduleKind match {
      case ScalaJsLinkConfig.ModuleKind.NoModule       => "org.scalajs.jsenv.Input$Script"
      case ScalaJsLinkConfig.ModuleKind.CommonJSModule => "org.scalajs.jsenv.Input$CommonJSModule"
      case ScalaJsLinkConfig.ModuleKind.ESModule       => "org.scalajs.jsenv.Input$ESModule"
    }
    loader.loadClass(inputClassName).getConstructor(classOf[Path]).newInstance(linkedJs.toAbsolutePath)
  }

  private def createNodeJsEnv(loader: ClassLoader, nodeBinary: String, env: Map[String, String]): Any = {
    val configClass = loader.loadClass("org.scalajs.jsenv.nodejs.NodeJSEnv$Config")
    val config = configClass.getConstructor().newInstance().asInstanceOf[AnyRef]
    val withExecutable = configClass.getMethod("withExecutable", classOf[String]).invoke(config, nodeBinary).asInstanceOf[AnyRef]
    val withEnv = configClass
      .getMethod("withEnv", loader.loadClass("scala.collection.immutable.Map"))
      .invoke(withExecutable, toScalaMap(env, loader).asInstanceOf[AnyRef])
      .asInstanceOf[AnyRef]
    loader.loadClass("org.scalajs.jsenv.nodejs.NodeJSEnv").getConstructor(configClass).newInstance(withEnv)
  }

  /** The adapter logs its own progress and its own failures through this logger. Node's stdout and stderr reach the handler through `SbtTestDriver` instead. */
  private def createScalaJsLogger(loader: ClassLoader): Any = {
    val levelClass = loader.loadClass("org.scalajs.logging.Level")
    val infoLevel = loader.loadClass("org.scalajs.logging.Level$Info$").getField("MODULE$").get(null)
    loader.loadClass("org.scalajs.logging.ScalaConsoleLogger").getDeclaredConstructor(levelClass).newInstance(infoLevel)
  }

  /** The linked module declares no framework the adapter could load.
    *
    * @param frameworkName
    *   the framework bleep asked for
    * @param classNames
    *   the `sbt.testing.Framework` class names the adapter tried
    * @param linkedJs
    *   the linked module the adapter loaded
    */
  case class NoScalaJsTestFrameworkException(frameworkName: String, classNames: List[String], linkedJs: Path)
      extends RuntimeException(s"No $frameworkName test framework in $linkedJs. Tried ${classNames.mkString(", ")}.")

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
      nodeBinary: String,
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
              java.util.Arrays.asList(nodeBinary, scriptPath.toAbsolutePath.toString)
            case NodeEnvironment.JSDOM(url) =>
              java.util.Arrays.asList(nodeBinary, "--experimental-vm-modules", scriptPath.toAbsolutePath.toString, "--jsdom", url)
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
      @annotation.unused nodeEnv: NodeEnvironment,
      nodeBinary: String,
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
              java.util.Arrays.asList(nodeBinary, "--experimental-vm-modules", scriptPath.toAbsolutePath.toString)
            case _ =>
              java.util.Arrays.asList(nodeBinary, scriptPath.toAbsolutePath.toString)
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
      @annotation.unused moduleKind: ScalaJsLinkConfig.ModuleKind,
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
