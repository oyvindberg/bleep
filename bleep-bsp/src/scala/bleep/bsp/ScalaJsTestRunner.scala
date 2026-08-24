package bleep.bsp

import bleep.analysis.{CompilerResolver, ScalaJsLinkConfig}
import bleep.bsp.protocol.{KillReason, OutputChannel}
import bleep.bsp.TestRunnerTypes.{RunnerEvent, TerminationReason, TestEventHandler, TestResult, TestSuite}
import cats.effect.{Deferred, IO}
import java.nio.file.Path

/** Runs Scala.js test suites in linked output.
  *
  * Execution goes through `org.scalajs.testing.adapter.TestAdapter`, the same JVM-side component sbt and mill use. The adapter speaks to
  * `org.scalajs.testing.bridge.Bridge` in the linked program and hands back ordinary `sbt.testing.Framework` instances, so from there on Scala.js tests run
  * through exactly the code that runs JVM and Scala Native tests — see [[SbtTestingBridge]].
  *
  * What this replaced was a JavaScript harness bleep injected into the linked output: it deleted the linker's `Bridge.start()` call with a regular expression,
  * re-ran the program in a `vm` sandbox, and then reached for Scala.js internal symbols under their mangled JavaScript names. Which mangled names a linked
  * program declares depends on which code that program happens to reach, so the harness worked only for the frameworks whose names it had been written against,
  * and only while they kept reaching the same code:
  *
  *   - it looked up `$m_Lorg_portablescala_reflect_Reflect$`, which a munit-only program never declares (munit reaches `scala.scalajs.reflect.Reflect`
  *     instead), so every munit suite failed to load;
  *   - it loaded suites only as modules, while munit's fingerprint declares its suites to be classes;
  *   - it drove `utest.TestRunner` specifically, with hand-built function objects, and had no path for any other framework;
  *   - and when it could not find what it wanted it reported the suite as passed, so a suite that never ran looked green.
  *
  * The adapter has none of these couplings: it works for any sbt-testing framework, and the entry point it uses is the one the linker already emits for test
  * projects (`ScalaJs1Bridge.createModuleInitializers`). Reported as issue #655.
  */
object ScalaJsTestRunner {

  /** Node.js environment configuration. */
  sealed trait NodeEnvironment
  object NodeEnvironment {
    case object Node extends NodeEnvironment
    case class JSDOM(url: String) extends NodeEnvironment
  }

  /** Run `suites` from the linked JavaScript at `linkedJs`.
    *
    * @param linkedJs
    *   the linked test program, whose module initializer is `Bridge.start` (the linker emits this for test projects)
    * @param moduleKind
    *   how the linker emitted the program; decides which `Input` the JSEnv is given
    * @param scalaJsVersion
    *   the project's Scala.js version. The adapter and the linked program's bridge speak a version-coupled protocol, so the adapter is resolved to match.
    */
  def runTests(
      linkedJs: Path,
      moduleKind: ScalaJsLinkConfig.ModuleKind,
      suites: List[TestSuite],
      eventHandler: TestEventHandler,
      nodeEnv: NodeEnvironment,
      nodeBinary: String,
      env: Map[String, String],
      scalaJsVersion: String,
      classpath: List[Path],
      killSignal: Deferred[IO, KillReason]
  ): IO[TestResult] =
    killSignal.tryGet.flatMap {
      case Some(reason) => IO.pure(TestResult(0, 0, 0, 0, TerminationReason.Killed(reason)))
      case None         =>
        val work = IO.interruptible(runBlocking(linkedJs, moduleKind, suites, eventHandler, nodeEnv, nodeBinary, env, scalaJsVersion, classpath))
        Outcome
          .raceKill(killSignal)(work)
          .map {
            case Left(result)  => result
            case Right(reason) => TestResult(0, 0, 0, 0, TerminationReason.Killed(reason))
          }
    }

  private def runBlocking(
      linkedJs: Path,
      moduleKind: ScalaJsLinkConfig.ModuleKind,
      suites: List[TestSuite],
      eventHandler: TestEventHandler,
      nodeEnv: NodeEnvironment,
      nodeBinary: String,
      env: Map[String, String],
      scalaJsVersion: String,
      classpath: List[Path]
  ): TestResult = {
    val loader = CompilerResolver.getScalaJsTestAdapter(scalaJsVersion).loader
    val suiteTag = suites.headOption.map(_.fullyQualifiedName).getOrElse("")
    val adapter = newTestAdapter(loader, linkedJs, moduleKind, nodeEnv, nodeBinary, env, eventHandler, suiteTag, scalaJsVersion)

    val outcome =
      try {
        eventHandler.onRunnerEvent(RunnerEvent.Started)

        val loadMethod = adapter.getClass.getMethod("loadFrameworks", loader.loadClass("scala.collection.immutable.List"))
        val requested = SbtTestingBridge.ScalaColl.toList(
          SbtTestingBridge.knownFrameworkClassNames.map(name => SbtTestingBridge.ScalaColl.toList(List(name), loader)),
          loader
        )
        val loaded = SbtTestingBridge.ScalaColl
          .fromList[Any](loadMethod.invoke(adapter, requested), loader)
          .flatMap(opt => SbtTestingBridge.ScalaColl.fromOption[sbt.testing.Framework](opt, loader))

        loaded.headOption match {
          case None =>
            // No fallback: the suites were discovered on the classpath, so a linked program containing none of the frameworks that can run them is a real defect
            // and has to be reported as one.
            val names = suites.map(_.fullyQualifiedName).mkString(", ")
            val message =
              s"No sbt-testing framework found in the linked Scala.js output for $names. Tried: ${SbtTestingBridge.knownFrameworkClassNames.mkString(", ")}"
            eventHandler.onRunnerEvent(RunnerEvent.Error(message, None))
            TestResult(0, 0, 0, 0, TerminationReason.Error(message))

          case Some(framework) =>
            val result = SbtTestingBridge.runSuites(framework, suites, eventHandler, loader, SbtTestingBridge.moduleDetector(classpath))
            eventHandler.onRunnerEvent(RunnerEvent.ProcessExited(0))
            result
        }
      } catch {
        case fromBody: Throwable =>
          // Closing still has to happen, but must not become the reported failure. A plain `finally` replaces the body's exception with the cleanup's, which is
          // how a fault in the logger ended up masquerading as the reason a suite failed — the actual error was two levels down and never printed.
          try closeAdapter(adapter)
          catch { case fromClose: Throwable => fromBody.addSuppressed(fromClose) }
          throw fromBody
      }

    closeAdapter(adapter)
    outcome
  }

  private def closeAdapter(adapter: AnyRef): Unit =
    adapter.getClass.getMethod("close").invoke(adapter): Unit

  /** Build `TestAdapter(jsEnv, input, config)` inside the adapter's own classloader. */
  private def newTestAdapter(
      loader: ClassLoader,
      linkedJs: Path,
      moduleKind: ScalaJsLinkConfig.ModuleKind,
      nodeEnv: NodeEnvironment,
      nodeBinary: String,
      env: Map[String, String],
      eventHandler: TestEventHandler,
      suiteTag: String,
      scalaJsVersion: String
  ): AnyRef = {
    val jsEnv = newNodeJsEnv(loader, nodeEnv, nodeBinary, env)

    // The input type has to match how the linker emitted the program: a NoModule build is a plain script, and loading it as a module (or the reverse) fails
    // before any test runs.
    val inputClassName = moduleKind match {
      case ScalaJsLinkConfig.ModuleKind.NoModule       => "org.scalajs.jsenv.Input$Script"
      case ScalaJsLinkConfig.ModuleKind.CommonJSModule => "org.scalajs.jsenv.Input$CommonJSModule"
      case ScalaJsLinkConfig.ModuleKind.ESModule       => "org.scalajs.jsenv.Input$ESModule"
    }
    val input = loader.loadClass(inputClassName).getConstructor(classOf[Path]).newInstance(linkedJs.toAbsolutePath).asInstanceOf[AnyRef]
    val inputSeq = SbtTestingBridge.ScalaColl.toList(List(input), loader)

    val configClass = loader.loadClass("org.scalajs.testing.adapter.TestAdapter$Config")
    var config = configClass.getConstructor().newInstance().asInstanceOf[AnyRef]
    // Environment variables are set on the JSEnv, which is what actually spawns node. Passing them here as well would be redundant, and the adapter's own
    // `withEnv` covers only the com-channel process.
    config = configClass
      .getMethod("withLogger", loader.loadClass("org.scalajs.logging.Logger"))
      .invoke(config, adapterLogger(loader, eventHandler, suiteTag))
      .asInstanceOf[AnyRef]

    val adapterClass = loader.loadClass("org.scalajs.testing.adapter.TestAdapter")
    // Located by arity rather than by exact parameter types. `TestAdapter` declares a single three-argument constructor, but the erased type of its middle
    // parameter follows whichever Scala the artifact was built with: the 2.12 build takes `scala.collection.Seq`, the 2.13 build `scala.collection.immutable.Seq`
    // (2.13 rebound `Seq` to the immutable one). Naming either directly resolves against exactly one of the two artifacts and throws NoSuchMethodException for
    // the other.
    val ctor = adapterClass.getConstructors
      .find(_.getParameterCount == 3)
      .getOrElse(throw new RuntimeException(s"org.scalajs.testing.adapter.TestAdapter has no 3-argument constructor in Scala.js $scalaJsVersion"))
    ctor.newInstance(jsEnv, inputSeq, config).asInstanceOf[AnyRef]
  }

  private def newNodeJsEnv(loader: ClassLoader, nodeEnv: NodeEnvironment, nodeBinary: String, env: Map[String, String]): AnyRef = {
    val configClass = loader.loadClass("org.scalajs.jsenv.nodejs.NodeJSEnv$Config")
    var config = configClass.getConstructor().newInstance().asInstanceOf[AnyRef]
    config = configClass.getMethod("withExecutable", classOf[String]).invoke(config, nodeBinary).asInstanceOf[AnyRef]
    config = configClass
      .getMethod("withEnv", loader.loadClass("scala.collection.immutable.Map"))
      .invoke(config, SbtTestingBridge.ScalaColl.toMap(env, loader))
      .asInstanceOf[AnyRef]

    nodeEnv match {
      case NodeEnvironment.Node => ()
      // Running a DOM suite in a plain Node environment does not fail in any way the user can read: every DOM reference throws "document is not defined", far
      // from the setting that asked for a browser-like environment. A real jsdom run needs `JSDOMNodeJSEnv` from `org.scala-js:scalajs-env-jsdom-nodejs`,
      // which in turn needs the `jsdom` npm package on disk — and bleep has no npm handling at all. Until it does, say so rather than run the wrong thing.
      case NodeEnvironment.JSDOM(url) =>
        throw new UnsupportedOperationException(
          s"Scala.js tests requested a jsdom environment ($url), which bleep cannot provide yet: it needs org.scala-js:scalajs-env-jsdom-nodejs and the jsdom " +
            "npm package. Run these tests without jsdom, or track the request to add npm dependency support."
        )
    }

    loader.loadClass("org.scalajs.jsenv.nodejs.NodeJSEnv").getConstructor(configClass).newInstance(config).asInstanceOf[AnyRef]
  }

  /** A `org.scalajs.logging.Logger` that forwards the adapter's diagnostics into bleep's test output.
    *
    * Everything the adapter has to say about starting node, loading frameworks and the state of the com channel arrives through this. Discarding it (the
    * obvious `NullLogger`) means that when a Scala.js run goes wrong the user is told only that nothing ran, which is the experience issue #655 describes.
    *
    * `Logger` lives in the adapter's own classloader, so it cannot be implemented by a class compiled here; a dynamic proxy is the way to supply one. The
    * interface declares just `log` and `trace` as abstract, with `error`/`warn`/`info`/`debug` as defaults on top of `log` — but a proxy receives the default
    * methods too, so each is handled explicitly rather than left to a default implementation that will never run.
    */
  private def adapterLogger(loader: ClassLoader, eventHandler: TestEventHandler, suite: String): AnyRef = {
    val loggerClass = loader.loadClass("org.scalajs.logging.Logger")

    // Resolved on the `Function0` interface rather than on the thunk's own class. Scala compiles a by-name argument to a lambda, whose implementation class is
    // synthetic and not public, so `thunk.getClass.getMethod("apply")` yields a public method on an inaccessible class and invoking it throws
    // IllegalAccessException — which the proxy then rewrapped as UndeclaredThrowableException, and the `finally` below replaced with its own failure. The net
    // effect was that any framework whose loading made the adapter log at all died with an error naming a lambda. Interfaces are public, so this just works.
    val applyMethod = loader.loadClass("scala.Function0").getMethod("apply")
    def force(thunk: AnyRef): AnyRef = applyMethod.invoke(thunk)
    def emit(message: String, channel: OutputChannel): Unit = eventHandler.onOutput(suite, s"[scala.js] $message", channel)

    val handler = new java.lang.reflect.InvocationHandler {
      def invoke(proxy: Any, method: java.lang.reflect.Method, args: Array[AnyRef]): AnyRef =
        method.getName match {
          case "log" =>
            // Level is a sealed object hierarchy; its `toString` is the level name, which is all that is needed to pick a channel.
            val channel = if (args(0).toString.equalsIgnoreCase("Error")) OutputChannel.Stderr else OutputChannel.Stdout
            emit(String.valueOf(force(args(1))), channel)
            null
          case "error"         => emit(String.valueOf(force(args(0))), OutputChannel.Stderr); null
          case "warn" | "info" => emit(String.valueOf(force(args(0))), OutputChannel.Stdout); null
          case "debug"         => null
          case "trace"         => emit(String.valueOf(force(args(0))), OutputChannel.Stderr); null
          case "toString"      => "bleep-scalajs-adapter-logger"
          case "hashCode"      => Integer.valueOf(System.identityHashCode(proxy))
          case "equals"        => java.lang.Boolean.valueOf(proxy.asInstanceOf[AnyRef] eq args(0))
          case other           =>
            throw new UnsupportedOperationException(
              s"org.scalajs.logging.Logger.$other is not implemented by bleep's adapter logger; the interface gained a method this proxy does not handle"
            )
        }
    }

    java.lang.reflect.Proxy.newProxyInstance(loader, Array(loggerClass), handler)
  }

}
