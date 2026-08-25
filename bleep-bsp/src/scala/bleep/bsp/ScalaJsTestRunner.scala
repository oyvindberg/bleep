package bleep.bsp

import bleep.analysis.{CompilerResolver, ScalaJsLinkConfig}
import bleep.bsp.TestRunnerTypes.{frameworkClassNames, TerminationReason, TestEventHandler, TestFramework, TestResult, TestSuite}
import bleep.bsp.protocol.KillReason
import cats.effect.{Deferred, IO, Resource}

import java.nio.file.Path

/** Runs a linked Scala.js test module under Node.
  *
  * `org.scalajs.testing.adapter.TestAdapter` starts the node process. The adapter speaks the sbt-testing protocol to the `org.scalajs.testing.bridge.Bridge`
  * the linker put in the module. The adapter and the bridge both live outside bleep's classloader. Every call into the adapter goes through reflection.
  */
object ScalaJsTestRunner {

  /** Runs a linked Scala.js test module through `org.scalajs.testing.adapter.TestAdapter`.
    *
    * The adapter starts Node. The adapter then hands back an `sbt.testing.Framework`.
    *
    * @param linkedJs
    *   the linked main module
    * @param moduleKind
    *   the module kind the link config used. The adapter needs the matching `Input` case to load the file.
    * @param suites
    *   the suites to run. An empty list asks the framework to discover its own suites.
    * @param framework
    *   the framework whose class names the adapter tries
    * @param eventHandler
    *   bleep reports test progress through this handler
    * @param nodeBinary
    *   the node executable
    * @param env
    *   environment variables for the node process
    * @param scalaJsVersion
    *   the Scala.js version. The adapter artifact tracks that version.
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

        JsTestAdapter.open(loader, linkedJs, moduleKind, nodeBinary, env).use { adapter =>
          val runSuite = IO.interruptible {
            val sbtFramework = pickFramework(adapter, framework, linkedJs)
            SbtTestDriver.runFramework(sbtFramework, suites, eventHandler, loader)
          }

          // Stopping Node is what ends a suite that spins. Such a suite never reaches Node's event loop and never sees the adapter close its RPC channel.
          // The thread waiting for that suite's reply does not return either, which leaves fiber cancellation nothing to unwind. This branch produces its
          // reason only after the process is gone. The race therefore resolves with Node already stopped.
          val stopNodeThenReport = killSignal.get.flatTap(_ => IO.blocking(adapter.stopNodeRuns()))

          IO.race(runSuite, stopNodeThenReport).map {
            case Left(result)  => result
            case Right(reason) => TestResult(0, 0, 0, 0, TerminationReason.Killed(reason))
          }
        }
    }

  /** The framework the linked module declares.
    *
    * @throws NoScalaJsTestFrameworkException
    *   when the linked module declares none of the framework's class names
    */
  private def pickFramework(adapter: JsTestAdapter, framework: TestFramework, linkedJs: Path): sbt.testing.Framework = {
    val classNames = frameworkClassNames(framework)
    adapter
      .loadFrameworks(List(classNames))
      .flatten
      .headOption
      .getOrElse(throw NoScalaJsTestFrameworkException(framework.name, classNames, linkedJs))
  }

  /** The `org.scalajs.testing.adapter.TestAdapter` that `loader` built.
    */
  private case class JsTestAdapter(underlying: AnyRef, loader: ClassLoader, jsEnv: RecordingJsEnv) {

    /** Asks the adapter which of these class names the linked module declares.
      *
      * @param classNames
      *   one list per framework. Each list gives the alternative `sbt.testing.Framework` class names for one framework.
      * @return
      *   one entry per framework, in the order the frameworks arrived. An entry is the framework the linked module declares.
      */
    def loadFrameworks(classNames: List[List[String]]): List[Option[sbt.testing.Framework]] = {
      val requested = AlienList.of(classNames.map(names => AlienList.of(names, loader).underlying), loader)
      val loaded = JsTestAdapter
        .adapterClass(loader)
        .getMethod("loadFrameworks", loader.loadClass("scala.collection.immutable.List"))
        .invoke(underlying, requested.underlying)
      AlienList(loaded, loader).elements.map(element => AlienOption(element, loader).as[sbt.testing.Framework])
    }

    /** Closes the adapter's RPC channel for every runner it knows about. */
    def close(): Unit =
      JsTestAdapter.adapterClass(loader).getMethod("close").invoke(underlying): Unit

    /** Stops every Node process this adapter started. Safe to call more than once. */
    def stopNodeRuns(): Unit = jsEnv.stopStartedRuns()
  }

  /** A `org.scalajs.jsenv.JSEnv` that keeps every `org.scalajs.jsenv.JSRun` it starts.
    *
    * `TestAdapter.close` closes the RPC channel to each runner it knows about. It leaves a Node process that is spinning in a synchronous loop, because that
    * process never reaches its event loop and never sees the channel close. `JSRun.close` reaches the process instead. `ExternalJSRun.close` calls
    * `destroyForcibly` on it.
    *
    * Recording the runs keeps this containment exact. Each adapter stops the processes it started and no others, which matters because suites of one project
    * run their adapters at the same time.
    *
    * @param realEnv
    *   the `NodeJSEnv` this env delegates every call to
    * @param loader
    *   the classloader that owns `realEnv` and declares `JSEnv`
    */
  private class RecordingJsEnv(realEnv: AnyRef, loader: ClassLoader) {

    private val startedRuns = new java.util.concurrent.ConcurrentLinkedQueue[AnyRef]()

    /** The `JSEnv` to hand the adapter. Its `start` and `startWithCom` record what they return. */
    val asJsEnv: AnyRef =
      java.lang.reflect.Proxy.newProxyInstance(
        loader,
        Array(loader.loadClass("org.scalajs.jsenv.JSEnv")),
        (_, method, args) => {
          val returned =
            try method.invoke(realEnv, (if (args == null) Array.empty[AnyRef] else args)*)
            // The adapter reads the exception a `JSEnv` throws. A reflective wrapper around the cause would hide it.
            catch { case invoked: java.lang.reflect.InvocationTargetException => throw invoked.getCause }
          if (method.getName == "start" || method.getName == "startWithCom") startedRuns.add(returned): Unit
          returned
        }
      )

    /** Stops every run this env started. `JSRun.close` is documented idempotent, so a run the adapter already closed takes no harm.
      *
      * The process-level run closes first. That kills Node, which fails the socket read that `ComRun`'s receiver thread is blocked on. `ComRun.close` is
      * `synchronized` against that receiver thread.
      */
    def stopStartedRuns(): Unit = {
      val closeMethod = loader.loadClass("org.scalajs.jsenv.JSRun").getMethod("close")
      startedRuns.forEach { started =>
        closeMethod.invoke(processRunOf(started)): Unit
        closeMethod.invoke(started): Unit
      }
    }

    /** The run that owns the OS process.
      *
      * `startWithCom` returns a `org.scalajs.jsenv.nodejs.ComRun`. Closing a `ComRun` closes the socket it speaks to Node over. A suite spinning in a
      * synchronous loop never reaches Node's event loop to notice that close. The run a `ComRun` keeps reaches the process instead, because
      * `ExternalJSRun.close` calls `destroyForcibly`. `start` returns that process-level run with nothing wrapped around it.
      */
    private def processRunOf(started: AnyRef): AnyRef =
      if (started.getClass.getName == RecordingJsEnv.ComRunClassName)
        started.getClass.getField(RecordingJsEnv.ComRunRunField).get(started)
      else started
  }

  private object RecordingJsEnv {

    val ComRunClassName = "org.scalajs.jsenv.nodejs.ComRun"

    /** The mangled name of `ComRun`'s `run` field. The field is public, so this read needs no `setAccessible` and no module opening. A Scala.js release that
      * renames it fails this read loudly rather than leaking a Node process.
      */
    val ComRunRunField = "org$scalajs$jsenv$nodejs$ComRun$$run"
  }

  private object JsTestAdapter {

    /** A resource for a started adapter.
      *
      * Releasing the resource stops every Node process the adapter started, then closes the adapter. A `close()` that throws fails the run rather than passing
      * quietly. The Node processes are already stopped by then.
      */
    def open(
        loader: ClassLoader,
        linkedJs: Path,
        moduleKind: ScalaJsLinkConfig.ModuleKind,
        nodeBinary: String,
        env: Map[String, String]
    ): Resource[IO, JsTestAdapter] = {
      val acquire = IO.blocking {
        val configClass = loader.loadClass("org.scalajs.testing.adapter.TestAdapter$Config")
        val config = configClass
          .getMethod("withLogger", loader.loadClass("org.scalajs.logging.Logger"))
          .invoke(configClass.getConstructor().newInstance().asInstanceOf[AnyRef], consoleLogger(loader))
          .asInstanceOf[AnyRef]

        val recordingEnv = new RecordingJsEnv(nodeJsEnv(loader, nodeBinary, env), loader)

        val adapter = adapterClass(loader)
          .getConstructor(loader.loadClass("org.scalajs.jsenv.JSEnv"), loader.loadClass("scala.collection.immutable.Seq"), configClass)
          .newInstance(recordingEnv.asJsEnv, AlienList.of(List(input(loader, linkedJs, moduleKind)), loader).underlying, config)
          .asInstanceOf[AnyRef]

        JsTestAdapter(adapter, loader, recordingEnv)
      }

      // Stopping the runs comes first. It kills the Node process, which fails the RPC read that a suite thread is blocked on. `TestAdapter.close` takes the
      // adapter's own monitor. A suite thread that still held that monitor would block this release forever.
      Resource.make(acquire) { adapter =>
        IO.blocking {
          adapter.stopNodeRuns()
          adapter.close()
        }
      }
    }

    private def adapterClass(loader: ClassLoader): Class[?] =
      loader.loadClass("org.scalajs.testing.adapter.TestAdapter")

    /** Builds the `org.scalajs.jsenv.Input` case that matches the module kind the link used. */
    private def input(loader: ClassLoader, linkedJs: Path, moduleKind: ScalaJsLinkConfig.ModuleKind): AnyRef = {
      val inputClassName = moduleKind match {
        case ScalaJsLinkConfig.ModuleKind.NoModule       => "org.scalajs.jsenv.Input$Script"
        case ScalaJsLinkConfig.ModuleKind.CommonJSModule => "org.scalajs.jsenv.Input$CommonJSModule"
        case ScalaJsLinkConfig.ModuleKind.ESModule       => "org.scalajs.jsenv.Input$ESModule"
      }
      loader.loadClass(inputClassName).getConstructor(classOf[Path]).newInstance(linkedJs.toAbsolutePath)
    }

    /** Builds the `org.scalajs.jsenv.nodejs.NodeJSEnv` that starts `nodeBinary` with `env` set. */
    private def nodeJsEnv(loader: ClassLoader, nodeBinary: String, env: Map[String, String]): AnyRef = {
      val configClass = loader.loadClass("org.scalajs.jsenv.nodejs.NodeJSEnv$Config")
      val config = configClass.getConstructor().newInstance().asInstanceOf[AnyRef]
      val withExecutable = configClass.getMethod("withExecutable", classOf[String]).invoke(config, nodeBinary).asInstanceOf[AnyRef]
      val withEnv = configClass
        .getMethod("withEnv", loader.loadClass("scala.collection.immutable.Map"))
        .invoke(withExecutable, AlienMap.of(env, loader).underlying)
        .asInstanceOf[AnyRef]
      loader.loadClass("org.scalajs.jsenv.nodejs.NodeJSEnv").getConstructor(configClass).newInstance(withEnv)
    }

    /** The adapter logs its own progress and its own failures through this logger. */
    private def consoleLogger(loader: ClassLoader): AnyRef = {
      val levelClass = loader.loadClass("org.scalajs.logging.Level")
      val infoLevel = loader.loadClass("org.scalajs.logging.Level$Info$").getField("MODULE$").get(null)
      loader.loadClass("org.scalajs.logging.ScalaConsoleLogger").getDeclaredConstructor(levelClass).newInstance(infoLevel)
    }
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
}
