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

          // Stopping node is what ends a suite that spins. Such a suite never returns to node's event loop, and never notices the adapter closing its
          // socket to node. The thread waiting for that suite's reply does not return either.
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

    /** Closes the adapter's socket for every runner the adapter keeps. */
    def close(): Unit =
      JsTestAdapter.adapterClass(loader).getMethod("close").invoke(underlying): Unit

    /** Stops every node process this adapter started. Safe to call more than once. */
    def stopNodeRuns(): Unit = jsEnv.stopStartedRuns()
  }

  /** A `org.scalajs.jsenv.JSEnv` that keeps every `org.scalajs.jsenv.JSRun` it starts.
    *
    * `TestAdapter.close` closes the socket to each runner. That close leaves a node process spinning in a synchronous loop because such a
    * process never returns to its event loop to notice the socket closing. `ExternalJSRun.close` reaches the process instead via `destroyForcibly`.
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
            catch { case invoked: java.lang.reflect.InvocationTargetException => throw invoked.getCause }
          if (method.getName == "start" || method.getName == "startWithCom") startedRuns.add(returned): Unit
          returned
        }
      )

    /** Stops every run this env started. `JSRun.close` is safe to re-run.
      *
      * The process-level run closes first. Closing that run kills node. The socket read that blocks `ComRun`'s receiver thread then fails, which releases the
      * monitor `ComRun.close` needs.
      */
    def stopStartedRuns(): Unit = {
      val closeMethod = loader.loadClass("org.scalajs.jsenv.JSRun").getMethod("close")
      startedRuns.forEach { started =>
        closeMethod.invoke(processRunOf(started)): Unit
        closeMethod.invoke(started): Unit
      }
    }

    /** The run that owns the node process.
      *
      * `startWithCom` returns a `org.scalajs.jsenv.nodejs.ComRun`. Closing a `ComRun` closes the socket a `ComRun` uses to speak to node.
      */
    private def processRunOf(started: AnyRef): AnyRef =
      if (started.getClass.getName == RecordingJsEnv.ComRunClassName)
        started.getClass.getField(RecordingJsEnv.ComRunRunField).get(started)
      else started
  }

  private object RecordingJsEnv {

    val ComRunClassName = "org.scalajs.jsenv.nodejs.ComRun"

    /** The mangled name of `ComRun`'s `run` field. The field is public.
      */
    val ComRunRunField = "org$scalajs$jsenv$nodejs$ComRun$$run"
  }

  private object JsTestAdapter {

    /** A resource for a started adapter.
      *
      * Releasing the resource stops every node process the adapter started, then closes the adapter.
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
