package bleep.bsp

import bleep.analysis.{CompilerResolver, ScalaJsLinkConfig}
import bleep.bsp.ScalaCollectionReflection.{fromScalaList, fromScalaOption, toScalaList, toScalaMap}
import bleep.bsp.TestRunnerTypes.*
import bleep.bsp.protocol.KillReason
import cats.effect.{Deferred, IO, Resource}

import java.nio.file.Path

/** Runs a linked Scala.js test module under Node.
  *
  * `org.scalajs.testing.adapter.TestAdapter` starts the node process and speaks the sbt-testing protocol to the `org.scalajs.testing.bridge.Bridge` the linker
  * put in the module. The adapter and the bridge both live outside bleep's classloader, which is why every call into them goes through reflection.
  */
object ScalaJsTestRunner {

  /** Run a linked Scala.js test module through `org.scalajs.testing.adapter.TestAdapter`.
    *
    * The adapter starts Node and hands back an `sbt.testing.Framework`.
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

  /** Build the `org.scalajs.jsenv.Input` case that matches the module kind the link used.
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

  /** The adapter logs its own progress and its own failures through this logger. */
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
}
