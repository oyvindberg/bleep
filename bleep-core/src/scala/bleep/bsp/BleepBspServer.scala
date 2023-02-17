package bleep
package bsp

import bleep.BleepException
import bleep.internal.throwableMessages
import bleep.logging.Logger
import ch.epfl.scala.bsp4j
import com.google.gson.{JsonObject, JsonPrimitive}

import java.io.{PrintWriter, StringWriter}
import java.util
import java.util.concurrent.{CompletableFuture, TimeUnit}
import java.util.function.BiFunction
import scala.build.bloop.BuildServer
import scala.build.blooprifle.internal.Constants
import scala.concurrent.{Future, Promise}
import scala.jdk.CollectionConverters._
import scala.util.Random

class BleepBspServer(
    val logger: Logger,
    var sendToIdeClient: bsp4j.BuildClient,
    var bloopServer: BuildServer,
    var ensureBloopUpToDate: () => Either[BleepException, Started]
) extends BuildServer {
  val supportedLanguages: util.List[String] = List("scala", "java").asJava

  protected def onFatalError(throwable: Throwable, context: String): Nothing = {
    val sw = new StringWriter()
    throwable.printStackTrace(new PrintWriter(sw))
    val message = s"Fatal error has occurred within $context. Shutting down the server:\n ${sw.toString}"
    System.err.println(message)
    sendToIdeClient.onBuildLogMessage(new bsp4j.LogMessageParams(bsp4j.MessageType.ERROR, message))

    // wait random bit before shutting down server to reduce risk of multiple bleep instances starting bloop at the same time
    val timeout = Random.nextInt(400)
    TimeUnit.MILLISECONDS.sleep(100L + timeout)
    sys.exit(1)
  }

  def fatalExceptionHandler[T](methodName: String, params: Any*): BiFunction[T, Throwable, T] =
    (maybeValue: T, maybeException: Throwable) =>
      maybeException match {
        case null =>
          maybeValue
        case error =>
          val methodContext = s"bloop bsp server, method: $methodName"
          val context = if (params.isEmpty) methodContext else params.mkString(s"$methodContext, with params: ", ", ", "")
          onFatalError(error, context)
      }

  private def capabilities: bsp4j.BuildServerCapabilities = {
    val ret = new bsp4j.BuildServerCapabilities
    ret.setCompileProvider(new bsp4j.CompileProvider(supportedLanguages))
    ret.setTestProvider(new bsp4j.TestProvider(supportedLanguages))
    ret.setRunProvider(new bsp4j.RunProvider(supportedLanguages))
    ret.setDebugProvider(new bsp4j.DebugProvider(supportedLanguages))
    ret.setInverseSourcesProvider(true)
    ret.setDependencySourcesProvider(true)
    ret.setResourcesProvider(true)
    ret.setBuildTargetChangedProvider(true)
    ret.setJvmRunEnvironmentProvider(true)
    ret.setJvmTestEnvironmentProvider(true)
    ret.setCanReload(true)
    ret.setDependencyModulesProvider(true)
    ret
  }

  override def buildInitialize(params: bsp4j.InitializeBuildParams): CompletableFuture[bsp4j.InitializeBuildResult] = {
    logger.debug(("buildInitialize", params.toString))

    ensureBloopUpToDate() match {
      case Left(th) =>
        sendToIdeClient.onBuildShowMessage(new bsp4j.ShowMessageParams(bsp4j.MessageType.ERROR, throwableMessages(th).mkString(": ")))

        logger.error("couldn't refresh build", th)
        CompletableFuture.failedFuture(th)
      case Right(started) =>
        val workspaceDir = started.buildPaths.buildVariantDir

        val initParams = new bsp4j.InitializeBuildParams(
          s"bleep / ${params.getDisplayName}",
          s"${model.BleepVersion.current.value} / ${params.getVersion}",
          Constants.bspVersion,
          workspaceDir.toUri.toASCIIString,
          new bsp4j.BuildClientCapabilities(supportedLanguages)
        )

        initParams.setData {
          val data = new JsonObject
          data.add("clientClassesRootDir", new JsonPrimitive((workspaceDir / "classes").toUri.toASCIIString))
          data.add("ownsBuildFiles", new JsonPrimitive(true))
          params.getData match {
            // pick up these values to make metals work.
            case dataFromIDE: JsonObject =>
              data.add("semanticdbVersion", dataFromIDE.get("semanticdbVersion"))
              data.add("javaSemanticdbVersion", dataFromIDE.get("javaSemanticdbVersion"))
              data.add("supportedScalaVersions", dataFromIDE.get("supportedScalaVersions"))
            case unexpected =>
              logger.warn(s"got unexpected data element: $unexpected")
          }
          data
        }

        logger.debug("Sending buildInitialize BSP command to Bloop")
        bloopServer
          .buildInitialize(initParams)
          .handle(fatalExceptionHandler("buildInitialize", initParams))
          .thenApply { _ =>
            bloopServer.onBuildInitialized()
            new bsp4j.InitializeBuildResult("bleep", model.BleepVersion.current.value, Constants.bspVersion, capabilities)
          }
    }
  }

  override def workspaceBuildTargets(): CompletableFuture[bsp4j.WorkspaceBuildTargetsResult] = {
    logger.debug("workspaceBuildTargets")

    ensureBloopUpToDate() match {
      case Left(th) =>
        logger.error("couldn't refresh build", th)
        CompletableFuture.failedFuture(th)
      case Right(_) => bloopServer.workspaceBuildTargets().handle(fatalExceptionHandler("workspaceBuildTargets"))
    }
  }

  override def onBuildInitialized(): Unit = {
    logger.debug("onBuildInitialized")
    ()
  }

  override def workspaceReload(): CompletableFuture[Object] = {
    logger.debug("workspaceReload")
    // Bloop does not support workspaceReload and Intellij calls it at the start
    CompletableFuture.completedFuture(new Object).handle(fatalExceptionHandler("workspaceReload"))
  }

  override def buildTargetCleanCache(params: bsp4j.CleanCacheParams): CompletableFuture[bsp4j.CleanCacheResult] = {
    logger.debug(("buildTargetCleanCache", params.toString))
    bloopServer.buildTargetCleanCache(params).handle(fatalExceptionHandler("buildTargetCleanCache", params))
  }
  override def buildTargetCompile(params: bsp4j.CompileParams): CompletableFuture[bsp4j.CompileResult] = {
    logger.debug(("buildTargetCompile", params.toString))
    bloopServer.buildTargetCompile(params).handle(fatalExceptionHandler("buildTargetCompile", params))
  }
  override def buildTargetDependencySources(params: bsp4j.DependencySourcesParams): CompletableFuture[bsp4j.DependencySourcesResult] = {
    logger.debug(("buildTargetDependencySources", params.toString))
    bloopServer.buildTargetDependencySources(params).handle(fatalExceptionHandler("buildTargetDependencySources", params))
  }
  override def buildTargetInverseSources(params: bsp4j.InverseSourcesParams): CompletableFuture[bsp4j.InverseSourcesResult] = {
    logger.debug(("buildTargetInverseSources", params.toString))
    bloopServer.buildTargetInverseSources(params).handle(fatalExceptionHandler("buildTargetInverseSources", params))
  }
  override def buildTargetResources(params: bsp4j.ResourcesParams): CompletableFuture[bsp4j.ResourcesResult] = {
    logger.debug(("buildTargetResources", params.toString))
    bloopServer.buildTargetResources(params).handle(fatalExceptionHandler("buildTargetResources", params))
  }
  override def buildTargetRun(params: bsp4j.RunParams): CompletableFuture[bsp4j.RunResult] = {
    logger.debug(("buildTargetRun", params.toString))
    bloopServer.buildTargetRun(params).handle(fatalExceptionHandler("buildTargetRun", params))
  }
  override def buildTargetSources(params: bsp4j.SourcesParams): CompletableFuture[bsp4j.SourcesResult] = {
    logger.debug(("buildTargetSources", params.toString))
    bloopServer.buildTargetSources(params).handle(fatalExceptionHandler("buildTargetSources", params))
  }
  override def buildTargetTest(params: bsp4j.TestParams): CompletableFuture[bsp4j.TestResult] = {
    logger.debug(("buildTargetTest", params.toString))
    bloopServer.buildTargetTest(params).handle(fatalExceptionHandler("buildTargetTest", params))
  }
  override def buildTargetDependencyModules(params: bsp4j.DependencyModulesParams): CompletableFuture[bsp4j.DependencyModulesResult] = {
    logger.debug(("buildTargetDependencyModules", params.toString))
    bloopServer.buildTargetDependencyModules(params).handle(fatalExceptionHandler("buildTargetDependencyModules", params))
  }
  override def buildTargetJavacOptions(params: bsp4j.JavacOptionsParams): CompletableFuture[bsp4j.JavacOptionsResult] = {
    logger.debug(("buildTargetJavacOptions", params.toString))
    bloopServer.buildTargetJavacOptions(params).handle(fatalExceptionHandler("buildTargetJavacOptions", params))
  }
  override def buildTargetScalaMainClasses(params: bsp4j.ScalaMainClassesParams): CompletableFuture[bsp4j.ScalaMainClassesResult] = {
    logger.debug(("buildTargetScalaMainClasses", params.toString))
    bloopServer.buildTargetScalaMainClasses(params).handle(fatalExceptionHandler("buildTargetScalaMainClasses", params))
  }
  override def buildTargetScalaTestClasses(params: bsp4j.ScalaTestClassesParams): CompletableFuture[bsp4j.ScalaTestClassesResult] = {
    logger.debug(("buildTargetScalaTestClasses", params.toString))
    bloopServer.buildTargetScalaTestClasses(params).handle(fatalExceptionHandler("buildTargetScalaTestClasses", params))
  }
  override def buildTargetScalacOptions(params: bsp4j.ScalacOptionsParams): CompletableFuture[bsp4j.ScalacOptionsResult] = {
    logger.debug(("buildTargetScalacOptions", params.toString))
    bloopServer.buildTargetScalacOptions(params).handle(fatalExceptionHandler("buildTargetScalacOptions", params))
  }
  override def debugSessionStart(params: bsp4j.DebugSessionParams): CompletableFuture[bsp4j.DebugSessionAddress] = {
    logger.debug(("debugSessionStart", params.toString))
    bloopServer.debugSessionStart(params).handle(fatalExceptionHandler("debugSessionStart", params))
  }
  override def buildTargetOutputPaths(params: bsp4j.OutputPathsParams): CompletableFuture[bsp4j.OutputPathsResult] = {
    logger.debug(("buildTargetOutputPaths", params.toString))
    bloopServer.buildTargetOutputPaths(params).handle(fatalExceptionHandler("buildTargetOutputPaths", params))
  }
  override def jvmRunEnvironment(params: bsp4j.JvmRunEnvironmentParams): CompletableFuture[bsp4j.JvmRunEnvironmentResult] = {
    logger.debug(("jvmRunEnvironment", params.toString))
    bloopServer.jvmRunEnvironment(params).handle(fatalExceptionHandler("jvmRunEnvironment", params))
  }
  override def jvmTestEnvironment(params: bsp4j.JvmTestEnvironmentParams): CompletableFuture[bsp4j.JvmTestEnvironmentResult] = {
    logger.debug(("jvmTestEnvironment", params.toString))
    bloopServer.jvmTestEnvironment(params).handle(fatalExceptionHandler("jvmTestEnvironment", params))
  }

  private val shutdownPromise = Promise[Unit]()

  override def buildShutdown(): CompletableFuture[Object] = {
    logger.debug("buildShutdown")
    if (!shutdownPromise.isCompleted)
      shutdownPromise.success(())
    bloopServer.buildShutdown()
  }

  override def onBuildExit(): Unit = {
    logger.debug("onBuildExit")
    ()
  }

  def initiateShutdown: Future[Unit] =
    shutdownPromise.future
}
