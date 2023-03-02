package bleep
package bsp

import bleep.internal.{DoSourceGen, Throwables, TransitiveProjects}
import bleep.logging.Logger
import ch.epfl.scala.bsp4j
import com.google.gson.{JsonObject, JsonPrimitive}
import org.eclipse.lsp4j.jsonrpc.ResponseErrorException
import org.eclipse.lsp4j.jsonrpc.messages.{ResponseError, ResponseErrorCode}

import java.util
import java.util.concurrent.CompletableFuture
import java.util.function.BiFunction
import scala.build.bloop.BuildServer
import scala.build.blooprifle.internal.Constants
import scala.concurrent.{Future, Promise}
import scala.jdk.CollectionConverters._

class BleepBspServer(
    val logger: Logger,
    var sendToIdeClient: bsp4j.BuildClient,
    var bloopServer: BuildServer,
    var buildChangeTracker: BuildChangeTracker
) extends BuildServer {
  val supportedLanguages: util.List[String] = List("scala", "java").asJava

  var isMetals = true
  var initialized = false

  def fail(msg: String, th: Throwable): Nothing = {
    logger.debug(msg, th)
    logger.warn(s"$msg: ${Throwables.messagesFrom(th).mkString(": ")}")
    if (isMetals) {
      sys.exit(1)
    }
    throw new ResponseErrorException(
      new ResponseError(ResponseErrorCode.UnknownErrorCode, s"$msg: ${Throwables.messagesFrom(th).mkString(": ")}", null)
    )
  }

  def handleBloopFailure[T](methodName: String, params: Any*): BiFunction[T, Throwable, T] =
    (maybeValue: T, maybeException: Throwable) =>
      maybeException match {
        case null =>
          maybeValue
        case error =>
          Throwables.tryExtract(classOf[java.net.SocketException])(error) match {
            case Some(socketException) =>
              buildShutdown() // don't wait for this
              fail("Lost contact with bloop server. initializing shutdown", socketException)

            case None =>
              val methodContext = s"Got error from bloop while running: $methodName: ${error.getClass.getName}"
              val context = if (params.isEmpty) methodContext else params.mkString(s"$methodContext, with params: ", ", ", "")
              fail(context, error)
          }
      }

  def enter(name: String, args: Any*): Unit = {
    logger.debug(s"$name(${args.mkString(", ")})")

    buildChangeTracker.current match {
      case Left(_) =>
        // try to reload and see if we can salvage the broken build situation
        buildChangeTracker.ensureBloopUpToDate() match {
          case Left(bleepException) => fail("Bleep is not able to load your build", bleepException)
          case Right(_)             => ()
        }

      case Right(started) =>
        if (initialized)
          DoSourceGen(started, bloopServer, TransitiveProjects.all(started.build)) match {
            case Left(bleepException) => fail("Bleep was not able to run source generators", bleepException)
            case Right(())            => ()
          }
    }
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
    enter("buildInitialize", params.toString)

    buildChangeTracker.ensureBloopUpToDate() match {
      case Left(bleepException) =>
        fail("couldn't load build", bleepException)
      case Right(started) =>
        val workspaceDir = started.buildPaths.buildVariantDir

        val displayName = params.getDisplayName // "Metals" or "IntelliJ-BSP"
        isMetals = displayName != "Metals"

        val initParams = new bsp4j.InitializeBuildParams(
          s"bleep / $displayName",
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
          .handle(handleBloopFailure("buildInitialize", initParams))
          .thenApply { _ =>
            bloopServer.onBuildInitialized()
            new bsp4j.InitializeBuildResult("bleep", model.BleepVersion.current.value, Constants.bspVersion, capabilities)
          }
    }
  }

  override def workspaceBuildTargets(): CompletableFuture[bsp4j.WorkspaceBuildTargetsResult] = {
    enter("workspaceBuildTargets")

    buildChangeTracker.ensureBloopUpToDate() match {
      case Left(bleepException) =>
        fail("couldn't refresh build", bleepException)
      case Right(_) =>
        bloopServer.workspaceBuildTargets().handle(handleBloopFailure("workspaceBuildTargets"))
    }
  }

  override def onBuildInitialized(): Unit = {
    initialized = true
    enter("onBuildInitialized")
    ()
  }

  override def workspaceReload(): CompletableFuture[Object] = {
    enter("workspaceReload")
    // Bloop does not support workspaceReload and Intellij calls it at the start
    CompletableFuture.completedFuture(new Object).handle(handleBloopFailure("workspaceReload"))
  }

  override def buildTargetCleanCache(params: bsp4j.CleanCacheParams): CompletableFuture[bsp4j.CleanCacheResult] = {
    enter("buildTargetCleanCache", params)
    bloopServer.buildTargetCleanCache(params).handle(handleBloopFailure("buildTargetCleanCache", params))
  }
  override def buildTargetCompile(params: bsp4j.CompileParams): CompletableFuture[bsp4j.CompileResult] = {
    enter("buildTargetCompile", params)
    bloopServer.buildTargetCompile(params).handle(handleBloopFailure("buildTargetCompile", params))
  }
  override def buildTargetDependencySources(params: bsp4j.DependencySourcesParams): CompletableFuture[bsp4j.DependencySourcesResult] = {
    enter("buildTargetDependencySources", params)
    bloopServer.buildTargetDependencySources(params).handle(handleBloopFailure("buildTargetDependencySources", params))
  }
  override def buildTargetInverseSources(params: bsp4j.InverseSourcesParams): CompletableFuture[bsp4j.InverseSourcesResult] = {
    enter("buildTargetInverseSources", params)
    bloopServer.buildTargetInverseSources(params).handle(handleBloopFailure("buildTargetInverseSources", params))
  }
  override def buildTargetResources(params: bsp4j.ResourcesParams): CompletableFuture[bsp4j.ResourcesResult] = {
    enter("buildTargetResources", params)
    bloopServer.buildTargetResources(params).handle(handleBloopFailure("buildTargetResources", params))
  }
  override def buildTargetRun(params: bsp4j.RunParams): CompletableFuture[bsp4j.RunResult] = {
    enter("buildTargetRun", params)
    bloopServer.buildTargetRun(params).handle(handleBloopFailure("buildTargetRun", params))
  }
  override def buildTargetSources(params: bsp4j.SourcesParams): CompletableFuture[bsp4j.SourcesResult] = {
    enter("buildTargetSources")
    bloopServer.buildTargetSources(params).handle(handleBloopFailure("buildTargetSources", params))
  }
  override def buildTargetTest(params: bsp4j.TestParams): CompletableFuture[bsp4j.TestResult] = {
    enter("buildTargetTest")
    bloopServer.buildTargetTest(params).handle(handleBloopFailure("buildTargetTest", params))
  }
  override def buildTargetDependencyModules(params: bsp4j.DependencyModulesParams): CompletableFuture[bsp4j.DependencyModulesResult] = {
    enter("buildTargetDependencyModules", params)
    bloopServer.buildTargetDependencyModules(params).handle(handleBloopFailure("buildTargetDependencyModules", params))
  }
  override def buildTargetJavacOptions(params: bsp4j.JavacOptionsParams): CompletableFuture[bsp4j.JavacOptionsResult] = {
    enter("buildTargetJavacOptions", params)
    bloopServer.buildTargetJavacOptions(params).handle(handleBloopFailure("buildTargetJavacOptions", params))
  }
  override def buildTargetScalaMainClasses(params: bsp4j.ScalaMainClassesParams): CompletableFuture[bsp4j.ScalaMainClassesResult] = {
    enter("buildTargetScalaMainClasses", params)
    bloopServer.buildTargetScalaMainClasses(params).handle(handleBloopFailure("buildTargetScalaMainClasses", params))
  }
  override def buildTargetScalaTestClasses(params: bsp4j.ScalaTestClassesParams): CompletableFuture[bsp4j.ScalaTestClassesResult] = {
    enter("buildTargetScalaTestClasses", params)
    bloopServer.buildTargetScalaTestClasses(params).handle(handleBloopFailure("buildTargetScalaTestClasses", params))
  }
  override def buildTargetScalacOptions(params: bsp4j.ScalacOptionsParams): CompletableFuture[bsp4j.ScalacOptionsResult] = {
    enter("buildTargetScalacOptions", params)
    bloopServer.buildTargetScalacOptions(params).handle(handleBloopFailure("buildTargetScalacOptions", params))
  }
  override def debugSessionStart(params: bsp4j.DebugSessionParams): CompletableFuture[bsp4j.DebugSessionAddress] = {
    enter("debugSessionStart", params)
    bloopServer.debugSessionStart(params).handle(handleBloopFailure("debugSessionStart", params))
  }
  override def buildTargetOutputPaths(params: bsp4j.OutputPathsParams): CompletableFuture[bsp4j.OutputPathsResult] = {
    enter("buildTargetOutputPaths", params)
    bloopServer.buildTargetOutputPaths(params).handle(handleBloopFailure("buildTargetOutputPaths", params))
  }
  override def jvmRunEnvironment(params: bsp4j.JvmRunEnvironmentParams): CompletableFuture[bsp4j.JvmRunEnvironmentResult] = {
    enter("jvmRunEnvironment", params)
    bloopServer.jvmRunEnvironment(params).handle(handleBloopFailure("jvmRunEnvironment", params))
  }
  override def jvmTestEnvironment(params: bsp4j.JvmTestEnvironmentParams): CompletableFuture[bsp4j.JvmTestEnvironmentResult] = {
    enter("jvmTestEnvironment", params)
    bloopServer.jvmTestEnvironment(params).handle(handleBloopFailure("jvmTestEnvironment", params))
  }

  private val shutdownPromise = Promise[Unit]()

  override def buildShutdown(): CompletableFuture[Object] = {
    enter("buildShutdown")
    if (!shutdownPromise.isCompleted)
      shutdownPromise.success(())
    bloopServer.buildShutdown()
  }

  override def onBuildExit(): Unit = {
    enter("onBuildExit")
    ()
  }

  def initiateShutdown: Future[Unit] =
    shutdownPromise.future
}
