package bleep.bsp

import cats.effect.{IO, Resource}
import ch.epfl.scala.bsp4j.BuildClient
import org.eclipse.lsp4j.jsonrpc.Launcher

import java.io.PrintWriter
import java.nio.file.{Files, Path}
import java.util.concurrent.{ExecutorService, Executors}

/** Creates typed BSP server proxy from a BspConnection.
  *
  * Uses LSP4J's Launcher to create a JSON-RPC client that implements the BuildServer interface, similar to how bloop-rifle does it.
  */
object BspServerBuilder {

  /** Create a typed BuildServer from a BspConnection.
    *
    * @param connection
    *   The raw socket connection to the BSP server
    * @param client
    *   The BuildClient implementation for receiving notifications
    * @param traceFile
    *   Optional path for BSP message tracing (logs all JSON-RPC messages)
    * @return
    *   Resource containing the BuildServer proxy and executor
    */
  def create(
      connection: BspConnection,
      client: BuildClient,
      traceFile: Option[Path] = None,
      onCleanup: Option[String => Unit] = None
  ): Resource[IO, BuildServerWithLifecycle] =
    Resource.make(
      IO.blocking {
        // Create executor for JSON-RPC message handling
        val executor: ExecutorService = Executors.newCachedThreadPool { r =>
          val t = new Thread(r, "bsp-client-worker")
          t.setDaemon(true)
          t
        }

        // Create trace writer if path provided
        val traceWriter: Option[PrintWriter] = traceFile.map { path =>
          Files.createDirectories(path.getParent)
          new PrintWriter(Files.newBufferedWriter(path), true) // autoFlush=true
        }

        // Create LSP4J launcher with optional tracing
        // Use our combined BuildServer trait that includes Scala/Java/JVM interfaces
        val launcherBuilder = new Launcher.Builder[BuildServer]()
          .setLocalService(client)
          .setRemoteInterface(classOf[BuildServer])
          .setInput(connection.input)
          .setOutput(connection.output)
          .setExecutorService(executor)

        // Add message tracing if enabled
        traceWriter.foreach(launcherBuilder.traceMessages)

        val launcher: Launcher[BuildServer] = launcherBuilder.create()

        // Start listening for messages in background
        val listening = launcher.startListening()

        // Get the remote proxy
        val server: BuildServer = launcher.getRemoteProxy

        BuildServerWithLifecycle(
          server = server,
          executor = executor,
          listening = listening,
          connection = connection,
          traceWriter = traceWriter
        )
      }
    ) { lifecycle =>
      IO.blocking {
        val log: String => Unit = onCleanup.getOrElse((_: String) => ())
        log("[CLEANUP] Starting BspServerBuilder cleanup")
        // Cancel listening (interrupts the reader thread)
        log("[CLEANUP] Calling listening.cancel(true)")
        lifecycle.listening.cancel(true)
        log("[CLEANUP] listening.cancel returned")
        // Shutdown executor — allows submitted tasks to complete
        log("[CLEANUP] Calling executor.shutdown()")
        lifecycle.executor.shutdown()
        // CRITICAL: Wait for executor tasks (notification handlers) to complete.
        // Without this, SuiteFinished handlers still in the executor queue may
        // not have called emit() before the poison pill is offered downstream.
        log("[CLEANUP] Calling executor.awaitTermination(5s)")
        val terminated = lifecycle.executor.awaitTermination(5, java.util.concurrent.TimeUnit.SECONDS)
        log(s"[CLEANUP] executor.awaitTermination returned: terminated=$terminated")
        // Close trace writer
        lifecycle.traceWriter.foreach(_.close())
        log("[CLEANUP] BspServerBuilder cleanup done")
      }
    }

  /** Convenience method to initialize BSP session after connection.
    *
    * Sends buildInitialize and buildInitialized notifications.
    *
    * @param buildData
    *   Optional build data to pass to the BSP server (for bleep-to-bleep communication)
    */
  def initializeSession(
      server: BuildServer,
      clientName: String,
      clientVersion: String,
      rootUri: String,
      buildData: Option[BspBuildData.Payload] = None
  ): IO[ch.epfl.scala.bsp4j.InitializeBuildResult] = IO.blocking {
    import ch.epfl.scala.bsp4j._
    import scala.jdk.CollectionConverters._

    val capabilities = new BuildClientCapabilities(List("scala", "java").asJava)
    val params = new InitializeBuildParams(
      clientName,
      clientVersion,
      "2.1.0", // BSP version
      rootUri,
      capabilities
    )

    // Pass rewritten build if available
    // Note: setData takes an Object that Gson will serialize. If we pass a String,
    // Gson will serialize it as a JSON string (adding quotes). We need to pass
    // a Gson JsonElement so it serializes as a JSON object.
    buildData.foreach { data =>
      params.setDataKind(BspBuildData.DataKind)
      val jsonStr = BspBuildData.Payload.encode(data)
      val gsonParser = new com.google.gson.JsonParser()
      val jsonElement = gsonParser.parse(jsonStr)
      params.setData(jsonElement)
    }

    val result = server.buildInitialize(params).get()
    server.onBuildInitialized()
    result
  }
}

/** Holds the BuildServer proxy and its lifecycle resources. */
case class BuildServerWithLifecycle(
    server: BuildServer,
    executor: ExecutorService,
    listening: java.util.concurrent.Future[Void],
    connection: BspConnection,
    traceWriter: Option[PrintWriter] = None
)
