package bleep.bsp

import bleep._
import cats.effect.unsafe.implicits.global
import io.circe.Json

import java.io.{ByteArrayOutputStream, InputStream}
import java.nio.file.Path

/** `bleep bsp` — what an IDE launches, per the `.bsp/bleep.json` descriptor.
  *
  * This is a full bleep client, not a pipe. It loads and resolves the build, hands it to the daemon in `build/initialize`, watches the build files for the life
  * of the IDE session, and pushes `bleep/buildChanged` when they change. That is what lets the daemon be a pure executor: every route to it — `bleep compile`,
  * the MCP server, and an IDE — now arrives with a build already resolved by a bleep process, so there is exactly one place a build is loaded and one place its
  * files are watched.
  *
  * It still forwards the IDE's own traffic untouched. Only the client-to-server direction is framed, so this process can interleave its own notifications
  * without splitting someone else's message; server-to-client stays a straight copy.
  */
object BspProxy {

  /** Clients we treat as IDEs: they get the semanticDB rewrite and the `bsp` build variant.
    *
    * This decision used to live in the server, keyed off the same display names. It belongs here now — the client owns the build, so the client has to be the
    * one that decides which build to resolve. Getting it wrong fails quietly rather than loudly: the wrong variant compiles into a different output directory,
    * and a missing rewrite costs Metals goto-definition and find-references.
    */
  private val KnownIdeClients = Set("Metals", "IntelliJ", "IntelliJ-BSP")

  private val DefaultSemanticDbVersion = "4.15.2"

  def run(pre: Prebootstrapped): ExitCode = {
    val config = BleepConfigOps.loadOrDefault(pre.userPaths).orThrow

    // The IDE's initialize says who it is and which semanticDB versions it wants, and we need all
    // of that before we can decide which build to resolve.
    val initMessageBytes = readMessage(System.in).getOrElse(
      throw new BleepException.Text("Unexpected EOF reading the first BSP message")
    )
    val initJson = io.circe.parser
      .parse(new String(initMessageBytes, "UTF-8"))
      .getOrElse(throw new BleepException.Text("First BSP message was not valid JSON"))

    val displayName = initJson.hcursor.downField("params").get[String]("displayName").toOption
    val isIdeClient = displayName.exists(KnownIdeClients.contains)
    val initData = initJson.hcursor.downField("params").get[Json]("data").toOption
    val semanticDbVersion = initData.flatMap(_.hcursor.get[String]("semanticdbVersion").toOption).getOrElse(DefaultSemanticDbVersion)
    val javaSemanticdbVersion = initData.flatMap(_.hcursor.get[String]("javaSemanticdbVersion").toOption)

    val buildRewrites: List[bleep.rewrites.BuildRewrite] =
      if (isIdeClient) List(new bleep.rewrites.semanticDb(semanticDbVersion)) else Nil

    // The variant names the output directory, so pick it explicitly rather than letting it be
    // derived from the rewrite list — that would produce a name the daemon has never used.
    val variant = if (isIdeClient) model.BuildVariant.BSP else model.BuildVariant.Normal
    val bspPre = pre.copy(
      buildPaths = BuildPaths(pre.buildPaths.cwd, pre.existingBuild.bleepYaml, variant, pre.buildPaths.wantedBleepVersion)
    )

    val started = bootstrap
      .from(bspPre, ResolveProjects.InMemory, buildRewrites, config, CoursierResolver.Factory.default)
      .orThrow

    pre.logger.info(
      s"Resolved build for ${displayName.getOrElse("BSP client")} (variant ${variant.name}, ${started.build.explodedProjects.size} projects)"
    )

    val patchedInit = withBuildPayload(initJson, BspBuildData.Payload.from(started))

    val bspConfig = SetupBleepBsp(
      compileServerMode = config.compileServerModeOrDefault,
      config = config,
      resolvedJvm = pre.resolvedJvm.forceGet,
      userPaths = pre.userPaths,
      resolver = started.resolver,
      logger = pre.logger,
      extraServerClasspath = resolveExtraClasspath(javaSemanticdbVersion, pre.logger)
    ).orThrow

    val program = BspRifle.ensureRunningAndConnect(bspConfig, pre.logger).use { connection =>
      cats.effect.IO.blocking {
        // Every write to the server goes through here. The build watcher and the forwarding thread
        // both produce messages, and a half-written frame would desync the stream.
        val serverWriteLock = new Object
        def sendToServer(bytes: Array[Byte]): Unit =
          serverWriteLock.synchronized {
            connection.output.write(s"Content-Length: ${bytes.length}\r\n\r\n".getBytes("US-ASCII"))
            connection.output.write(bytes)
            connection.output.flush()
          }

        sendToServer(patchedInit)

        val currentStarted = new java.util.concurrent.atomic.AtomicReference(started)

        def reloadAndPush(why: String): Unit =
          currentStarted.get().reloadFromDisk(buildRewrites) match {
            case Left(ex) =>
              pre.logger.error(s"Build changed ($why) but could not be reloaded: ${ex.getMessage}", ex)
            case Right(None) =>
              () // parsed to the same build, nothing for the daemon to do
            case Right(Some(newStarted)) =>
              currentStarted.set(newStarted)
              sendToServer(buildChangedNotification(BspBuildData.Payload.from(newStarted)))
              pre.logger.info(s"Sent updated build to bleep-bsp ($why)")
          }

        val buildWatcher = BleepFileWatching.build(bspPre) { changedFiles =>
          reloadAndPush(changedFiles.mkString(", "))
        }
        val watcherThread = new Thread(() => buildWatcher.run(FileWatching.StopWhen.Never), "bsp-build-watcher")
        watcherThread.setDaemon(true)
        watcherThread.start()

        val stdinToServer = new Thread("bsp-stdin-to-server") {
          setDaemon(true)
          override def run(): Unit =
            try {
              var message = readMessage(System.in)
              while (message.isDefined) {
                val bytes = message.get
                // The IDE asks for a reload when it notices the build changed. We own the build, so
                // re-resolve and push before letting the request through — otherwise the server
                // would just re-adopt the build it already has.
                if (isReloadRequest(bytes)) reloadAndPush("workspace/reload")
                sendToServer(bytes)
                message = readMessage(System.in)
              }
            } catch { case _: Exception => () }
            finally
              try connection.output.close()
              catch { case _: Exception => () }
        }
        val serverToStdout = new Thread("bsp-server-to-stdout") {
          setDaemon(true)
          override def run(): Unit =
            try connection.input.transferTo(System.out): Unit
            catch { case _: Exception => () }
            finally
              try System.out.close()
              catch { case _: Exception => () }
        }

        stdinToServer.start()
        serverToStdout.start()

        // Wait for server output to finish (server disconnects or dies)
        serverToStdout.join()
        try buildWatcher.close()
        catch { case _: Exception => () }
        ExitCode.Success
      }
    }

    program.unsafeRunSync()
  }

  /** Add the resolved build to `params.data`, leaving whatever the IDE put there intact. */
  private[bsp] def withBuildPayload(initJson: Json, payload: BspBuildData.Payload): Array[Byte] = {
    import io.circe.syntax._
    val existingData = initJson.hcursor.downField("params").get[Json]("data").toOption.getOrElse(Json.obj())
    val newData = existingData.deepMerge(Json.obj(BspBuildData.DataField -> payload.asJson))
    initJson.hcursor
      .downField("params")
      .withFocus(_.deepMerge(Json.obj("dataKind" -> BspBuildData.DataKind.asJson, "data" -> newData)))
      .top
      .getOrElse(throw new BleepException.Text("Could not add build data to the BSP initialize message"))
      .noSpaces
      .getBytes("UTF-8")
  }

  private[bsp] def buildChangedNotification(payload: BspBuildData.Payload): Array[Byte] = {
    import io.circe.syntax._
    Json
      .obj(
        "jsonrpc" -> "2.0".asJson,
        "method" -> bleep.bsp.protocol.BleepBspProtocol.BuildChanged.asJson,
        "params" -> payload.asJson
      )
      .noSpaces
      .getBytes("UTF-8")
  }

  private[bsp] def isReloadRequest(messageBytes: Array[Byte]): Boolean =
    io.circe.parser
      .parse(new String(messageBytes, "UTF-8"))
      .flatMap(_.hcursor.get[String]("method"))
      .exists(_ == "workspace/reload")

  /** Read one JSON-RPC message using Content-Length framing. `None` at end of stream. */
  private[bsp] def readMessage(in: InputStream): Option[Array[Byte]] = {
    var contentLength = -1
    var line = readLine(in)
    if (line == null) return None
    while (line != null && line.nonEmpty) {
      if (line.startsWith("Content-Length:")) {
        contentLength = line.substring("Content-Length:".length).trim.toInt
      }
      line = readLine(in)
    }
    if (contentLength < 0) {
      throw new BleepException.Text("Failed to read Content-Length from BSP message")
    }
    val body = new Array[Byte](contentLength)
    var offset = 0
    while (offset < contentLength) {
      val n = in.read(body, offset, contentLength - offset)
      if (n < 0) throw new BleepException.Text("Unexpected EOF reading BSP message body")
      offset += n
    }
    Some(body)
  }

  /** Read a line terminated by \r\n from an InputStream. Returns the line content without the terminator. */
  private def readLine(in: InputStream): String = {
    val buf = new ByteArrayOutputStream()
    var prev = -1
    var b = in.read()
    while (b != -1) {
      if (prev == '\r' && b == '\n') {
        // Remove trailing \r from buffer
        val bytes = buf.toByteArray
        return new String(bytes, 0, bytes.length - 1, "US-ASCII")
      }
      buf.write(b)
      prev = b
      b = in.read()
    }
    // EOF — return whatever we have (empty string if nothing)
    val bytes = buf.toByteArray
    if (bytes.isEmpty) null
    else new String(bytes, "US-ASCII")
  }

  /** Resolve semanticdb-javac for the BSP server classpath, when the IDE asked for Java semanticDB. */
  private def resolveExtraClasspath(javaSemanticdbVersion: Option[String], logger: ryddig.Logger): Seq[Path] =
    javaSemanticdbVersion match {
      case Some(version) =>
        logger.info(s"Resolving semanticdb-javac $version for BSP server classpath")
        import coursier._
        val dep = Dependency(Module(Organization("com.sourcegraph"), ModuleName("semanticdb-javac")), version)
        val jars = Fetch().addDependencies(dep).run().map(_.toPath).toSeq
        logger.info(s"Resolved ${jars.size} JARs for semanticdb-javac")
        jars
      case None =>
        Seq.empty
    }
}
