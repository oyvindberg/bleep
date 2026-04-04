package bleep.bsp

import bleep._
import cats.effect.unsafe.implicits.global

import java.io.{ByteArrayOutputStream, InputStream}
import java.nio.file.Path

object BspProxy {
  def run(pre: Prebootstrapped): ExitCode = {
    val config = BleepConfigOps.loadOrDefault(pre.userPaths).orThrow
    val build = pre.existingBuild.buildFile.forceGet.getOrElse {
      model.BuildFile(
        model.$schema,
        model.BleepVersion.current,
        model.JsonMap.empty,
        model.JsonMap.empty,
        model.JsonList.empty,
        model.JsonMap.empty,
        None,
        None
      )
    }
    val resolver = CoursierResolver.Factory.default(pre, config, build)

    // Read the first JSON-RPC message from stdin to extract semanticdb versions
    val initMessageBytes = readFirstMessage(System.in)
    val extraServerClasspath = resolveExtraClasspath(initMessageBytes, pre.logger)

    val bspConfig = SetupBleepBsp(
      compileServerMode = config.compileServerModeOrDefault,
      config = config,
      resolvedJvm = pre.resolvedJvm.forceGet,
      userPaths = pre.userPaths,
      resolver = resolver,
      logger = pre.logger,
      extraServerClasspath = extraServerClasspath
    ).orThrow

    val program = BspRifle.ensureRunningAndConnect(bspConfig, pre.logger).use { connection =>
      cats.effect.IO.blocking {
        // Forward the already-read init message to the server
        val header = s"Content-Length: ${initMessageBytes.length}\r\n\r\n"
        connection.output.write(header.getBytes("US-ASCII"))
        connection.output.write(initMessageBytes)
        connection.output.flush()

        val stdinToServer = new Thread("bsp-stdin-to-server") {
          setDaemon(true)
          override def run(): Unit =
            try System.in.transferTo(connection.output)
            catch { case _: Exception => () }
            finally
              try connection.output.close()
              catch { case _: Exception => () }
        }
        val serverToStdout = new Thread("bsp-server-to-stdout") {
          setDaemon(true)
          override def run(): Unit =
            try connection.input.transferTo(System.out)
            catch { case _: Exception => () }
            finally
              try System.out.close()
              catch { case _: Exception => () }
        }

        stdinToServer.start()
        serverToStdout.start()

        // Wait for server output to finish (server disconnects or dies)
        serverToStdout.join()
        ExitCode.Success
      }
    }

    program.unsafeRunSync()
  }

  /** Read a single JSON-RPC message from the input stream using Content-Length framing.
    *
    * Reads headers until blank line, extracts Content-Length, then reads that many bytes of body.
    */
  private def readFirstMessage(in: InputStream): Array[Byte] = {
    // Read headers line by line until we hit a blank line
    var contentLength = -1
    var line = readLine(in)
    while (line != null && line.nonEmpty) {
      if (line.startsWith("Content-Length:")) {
        contentLength = line.substring("Content-Length:".length).trim.toInt
      }
      line = readLine(in)
    }
    if (contentLength < 0) {
      throw new BleepException.Text("Failed to read Content-Length from first BSP message")
    }
    val body = new Array[Byte](contentLength)
    var offset = 0
    while (offset < contentLength) {
      val n = in.read(body, offset, contentLength - offset)
      if (n < 0) throw new BleepException.Text("Unexpected EOF reading first BSP message body")
      offset += n
    }
    body
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

  /** Parse the init message body to extract javaSemanticdbVersion, then resolve semanticdb-javac JARs via Coursier. */
  private def resolveExtraClasspath(body: Array[Byte], logger: ryddig.Logger): Seq[Path] = {
    val jsonStr = new String(body, "UTF-8")
    val json = io.circe.parser.parse(jsonStr).toOption
    val dataJson = json
      .flatMap(_.hcursor.get[io.circe.Json]("params").toOption)
      .flatMap(_.hcursor.get[io.circe.Json]("initializationOptions").toOption)
      .orElse(json.flatMap(_.hcursor.get[io.circe.Json]("params").toOption).flatMap(_.hcursor.get[io.circe.Json]("data").toOption))

    val javaSemanticdbVersion: Option[String] =
      dataJson.flatMap(_.hcursor.get[String]("javaSemanticdbVersion").toOption)

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
}
