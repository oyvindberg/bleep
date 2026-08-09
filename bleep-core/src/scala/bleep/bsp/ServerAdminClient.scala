package bleep
package bsp

import bleep.bsp.protocol.{BleepServerAdmin, DaemonStatus, StatusRequest}
import cats.effect.unsafe.implicits.global
import io.circe.syntax._

import java.io.{BufferedReader, InputStream, InputStreamReader, OutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.Path

/** Why the daemon could not be asked. Each case is something to say out loud, never a row to silently drop. */
sealed trait AdminError {
  def message: String
}

object AdminError {

  /** The daemon predates the admin methods. `ls`, `kill`, `log` and `metrics` still work — they read files and signal processes — but rich status does not. */
  case class TooOld(socketDir: Path) extends AdminError {
    def message = s"daemon at $socketDir is from an older bleep and cannot report status — kill/log/metrics still work; restart it for full status"
  }

  /** The daemon speaks a newer admin protocol than this client knows. Rendered with whatever decoded, plus this warning. */
  case class TooNew(socketDir: Path, theirs: Int, ours: Int) extends AdminError {
    def message = s"daemon at $socketDir speaks admin protocol v$theirs, this bleep knows v$ours — upgrade bleep or expect missing fields"
  }

  case class Failed(socketDir: Path, reason: String) extends AdminError {
    def message = s"could not query daemon at $socketDir: $reason"
  }
}

/** A one-shot JSON-RPC caller for the daemon's admin methods.
  *
  * Deliberately not a BSP client: it never sends `build/initialize`, never ships a build, and never takes part in the compile lifecycle. It connects, asks one
  * question as an observer, and disconnects. That is what makes `bleep server ls` safe to run against a daemon an IDE is actively using, and what lets it work
  * from a directory with no bleep.yaml at all.
  */
object ServerAdminClient {

  private val ContentLength = "Content-Length:"
  private val MethodNotFound = -32601

  /** `observer = true` always: reading a daemon's status must never extend its life or reset its idle clock. */
  def status(socketDir: Path): Either[AdminError, DaemonStatus] =
    call(socketDir, BleepServerAdmin.StatusMethod, StatusRequest(observer = true).asJson).flatMap { json =>
      json.as[DaemonStatus] match {
        case Left(err)     => Left(AdminError.Failed(socketDir, s"could not decode status: ${err.getMessage}"))
        case Right(status) => Right(status)
      }
    }

  /** A daemon newer than this client is not an error — additive fields decode fine — but it is worth saying, since anything genuinely new will be missing. */
  def skewWarning(socketDir: Path, status: DaemonStatus): Option[AdminError.TooNew] =
    if (status.adminProtocolVersion > BleepServerAdmin.ProtocolVersion)
      Some(AdminError.TooNew(socketDir, status.adminProtocolVersion, BleepServerAdmin.ProtocolVersion))
    else None

  /** Ask the daemon to shut itself down. Distinct from killing the process: the daemon replies first, then runs its normal cleanup — lock released, pid and
    * socket files removed, metrics flushed — so the next client gets a clean refusal rather than a stale socket.
    */
  def shutdown(socketDir: Path): Either[AdminError, Unit] =
    call(socketDir, BleepServerAdmin.ShutdownMethod, io.circe.Json.obj()).map(_ => ())

  private def call(socketDir: Path, method: String, params: io.circe.Json): Either[AdminError, io.circe.Json] = {
    val address = BspRifleConfig.Address.DomainSocket(socketDir.resolve("socket"))

    val connection =
      try BspServerOperations.openConnection(address).unsafeRunSync()
      catch { case e: Throwable => return Left(AdminError.Failed(socketDir, s"${e.getClass.getSimpleName}: ${e.getMessage}")) }

    try {
      val (in, out) = streams(connection)
      writeMessage(out, requestJson(method, params))
      readMessage(in) match {
        case Left(reason) => Left(AdminError.Failed(socketDir, reason))
        case Right(json)  =>
          json.hcursor.downField("error").focus match {
            case Some(err) =>
              val code = err.hcursor.get[Int]("code").getOrElse(0)
              val msg = err.hcursor.get[String]("message").getOrElse(err.noSpaces)
              if (code == MethodNotFound) Left(AdminError.TooOld(socketDir)) else Left(AdminError.Failed(socketDir, msg))
            case None =>
              json.hcursor.downField("result").focus match {
                case Some(result) => Right(result)
                case None         => Left(AdminError.Failed(socketDir, s"response had neither result nor error: ${json.noSpaces}"))
              }
          }
      }
    } finally
      try connection.close()
      catch { case _: Throwable => () }
  }

  private def streams(connection: BspServerOperations.Connection): (InputStream, OutputStream) =
    connection match {
      case BspServerOperations.Connection.FromSocket(socket)   => (socket.getInputStream, socket.getOutputStream)
      case BspServerOperations.Connection.FromChannel(channel) =>
        (java.nio.channels.Channels.newInputStream(channel), java.nio.channels.Channels.newOutputStream(channel))
    }

  private def requestJson(method: String, params: io.circe.Json): String =
    io.circe.Json
      .obj(
        "jsonrpc" -> io.circe.Json.fromString("2.0"),
        "id" -> io.circe.Json.fromInt(1),
        "method" -> io.circe.Json.fromString(method),
        "params" -> params
      )
      .noSpaces

  private def writeMessage(out: OutputStream, body: String): Unit = {
    val bytes = body.getBytes(StandardCharsets.UTF_8)
    out.write(s"$ContentLength ${bytes.length}\r\n\r\n".getBytes(StandardCharsets.UTF_8))
    out.write(bytes)
    out.flush()
  }

  private def readMessage(in: InputStream): Either[String, io.circe.Json] = {
    val reader = new BufferedReader(new InputStreamReader(in, StandardCharsets.UTF_8))

    var contentLength = -1
    var line = reader.readLine()
    while (line != null && line.nonEmpty) {
      if (line.startsWith(ContentLength)) contentLength = line.drop(ContentLength.length).trim.toInt
      line = reader.readLine()
    }
    if (line == null) return Left("connection closed before a response arrived")
    if (contentLength < 0) return Left("response had no Content-Length header")

    // The reader decodes UTF-8, so Content-Length (bytes) is an upper bound on chars. Read until the JSON parses or the stream ends.
    val buf = new Array[Char](contentLength)
    var read = 0
    while (read < contentLength) {
      val n = reader.read(buf, read, contentLength - read)
      if (n < 0) return io.circe.parser.parse(new String(buf, 0, read)).left.map(e => s"truncated response: ${e.getMessage}")
      read += n
      val candidate = new String(buf, 0, read)
      io.circe.parser.parse(candidate) match {
        case Right(json) => return Right(json)
        case Left(_)     => ()
      }
    }
    io.circe.parser.parse(new String(buf, 0, read)).left.map(e => s"malformed response: ${e.getMessage}")
  }
}
