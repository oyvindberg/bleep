package bleep
package bsp

import bleep.internal.FileUtils
import bleep.logging.Logger
import io.circe.parser.decode
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, DecodingFailure, Encoder, Json}

import java.nio.file._
import java.nio.file.attribute.PosixFilePermission
import scala.build.blooprifle.BloopRifleConfig
import scala.util.{Failure, Properties, Random, Success, Try}

sealed abstract class CompileServerConfig(val status: String) {
  def asAddress(userPaths: UserPaths): BloopRifleConfig.Address =
    BloopRifleConfig.Address.DomainSocket(CompileServerConfig.bspSocketFile(userPaths, this))
}

object CompileServerConfig {
  case object NewEachInvocation extends CompileServerConfig("new-each-invocation")
  case object Shared extends CompileServerConfig("shared")
  val All = List(NewEachInvocation, Shared)

  implicit val decoder: Decoder[CompileServerConfig] =
    Decoder.instance { c =>
      c.downField("status")
        .as[String]
        .flatMap(str => All.find(_.status == str).toRight(DecodingFailure(s"$str not among ${All.map(_.status).mkString(", ")}", c.history)))
    }

  implicit val encoder: Encoder[CompileServerConfig] =
    Encoder.instance(config => Json.obj("status" -> config.status.asJson))

  def store(logger: Logger, userPaths: UserPaths, config: CompileServerConfig): Unit = {
    FileUtils.writeString(userPaths.bloopCompileServerJson, config.asJson.spaces2)
    logger.withContext(userPaths.bloopCompileServerJson).debug(s"wrote")
  }

  def delete(logger: Logger, userPaths: UserPaths): Unit = {
    Files.deleteIfExists(userPaths.bloopCompileServerJson)
    logger.withContext(userPaths.bloopCompileServerJson).debug(s"wrote")
  }

  def load(userPaths: UserPaths): Either[BuildException, CompileServerConfig] =
    if (FileUtils.exists(userPaths.bloopCompileServerJson)) {
      decode[CompileServerConfig](Files.readString(userPaths.bloopCompileServerJson)).left
        .map(e => new BuildException.InvalidJson(userPaths.bloopCompileServerJson, e))
    } else Right(Shared)

  private def socketDirectory(userPaths: UserPaths, socketId: String): Path = {
    val dir = userPaths.bspSocketDir
    // Ensuring that whenever dir exists, it has the right permissions
    if (!Files.isDirectory(dir)) {
      val tmpDir = dir.getParent / s".${dir.getFileName}.tmp-$socketId"
      try {
        Files.createDirectories(tmpDir)
        if (!Properties.isWin)
          Files.setPosixFilePermissions(
            tmpDir,
            java.util.Set.of(PosixFilePermission.OWNER_EXECUTE, PosixFilePermission.OWNER_READ, PosixFilePermission.OWNER_WRITE)
          )
        try Files.move(tmpDir, dir, StandardCopyOption.ATOMIC_MOVE)
        catch {
          case _: AtomicMoveNotSupportedException =>
            try Files.move(tmpDir, dir)
            catch {
              case _: FileAlreadyExistsException =>
            }
          case _: FileAlreadyExistsException =>
        }
      } finally {
        Files.deleteIfExists(tmpDir)
        ()
      }
    }
    dir
  }

  def bspSocketFile(userPaths: UserPaths, config: CompileServerConfig): Path = {
    val somewhatRandomIdentifier = Try(ProcessHandle.current.pid) match {
      case Failure(_) =>
        val r = new Random
        s"conn-${r.nextInt()}"
      case Success(pid) => s"proc-$pid"
    }

    val socketName: String =
      config match {
        case NewEachInvocation => somewhatRandomIdentifier
        case Shared            => "shared"
      }

    val socket: Path =
      socketDirectory(userPaths, somewhatRandomIdentifier) / socketName

    config match {
      case NewEachInvocation =>
        Runtime.getRuntime.addShutdownHook(
          new Thread("delete-bloop-bsp-named-socket") {
            override def run() = {
              FileUtils.deleteDirectory(socket)
              ()
            }
          }
        )

      case Shared =>
    }

    socket
  }
}
