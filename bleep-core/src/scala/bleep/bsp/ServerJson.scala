package bleep
package bsp

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import io.circe.syntax._

import java.nio.file.{Files, Path}

/** Identity of a spawned BSP daemon, written into its socket directory at spawn time.
  *
  * The socket directory is named after [[JvmKey.hash]], a SHA-256 of bleep version + JVM + sorted java options. That hash is not reversible: given a directory
  * on disk there is no way to tell which bleep version or JVM owns it. Once the daemon is dead — the case where you most want to know what it was — even asking
  * it is off the table.
  *
  * So we write it down. This is what lets `bleep server ls` name a dead or wedged daemon, and what gives `bleep server restart` a command to replay.
  */
case class ServerJson(
    bleepVersion: String,
    jvmName: String,
    jvmVersion: String,
    javaBin: String,
    javaOpts: List[String],
    serverMainClass: String,
    command: List[String],
    workingDir: String,
    spawnedAtEpochMs: Long
)

object ServerJson {
  implicit val codec: Codec[ServerJson] = deriveCodec

  val FileName = "server.json"

  def file(socketDir: Path): Path = socketDir / FileName

  def write(socketDir: Path, value: ServerJson): Unit =
    Files.writeString(file(socketDir), value.asJson.spaces2): Unit

  /** `None` means the file is absent, which is a real and expected state: daemons spawned before this file existed. Every other outcome — unreadable file,
    * malformed json, wrong shape — throws, because a socket directory we cannot make sense of is a bug to surface, not a row to quietly drop from `ls`.
    */
  def read(socketDir: Path): Option[ServerJson] = {
    val path = file(socketDir)
    if (!Files.exists(path)) None
    else {
      val content = Files.readString(path)
      io.circe.parser.decode[ServerJson](content) match {
        case Right(value) => Some(value)
        case Left(err)    => throw new BleepException.InvalidJson(path, err)
      }
    }
  }
}
