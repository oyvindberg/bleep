package bleep

import bleep.bsp.CompileServerMode
import bleep.internal.{FileUtils, Lazy}
import bleep.logging.Logger
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.jawn.decode
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder}

import java.nio.file.Files

case class BleepConfig(
    compileServerMode: CompileServerMode,
    compileServerJvm: Option[model.Jvm],
    authentications: CoursierResolver.Authentications
)

object BleepConfig {
  val default = BleepConfig(
    compileServerMode = CompileServerMode.Shared,
    compileServerJvm = None,
    authentications = CoursierResolver.Authentications.empty
  )

  implicit val decoder: Decoder[BleepConfig] = deriveDecoder
  implicit val encoder: Encoder[BleepConfig] = deriveEncoder

  def store(logger: Logger, userPaths: UserPaths, config: BleepConfig): Unit = {
    FileUtils.writeString(userPaths.configJson, config.asJson.spaces2)
    logger.withContext(userPaths.configJson).debug(s"wrote")
  }

  def load(userPaths: UserPaths): Either[BuildException, Option[BleepConfig]] =
    if (FileUtils.exists(userPaths.configJson)) {
      decode[BleepConfig](Files.readString(userPaths.configJson)) match {
        case Left(e)       => Left(new BuildException.InvalidJson(userPaths.configJson, e))
        case Right(config) => Right(Some(config))
      }
    } else Right(None)

  def loadOrDefault(userPaths: UserPaths): Either[BuildException, BleepConfig] =
    load(userPaths).map(_.getOrElse(default))

  def lazyForceLoad(userPaths: UserPaths): Lazy[BleepConfig] =
    Lazy {
      BleepConfig.loadOrDefault(userPaths) match {
        case Left(th)      => throw th
        case Right(config) => config
      }
    }

  def rewritePersisted(logger: Logger, userPaths: UserPaths)(f: BleepConfig => BleepConfig): Either[BuildException, BleepConfig] =
    load(userPaths).map {
      case None =>
        val config = f(default)
        store(logger, userPaths, config)
        config
      case Some(config0) =>
        val config = f(config0)
        if (config0 != config) store(logger, userPaths, config)
        config
    }
}
