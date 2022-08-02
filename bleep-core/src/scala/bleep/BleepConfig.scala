package bleep

import bleep.bsp.CompileServerMode
import bleep.internal.{asYamlString, FileUtils}
import bleep.logging.Logger
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.yaml12.parser.decode
import io.circe.{Decoder, Encoder}

import java.nio.file.Files

case class BleepConfig(
    compileServerMode: CompileServerMode,
    compileServerJvm: Option[model.Jvm],
    authentications: Option[CoursierResolver.Authentications]
) {
  def jvmOrSetDefault(logger: Logger, userPaths: UserPaths): model.Jvm =
    compileServerJvm match {
      case Some(jvm) => jvm
      case None =>
        val defaultJvm = model.Jvm.graalvm
        val newConfig = copy(compileServerJvm = Some(defaultJvm))
        logger.warn(s"Defaulting bleep config to use $defaultJvm for compile server")
        BleepConfig.store(logger, userPaths, newConfig)
        defaultJvm
    }
}

object BleepConfig {
  val default = BleepConfig(
    compileServerMode = CompileServerMode.Shared,
    compileServerJvm = None,
    authentications = None
  )

  implicit val decoder: Decoder[BleepConfig] = deriveDecoder
  implicit val encoder: Encoder[BleepConfig] = deriveEncoder

  def store(logger: Logger, userPaths: UserPaths, config: BleepConfig): Unit = {
    FileUtils.writeString(userPaths.configYaml, asYamlString(config))
    logger.withContext(userPaths.configYaml).debug(s"wrote")
  }

  def load(userPaths: UserPaths): Either[BleepException, Option[BleepConfig]] =
    if (FileUtils.exists(userPaths.configYaml)) {
      decode[BleepConfig](Files.readString(userPaths.configYaml)) match {
        case Left(e)       => Left(new BleepException.InvalidJson(userPaths.configYaml, e))
        case Right(config) => Right(Some(config))
      }
    } else Right(None)

  def loadOrDefault(userPaths: UserPaths): Either[BleepException, BleepConfig] =
    load(userPaths).map(_.getOrElse(default))

  def lazyForceLoad(userPaths: UserPaths): Lazy[BleepConfig] =
    Lazy {
      BleepConfig.loadOrDefault(userPaths) match {
        case Left(th)      => throw th
        case Right(config) => config
      }
    }

  def rewritePersisted(logger: Logger, userPaths: UserPaths)(f: BleepConfig => BleepConfig): Either[BleepException, BleepConfig] =
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
