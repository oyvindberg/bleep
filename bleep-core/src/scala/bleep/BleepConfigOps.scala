package bleep

import bleep.internal.{writeYamlLogged, FileUtils}
import ryddig.Logger

import java.nio.file.Files

object BleepConfigOps {
  def store(logger: Logger, userPaths: UserPaths, config: model.BleepConfig): Unit =
    writeYamlLogged(logger, "Wrote update config file", config, userPaths.configYaml)

  def load(userPaths: UserPaths): Either[BleepException, Option[model.BleepConfig]] =
    if (FileUtils.exists(userPaths.configYaml)) {
      yaml.decode[model.BleepConfig](Files.readString(userPaths.configYaml)) match {
        case Left(e)       => Left(new BleepException.InvalidJson(userPaths.configYaml, e))
        case Right(config) => Right(Some(config))
      }
    } else Right(None)

  def loadOrDefault(userPaths: UserPaths): Either[BleepException, model.BleepConfig] =
    load(userPaths).map(_.getOrElse(model.BleepConfig.default))

  def lazyForceLoad(userPaths: UserPaths): Lazy[model.BleepConfig] =
    Lazy(loadOrDefault(userPaths).orThrow)

  def rewritePersisted(logger: Logger, userPaths: UserPaths)(f: model.BleepConfig => model.BleepConfig): Either[BleepException, model.BleepConfig] =
    load(userPaths).map {
      case None =>
        val config = f(model.BleepConfig.default)
        store(logger, userPaths, config)
        config
      case Some(config0) =>
        val config = f(config0)
        if (config0 != config) store(logger, userPaths, config)
        config
    }
}
