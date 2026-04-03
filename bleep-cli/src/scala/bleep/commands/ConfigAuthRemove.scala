package bleep
package commands

import ryddig.Logger

import java.net.URI

case class ConfigAuthRemove(logger: Logger, userPaths: UserPaths, uriPrefix: URI) extends BleepCommand {
  override def run(): Either[BleepException, Unit] =
    BleepConfigOps
      .rewritePersisted(logger, userPaths) { config =>
        config.authentications match {
          case None =>
            logger.warn(s"No authentications configured, nothing to remove")
            config
          case Some(auths) =>
            val updated = auths.configs - uriPrefix
            if (updated.size == auths.configs.size) {
              logger.warn(s"No authentication found for $uriPrefix")
            } else {
              logger.info(s"Removed authentication for $uriPrefix")
            }
            config.copy(authentications = Some(model.Authentications(updated)))
        }
      }
      .map(_ => ())
}
