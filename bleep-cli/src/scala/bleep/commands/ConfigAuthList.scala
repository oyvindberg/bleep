package bleep
package commands

import ryddig.Logger

case class ConfigAuthList(logger: Logger, userPaths: UserPaths) extends BleepCommand {
  override def run(): Either[BleepException, Unit] =
    BleepConfigOps.loadOrDefault(userPaths).map { config =>
      config.authentications match {
        case None =>
          logger.info("No authentications configured")
        case Some(auths) if auths.configs.isEmpty =>
          logger.info("No authentications configured")
        case Some(auths) =>
          auths.configs.foreach { case (uri, entry) =>
            val detail = entry match {
              case model.AuthEntry.Static(_)                  => "(static credentials)"
              case model.AuthEntry.PrivateRepo(Some(account)) => s"(account: $account)"
              case model.AuthEntry.PrivateRepo(None)          => "(default account)"
            }
            logger.info(s"  $uri $detail")
          }
      }
    }
}
