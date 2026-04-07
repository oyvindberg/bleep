package bleep
package commands

import ryddig.Logger

case class ConfigAuthList(logger: Logger, userPaths: UserPaths, outputMode: OutputMode) extends BleepCommand {
  override def run(): Either[BleepException, Unit] =
    BleepConfigOps.loadOrDefault(userPaths).map { config =>
      val entries = config.authentications match {
        case None                                 => Nil
        case Some(auths) if auths.configs.isEmpty => Nil
        case Some(auths)                          =>
          auths.configs.toList.map { case (uri, entry) =>
            val kind = entry match {
              case model.AuthEntry.Static(_)                  => "static credentials"
              case model.AuthEntry.PrivateRepo(Some(account)) => s"account: $account"
              case model.AuthEntry.PrivateRepo(None)          => "default account"
            }
            AuthEntry(uri.toString, kind)
          }
      }

      outputMode match {
        case OutputMode.Text =>
          if (entries.isEmpty) logger.info("No authentications configured")
          else entries.foreach(e => logger.info(s"  ${e.uri} (${e.kind})"))
        case OutputMode.Json =>
          CommandResult.print(CommandResult.success(AuthList(entries)))
      }
    }
}
