package bleep
package commands

import ryddig.Logger

/** Interactive setup for remote cache credentials.
  *
  * Prompts for AWS access key ID and secret access key, then stores them in `~/.config/bleep/config.yaml`.
  *
  * Credentials can also be provided via `AWS_ACCESS_KEY_ID` / `AWS_SECRET_ACCESS_KEY` environment variables (no setup needed).
  */
case class ConfigRemoteCacheSetup(logger: Logger, userPaths: UserPaths) extends BleepCommand {
  override def run(): Either[BleepException, Unit] =
    // Check if env vars are already set
    model.RemoteCacheCredentials.fromEnv() match {
      case Some(_) =>
        logger.info("AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY environment variables are already set.")
        val overrideIdx = TuiPicker.pick(
          "Environment variables detected. What would you like to do?",
          List("Keep using environment variables (no config change)", "Store credentials in config file anyway")
        )
        overrideIdx match {
          case Some(1) => promptAndStore()
          case _ =>
            logger.info("No changes made. Remote cache will use environment variables.")
            Right(())
        }
      case None => promptAndStore()
    }

  private def promptAndStore(): Either[BleepException, Unit] = {
    System.err.println("Enter AWS Access Key ID:")
    System.err.print("> ")
    val accessKeyId = scala.io.StdIn.readLine().trim

    if (accessKeyId.isEmpty) {
      logger.info("Setup cancelled (empty key)")
      return Right(())
    }

    System.err.println("Enter AWS Secret Access Key:")
    System.err.print("> ")
    val secretAccessKey = scala.io.StdIn.readLine().trim

    if (secretAccessKey.isEmpty) {
      logger.info("Setup cancelled (empty secret)")
      return Right(())
    }

    val credentials = model.RemoteCacheCredentials(accessKeyId, secretAccessKey)

    BleepConfigOps.rewritePersisted(logger, userPaths)(_.copy(remoteCacheCredentials = Some(credentials))).map { _ =>
      logger.info(s"Remote cache credentials stored in ${userPaths.configYaml}")
    }
  }
}
