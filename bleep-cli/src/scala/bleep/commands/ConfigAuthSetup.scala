package bleep
package commands

import bleep.internal.FileUtils
import bleep.model.{AuthEntry, Authentications, PrivateRepoScheme}
import ryddig.Logger

import java.net.URI

case class ConfigAuthSetup(logger: Logger, userPaths: UserPaths) extends BleepCommand {
  override def run(): Either[BleepException, Unit] = {
    val providerIdx = TuiPicker.pick(
      "Select provider to configure:",
      List("Google Artifact Registry", "GitHub Packages", "GitLab Package Registry")
    )

    providerIdx match {
      case None =>
        logger.info("Setup cancelled")
        Right(())
      case Some(0) => setupArtifactRegistry()
      case Some(1) => setupGitHub()
      case Some(2) => setupGitLab()
      case _       => Right(())
    }
  }

  private val pathEnv: List[(String, String)] =
    sys.env.get("PATH").toList.map(p => ("PATH", p)) ++
      sys.env.get("HOME").toList.map(p => ("HOME", p))

  private def setupArtifactRegistry(): Either[BleepException, Unit] = {
    // Discover gcloud accounts
    val accounts =
      try {
        val result = cli(
          action = "gcloud auth list",
          cwd = FileUtils.TempDir,
          cmd = List("gcloud", "auth", "list", "--format=value(account)"),
          logger = logger,
          out = cli.Out.ViaLogger(logger),
          in = cli.In.No,
          env = pathEnv
        )
        result.stdout.filter(_.nonEmpty).toList
      } catch {
        case _: BleepException.Text =>
          throw new BleepException.Text(
            s"${PrivateRepoScheme.ArtifactRegistry.displayName}: 'gcloud' not found or failed.\n" +
              s"Install: ${PrivateRepoScheme.ArtifactRegistry.installUrl}\n" +
              s"Then run: gcloud auth login"
          )
      }

    accounts match {
      case Nil =>
        throw new BleepException.Text(
          "No gcloud accounts found. Run 'gcloud auth login' to authenticate."
        )

      case single :: Nil =>
        logger.info(s"Single gcloud account found: $single")
        promptAndSaveArtifactRegistry(Some(single))

      case multiple =>
        val accountIdx = TuiPicker.pick(
          "Select GCP account for Artifact Registry:",
          multiple
        )
        accountIdx match {
          case None =>
            logger.info("Setup cancelled")
            Right(())
          case Some(idx) =>
            promptAndSaveArtifactRegistry(Some(multiple(idx)))
        }
    }
  }

  private def promptAndSaveArtifactRegistry(account: Option[String]): Either[BleepException, Unit] = {
    System.err.print("Enter repository URI prefix (e.g. artifactregistry://region-maven.pkg.dev/project): ")
    val uriStr = scala.io.StdIn.readLine()
    if (uriStr == null || uriStr.trim.isEmpty) {
      logger.info("Setup cancelled")
      return Right(())
    }

    val uri = URI.create(uriStr.trim)
    saveAuth(uri, AuthEntry.PrivateRepo(account))
    logger.info(s"Saved Artifact Registry config for $uri" + account.map(a => s" (account: $a)").getOrElse(""))
    Right(())
  }

  private def setupGitHub(): Either[BleepException, Unit] =
    // Verify gh is installed and authenticated
    try {
      cli(
        action = "gh auth token",
        cwd = FileUtils.TempDir,
        cmd = List("gh", "auth", "token"),
        logger = logger,
        out = cli.Out.ViaLogger(logger),
        in = cli.In.No,
        env = pathEnv
      )
      logger.info("GitHub CLI is authenticated. No additional bleep configuration needed.")
      logger.info("Just add github://owner/repo to your bleep.yaml resolvers.")
      Right(())
    } catch {
      case _: BleepException.Text =>
        throw new BleepException.Text(
          s"${PrivateRepoScheme.GitHub.displayName}: 'gh' not found or not authenticated.\n" +
            s"Install: ${PrivateRepoScheme.GitHub.installUrl}\n" +
            s"Then run: gh auth login"
        )
    }

  private def setupGitLab(): Either[BleepException, Unit] =
    // Verify glab is installed and authenticated
    try {
      cli(
        action = "glab auth token",
        cwd = FileUtils.TempDir,
        cmd = List("glab", "auth", "token"),
        logger = logger,
        out = cli.Out.ViaLogger(logger),
        in = cli.In.No,
        env = pathEnv
      )
      logger.info("GitLab CLI is authenticated. No additional bleep configuration needed.")
      logger.info("Just add gitlab://host/path to your bleep.yaml resolvers.")
      Right(())
    } catch {
      case _: BleepException.Text =>
        throw new BleepException.Text(
          s"${PrivateRepoScheme.GitLab.displayName}: 'glab' not found or not authenticated.\n" +
            s"Install: ${PrivateRepoScheme.GitLab.installUrl}\n" +
            s"Then run: glab auth login"
        )
    }

  private def saveAuth(uri: URI, entry: AuthEntry): Unit =
    BleepConfigOps
      .rewritePersisted(logger, userPaths) { config =>
        val existing = config.authentications.getOrElse(Authentications.empty)
        val updated = existing.copy(configs = existing.configs + (uri -> entry))
        config.copy(authentications = Some(updated))
      }
      .orThrow
}
