package bleep

import bleep.internal.FileUtils
import bleep.model.{AuthEntry, Authentications, PrivateRepoScheme}
import coursier.core.Authentication
import ryddig.Logger

import java.net.URI
import java.util.concurrent.ConcurrentHashMap

/** Acquires credentials for private repository schemes (artifactregistry://, github://, gitlab://) by shelling out to provider CLI tools.
  *
  * Tokens are cached in-memory per process lifetime. Token acquisition is lazy -- it only happens when coursier actually resolves against a private repo, never
  * at startup.
  */
class CredentialProvider(logger: Logger, authentications: Option[Authentications]) {

  private val tokenCache: ConcurrentHashMap[String, String] = new ConcurrentHashMap()

  // cli.scala clears the environment, so we must pass PATH explicitly
  private val pathEnv: List[(String, String)] =
    sys.env.get("PATH").toList.map(p => ("PATH", p)) ++
      sys.env.get("HOME").toList.map(p => ("HOME", p)) ++
      sys.env.get("CLOUDSDK_CONFIG").toList.map(p => ("CLOUDSDK_CONFIG", p))

  /** Resolve credentials for a private repo URI. Returns a coursier Authentication ready to attach to a repository. */
  def resolve(scheme: PrivateRepoScheme, originalUri: URI): Authentication =
    scheme match {
      case PrivateRepoScheme.ArtifactRegistry => resolveArtifactRegistry(originalUri)
      case PrivateRepoScheme.GitHub           => resolveGitHub()
      case PrivateRepoScheme.GitLab           => resolveGitLab(originalUri)
    }

  private def resolveArtifactRegistry(repoUri: URI): Authentication = {
    val account = findPrivateRepoConfig(repoUri).flatMap(_.account)
    val cacheKey = s"artifactregistry:${account.getOrElse("")}"

    val token = tokenCache.computeIfAbsent(
      cacheKey,
      _ => {
        val cmd = List("gcloud", "auth", "print-access-token") ++ account.toList
        fetchToken(PrivateRepoScheme.ArtifactRegistry, cmd, account)
      }
    )
    Authentication("oauth2accesstoken").withPassword(token)
  }

  private def resolveGitHub(): Authentication = {
    val token = tokenCache.computeIfAbsent(
      "github",
      _ => fetchToken(PrivateRepoScheme.GitHub, List("gh", "auth", "token"), None)
    )
    Authentication("token").withPassword(token)
  }

  private def resolveGitLab(repoUri: URI): Authentication = {
    val host = Option(repoUri.getAuthority).getOrElse("gitlab.com")
    val cacheKey = s"gitlab:$host"
    val token = tokenCache.computeIfAbsent(
      cacheKey,
      _ => fetchToken(PrivateRepoScheme.GitLab, List("glab", "config", "get", "token", "--host", host), None)
    )
    // GitLab accepts both Private-Token header and basic auth. Use both for compatibility.
    Authentication("Private-Token").withPassword(token).withHttpHeaders(List(("Private-Token", token)))
  }

  /** Find the best-matching PrivateRepo config entry for a given URI by longest prefix match. */
  private def findPrivateRepoConfig(repoUri: URI): Option[AuthEntry.PrivateRepo] =
    authentications
      .map(_.configs.toList)
      .getOrElse(Nil)
      .collect { case (configUri, pr: AuthEntry.PrivateRepo) => (configUri, pr) }
      .filter { case (configUri, _) => repoUri.toString.startsWith(configUri.toString) }
      .sortBy { case (configUri, _) => -configUri.toString.length } // longest prefix first
      .headOption
      .map { case (_, pr) => pr }

  private def fetchToken(scheme: PrivateRepoScheme, cmd: List[String], account: Option[String]): String = {
    val startNanos = System.nanoTime()
    val result =
      try
        cli(
          action = s"${scheme.cliCommand} credential",
          cwd = FileUtils.TempDir,
          cmd = cmd,
          logger = logger,
          out = cli.Out.ViaLogger(logger),
          in = cli.In.No,
          env = pathEnv
        )
      catch {
        case e: BleepException.Text =>
          throw new BleepException.Text(buildErrorMessage(scheme, account, e.message))
      }

    val elapsedMs = (System.nanoTime() - startNanos) / 1_000_000
    val token = result.stdout.headOption.getOrElse("").trim

    if (token.isEmpty) {
      throw new BleepException.Text(
        s"${scheme.displayName}: credential command returned empty output. Run '${scheme.cliCommand} auth login' to authenticate."
      )
    }

    logger.info(s"Acquired ${scheme.displayName} token in ${elapsedMs}ms")
    token
  }

  private def buildErrorMessage(scheme: PrivateRepoScheme, account: Option[String], originalError: String): String = {
    val accountInfo = account.map(a => s" (account: $a)").getOrElse("")
    val sb = new StringBuilder
    sb.append(s"${scheme.displayName}: failed to acquire credentials$accountInfo.\n")
    sb.append(s"\n")

    // Check if the CLI tool is likely not installed (heuristic from error message)
    if (originalError.contains("exit code 127") || originalError.contains("No such file or directory")) {
      sb.append(s"'${scheme.cliCommand}' does not appear to be installed.\n")
      sb.append(s"Install: ${scheme.installUrl}\n")
      sb.append(s"Then run: ${scheme.cliCommand} auth login\n")
    } else {
      sb.append(s"This usually means credentials have expired or are not set up.\n")
      account match {
        case Some(acct) =>
          sb.append(s"Run: ${scheme.cliCommand} auth login $acct\n")
          sb.append(s"Or update your account: bleep config auth setup\n")
        case None =>
          sb.append(s"Run: ${scheme.cliCommand} auth login\n")
      }
    }

    sb.toString()
  }
}
