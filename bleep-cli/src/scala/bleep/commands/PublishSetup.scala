package bleep
package commands

import bleep.internal.{writeYamlLogged, FileUtils}
import bleep.model.{AuthEntry, Authentications, PrivateRepoScheme, Repository, ResolverName}
import coursier.core.Authentication
import ryddig.Logger

import java.net.URI
import java.security.SecureRandom

/** Interactive TUI wizard for setting up publishing. Handles auth configuration for private repos and Sonatype GPG key generation. */
case class PublishSetup(logger: Logger, userPaths: UserPaths, maybeStarted: Option[Started]) extends BleepCommand {
  override def run(): Either[BleepException, Unit] = {
    val providerIdx = TuiPicker.pick(
      "Select publishing target to configure:",
      List(
        "Artifactory / Nexus / generic Maven repo",
        "Google Artifact Registry",
        "GitHub Packages",
        "GitLab Package Registry",
        "Sonatype / Maven Central"
      )
    )

    providerIdx match {
      case None =>
        logger.info("Setup cancelled")
        Right(())
      case Some(0) => setupGenericMaven()
      case Some(1) => setupArtifactRegistry()
      case Some(2) => setupGitHub()
      case Some(3) => setupGitLab()
      case Some(4) => setupSonatype()
      case _       => Right(())
    }
  }

  // --- Generic Maven (Artifactory / Nexus / etc.) ---

  private def setupGenericMaven(): Either[BleepException, Unit] = {
    logger.info("=== Generic Maven repository setup ===")
    logger.info("")
    logger.info("Configures a plain http(s):// Maven repo with HTTP Basic Auth.")
    logger.info("Use this for Artifactory, Nexus, or any company-internal Maven repo")
    logger.info("that isn't one of the named providers above.")
    logger.info("")

    val resolverName = promptNonEmpty("Resolver name (used as `bleep publish <name>`)", default = Some("company-releases"))
    val uriStr = promptNonEmpty("Repository URI (e.g. https://artifactory.company.com/artifactory/libs-release)", default = None)
    val uri =
      try URI.create(uriStr)
      catch {
        case _: IllegalArgumentException =>
          throw new BleepException.Text(s"Not a valid URI: $uriStr")
      }
    if (uri.getScheme != "http" && uri.getScheme != "https")
      throw new BleepException.Text(s"URI scheme must be http or https, got: ${uri.getScheme}")

    val user = promptNonEmpty("Username (typically an Artifactory identity-token user / email)", default = None)
    val password = promptPassword("Password / token")

    saveAuthEntry(uri, AuthEntry.Static(Authentication(user, password)))
    logger.info(s"Saved credentials for $uri")

    maybeStarted match {
      case Some(_) =>
        addResolverToBuild(resolverName, Repository.Maven(Some(ResolverName(resolverName)), uri))
        logger.info(s"Publish with: bleep publish $resolverName")
      case None =>
        logger.info("")
        logger.info("Add this resolver to your bleep.yaml:")
        logger.info("  resolvers:")
        logger.info(s"    - name: $resolverName")
        logger.info(s"      type: maven")
        logger.info(s"      uri: $uri")
        logger.info("")
        logger.info(s"Then publish with: bleep publish $resolverName")
    }
    Right(())
  }

  /** Read a non-empty trimmed line from stdin, optionally with a default. Throws BleepException on cancel/EOF. */
  private def promptNonEmpty(prompt: String, default: Option[String]): String = {
    val rendered = default match {
      case Some(d) => s"$prompt [$d]: "
      case None    => s"$prompt: "
    }
    System.err.print(rendered)
    Option(scala.io.StdIn.readLine()).map(_.trim).filter(_.nonEmpty).orElse(default) match {
      case Some(s) => s
      case None    => throw new BleepException.Text("Cancelled (empty input)")
    }
  }

  /** Read a password without echoing if a console is attached; fall back to readLine otherwise. */
  private def promptPassword(prompt: String): String =
    Option(System.console()) match {
      case Some(console) =>
        val chars = console.readPassword(s"$prompt: ")
        if (chars == null || chars.isEmpty) throw new BleepException.Text("Cancelled (empty password)")
        new String(chars)
      case None =>
        System.err.print(s"$prompt (input will be visible — no console attached): ")
        Option(scala.io.StdIn.readLine())
          .map(_.trim)
          .filter(_.nonEmpty)
          .getOrElse(throw new BleepException.Text("Cancelled (empty password)"))
    }

  private val pathEnv: List[(String, String)] =
    sys.env.get("PATH").toList.map(p => ("PATH", p)) ++
      sys.env.get("HOME").toList.map(p => ("HOME", p)) ++
      sys.env.get("GNUPGHOME").toList.map(p => ("GNUPGHOME", p))

  /** Detect GitHub owner/repo from git remote. */
  private def detectGitHubRemote(): Option[String] =
    detectGitRemote("github.com")

  /** Detect GitLab project path from git remote. */
  private def detectGitLabRemote(): Option[String] =
    detectGitRemote("gitlab.com")

  /** Look up GitLab project ID via glab API. */
  private def lookupGitLabProjectId(projectPath: String): Option[String] =
    try {
      val encoded = projectPath.replace("/", "%2F")
      val result = cli(
        action = "glab api project",
        cwd = buildDir,
        cmd = List("glab", "api", s"projects/$encoded"),
        logger = logger,
        out = cli.Out.ViaLogger(logger),
        in = cli.In.No,
        env = pathEnv
      )
      // Parse JSON to extract "id" field
      val output = result.stdout.mkString
      """"id"\s*:\s*(\d+)""".r.findFirstMatchIn(output).map(_.group(1))
    } catch {
      case _: BleepException.Text =>
        logger.warn("Could not look up GitLab project ID via API")
        None
    }

  private val buildDir: java.nio.file.Path =
    maybeStarted.map(_.buildPaths.buildDir).getOrElse(FileUtils.cwd)

  private def detectGitRemote(host: String): Option[String] = {
    val patterns = List(
      s"https://$host/([^/]+/[^/]+?)(?:\\.git)?$$".r,
      s"git@$host:([^/]+/[^/]+?)(?:\\.git)?$$".r,
      s"git://$host/([^/]+/[^/]+?)(?:\\.git)?$$".r
    )
    try {
      // List all remote names
      val remoteNames = cli(
        action = "git remote",
        cwd = buildDir,
        cmd = List("git", "remote"),
        logger = logger,
        out = cli.Out.ViaLogger(logger),
        in = cli.In.No,
        env = pathEnv
      ).stdout.filter(_.nonEmpty)

      // Check each remote's URL for the target host
      remoteNames.iterator
        .flatMap { remoteName =>
          val result = cli(
            action = s"git remote url $remoteName",
            cwd = buildDir,
            cmd = List("git", "ls-remote", "--get-url", remoteName),
            logger = logger,
            out = cli.Out.ViaLogger(logger),
            in = cli.In.No,
            env = pathEnv
          )
          val url = result.stdout.headOption.getOrElse("").trim
          patterns.flatMap(_.findFirstMatchIn(url).map(_.group(1))).headOption
        }
        .nextOption()
    } catch {
      case _: BleepException.Text => None
    }
  }

  // --- Artifact Registry ---

  private def setupArtifactRegistry(): Either[BleepException, Unit] = {
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
        throw new BleepException.Text("No gcloud accounts found. Run 'gcloud auth login' to authenticate.")
      case single :: Nil =>
        logger.info(s"Single gcloud account found: $single")
        promptAndSaveAuth(Some(single))
      case multiple =>
        val accountIdx = TuiPicker.pick("Select GCP account for Artifact Registry:", multiple)
        accountIdx match {
          case None =>
            logger.info("Setup cancelled")
            Right(())
          case Some(idx) =>
            promptAndSaveAuth(Some(multiple(idx)))
        }
    }
  }

  private def promptAndSaveAuth(account: Option[String]): Either[BleepException, Unit] = {
    System.err.print("Enter repository URI prefix (e.g. artifactregistry://region-maven.pkg.dev/project): ")
    val uriStr = scala.io.StdIn.readLine()
    if (uriStr == null || uriStr.trim.isEmpty) {
      logger.info("Setup cancelled")
      return Right(())
    }
    val uri = URI.create(uriStr.trim)
    saveAuthEntry(uri, AuthEntry.PrivateRepo(account))
    logger.info(s"Saved Artifact Registry config for $uri" + account.map(a => s" (account: $a)").getOrElse(""))
    Right(())
  }

  // --- GitHub ---

  private def setupGitHub(): Either[BleepException, Unit] = {
    try
      cli(
        action = "gh auth token",
        cwd = FileUtils.TempDir,
        cmd = List("gh", "auth", "token"),
        logger = logger,
        out = cli.Out.ViaLogger(logger),
        in = cli.In.No,
        env = pathEnv
      )
    catch {
      case _: BleepException.Text =>
        throw new BleepException.Text(
          s"${PrivateRepoScheme.GitHub.displayName}: 'gh' not found or not authenticated.\n" +
            s"Install: ${PrivateRepoScheme.GitHub.installUrl}\n" +
            s"Then run: gh auth login"
        )
    }

    logger.info("GitHub CLI is authenticated.")

    // Try to detect GitHub remote
    val ownerRepo = detectGitHubRemote()

    ownerRepo match {
      case Some(or) =>
        logger.info(s"Detected GitHub repository: $or")
        addResolverToBuild("github-packages", Repository.Maven(Some(ResolverName("github-packages")), URI.create(s"github://$or")))
        logger.info("Publish with: bleep publish github-packages")
      case None =>
        logger.info("")
        logger.info("Could not detect GitHub remote. Add this to your bleep.yaml (replace owner/repo):")
        logger.info("  resolvers:")
        logger.info("    - name: github-packages")
        logger.info("      type: maven")
        logger.info("      uri: github://owner/repo")
        logger.info("")
        logger.info("Then publish with: bleep publish github-packages")
    }
    Right(())
  }

  // --- GitLab ---

  private def setupGitLab(): Either[BleepException, Unit] = {
    try
      cli(
        action = "glab config get token",
        cwd = FileUtils.TempDir,
        cmd = List("glab", "config", "get", "token", "--host", "gitlab.com"),
        logger = logger,
        out = cli.Out.ViaLogger(logger),
        in = cli.In.No,
        env = pathEnv
      )
    catch {
      case _: BleepException.Text =>
        throw new BleepException.Text(
          s"${PrivateRepoScheme.GitLab.displayName}: 'glab' not found or not authenticated.\n" +
            s"Install: ${PrivateRepoScheme.GitLab.installUrl}\n" +
            s"Then run: glab auth login"
        )
    }

    logger.info("GitLab CLI is authenticated.")

    val projectPath = detectGitLabRemote()

    projectPath match {
      case Some(path) =>
        logger.info(s"Detected GitLab project: $path")
        // Look up project ID via glab API
        val projectId = lookupGitLabProjectId(path)
        projectId match {
          case Some(id) =>
            logger.info(s"Project ID: $id")
            val uri = URI.create(s"gitlab://gitlab.com/api/v4/projects/$id/packages/maven")
            addResolverToBuild("gitlab-packages", Repository.Maven(Some(ResolverName("gitlab-packages")), uri))
            logger.info("Publish with: bleep publish gitlab-packages")
          case None =>
            logger.info("")
            logger.info("Could not look up project ID. Add resolver manually with the project ID:")
            logger.info(s"  Find it at: https://gitlab.com/$path")
            logger.info(s"  resolvers:")
            logger.info(s"    - name: gitlab-packages")
            logger.info(s"      type: maven")
            logger.info(s"      uri: gitlab://gitlab.com/api/v4/projects/<PROJECT_ID>/packages/maven")
        }
      case None =>
        logger.info("")
        logger.info("Add to your bleep.yaml (replace PROJECT_ID):")
        logger.info(s"  resolvers:")
        logger.info(s"    - name: gitlab-packages")
        logger.info(s"      type: maven")
        logger.info(s"      uri: gitlab://gitlab.com/api/v4/projects/<PROJECT_ID>/packages/maven")
        logger.info("")
        logger.info(s"Then publish with: bleep publish gitlab-packages")
    }
    Right(())
  }

  // --- Sonatype ---

  private def setupSonatype(): Either[BleepException, Unit] = {
    logger.info("=== Sonatype / Maven Central Setup ===")
    logger.info("")
    logger.info("Prerequisites:")
    logger.info("  1. A Sonatype account (create at https://central.sonatype.com/)")
    logger.info("  2. Publishing rights for your groupId (e.g. io.github.<username>)")
    logger.info("  3. GPG installed (gpg command)")
    logger.info("")

    // Check gpg is available
    try
      cli(
        action = "gpg --version",
        cwd = FileUtils.TempDir,
        cmd = List("gpg", "--version"),
        logger = logger,
        out = cli.Out.ViaLogger(logger),
        in = cli.In.No,
        env = pathEnv
      )
    catch {
      case _: BleepException.Text =>
        throw new BleepException.Text("'gpg' not found. Install GnuPG: https://gnupg.org/download/")
    }

    // Generate GPG key
    logger.info("Generating GPG key pair for artifact signing...")
    val passphrase = generatePassphrase()

    val keyConfig =
      s"""|%no-protection
          |Key-Type: RSA
          |Key-Length: 4096
          |Subkey-Type: RSA
          |Subkey-Length: 4096
          |Name-Real: bleep-publish
          |Name-Email: bleep-publish@localhost
          |Expire-Date: 2y
          |Passphrase: $passphrase
          |%commit""".stripMargin

    val genResult = cli(
      action = "gpg --gen-key",
      cwd = FileUtils.TempDir,
      cmd = List("gpg", "--batch", "--gen-key"),
      logger = logger,
      out = cli.Out.ViaLogger(logger),
      in = cli.In.Provided(keyConfig.getBytes("UTF-8")),
      env = pathEnv
    )

    // Extract key ID from output
    val keyIdPattern = "([0-9A-F]{40})".r
    val allOutput = (genResult.stdout ++ genResult.stderr).mkString("\n")
    val keyId = keyIdPattern.findFirstIn(allOutput).getOrElse {
      // Try listing keys to find the one we just created
      val listResult = cli(
        action = "gpg --list-keys",
        cwd = FileUtils.TempDir,
        cmd = List("gpg", "--list-keys", "--keyid-format", "long", "bleep-publish@localhost"),
        logger = logger,
        out = cli.Out.ViaLogger(logger),
        in = cli.In.No,
        env = pathEnv
      )
      keyIdPattern
        .findFirstIn(listResult.stdout.mkString("\n"))
        .getOrElse(
          throw new BleepException.Text("Failed to extract GPG key ID. Check 'gpg --list-keys' manually.")
        )
    }

    logger.info(s"Generated GPG key: $keyId")

    // Upload public key to keyserver
    logger.info("Uploading public key to keyserver.ubuntu.com...")
    try {
      cli(
        action = "gpg --send-key",
        cwd = FileUtils.TempDir,
        cmd = List("gpg", "--keyserver", "hkp://keyserver.ubuntu.com", "--send-key", keyId),
        logger = logger,
        out = cli.Out.ViaLogger(logger),
        in = cli.In.No,
        env = pathEnv
      )
      logger.info("Public key uploaded successfully.")
    } catch {
      case e: BleepException.Text =>
        logger.warn(s"Failed to upload key to keyserver (${e.message}). You may need to upload manually.")
    }

    // Export private key as base64
    val exportResult = cli(
      action = "gpg --export-secret-keys",
      cwd = FileUtils.TempDir,
      cmd = List("gpg", "--armor", "--export-secret-keys", keyId),
      logger = logger,
      out = cli.Out.ViaLogger(logger),
      in = cli.In.No,
      env = pathEnv
    )
    val privateKeyArmored = exportResult.stdout.mkString("\n")
    val pgpSecret = java.util.Base64.getEncoder.encodeToString(privateKeyArmored.getBytes("UTF-8"))

    // Output CI env vars
    logger.info("")
    logger.info("=== CI Environment Variables ===")
    logger.info("Set these in your CI system (GitHub Actions secrets, GitLab CI variables, etc.):")
    logger.info("")
    logger.info(s"  PGP_PASSPHRASE=$passphrase")
    logger.info(s"  PGP_SECRET=$pgpSecret")
    logger.info("  SONATYPE_USERNAME=<your Sonatype token username>")
    logger.info("  SONATYPE_PASSWORD=<your Sonatype token password>")
    logger.info("")
    logger.info("To get Sonatype tokens: https://central.sonatype.com/ > username > View Account > Generate User Token")
    logger.info("")
    logger.info("Then publish with: bleep publish sonatype")
    logger.info("")
    logger.info("Add 'publish' config to your bleep.yaml:")
    logger.info("  templates:")
    logger.info("    template-publishable:")
    logger.info("      publish:")
    logger.info("        groupId: <your groupId>")
    logger.info("        url: <your project URL>")
    logger.info("        licenses:")
    logger.info("          - name: MIT")
    logger.info("            url: http://opensource.org/licenses/MIT")
    logger.info("        developers:")
    logger.info("          - id: <your id>")
    logger.info("            name: <your name>")
    logger.info("            url: <your URL>")

    Right(())
  }

  private def generatePassphrase(): String = {
    val random = new SecureRandom()
    val chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
    val sb = new StringBuilder(32)
    var i = 0
    while (i < 32) {
      sb.append(chars.charAt(random.nextInt(chars.length)))
      i += 1
    }
    sb.toString()
  }

  private def saveAuthEntry(uri: URI, entry: AuthEntry): Unit =
    BleepConfigOps
      .rewritePersisted(logger, userPaths) { config =>
        val existing = config.authentications.getOrElse(Authentications.empty)
        val updated = existing.copy(configs = existing.configs + (uri -> entry))
        config.copy(authentications = Some(updated))
      }
      .orThrow

  /** Add a named resolver to bleep.yaml if we have build access and it doesn't already exist. */
  private def addResolverToBuild(name: String, repo: Repository): Unit =
    maybeStarted.foreach { started =>
      val resolverName = ResolverName(name)
      val alreadyExists = started.build.resolvers.values.exists(_.name.contains(resolverName))
      if (alreadyExists) {
        logger.info(s"Resolver '$name' already exists in bleep.yaml")
      } else {
        val build = started.build.requireFileBacked("publish setup")
        val updatedFile = build.file.copy(resolvers = model.JsonList(build.file.resolvers.values :+ repo))
        writeYamlLogged(logger, "Added resolver to build", updatedFile, started.buildPaths.bleepYamlFile)
      }
    }
}
