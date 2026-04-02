package bleep.model

import java.net.URI

sealed trait PrivateRepoScheme {
  val scheme: String
  val cliCommand: String
  val displayName: String
  val installUrl: String

  /** Whether bleep should generate and upload MD5/SHA1 checksums. GitLab computes them server-side and rejects uploaded checksum files. */
  val uploadChecksums: Boolean

  def toHttpsUri(originalUri: URI): URI
}

object PrivateRepoScheme {
  case object ArtifactRegistry extends PrivateRepoScheme {
    val scheme: String = "artifactregistry"
    val cliCommand: String = "gcloud"
    val displayName: String = "Google Artifact Registry"
    val installUrl: String = "https://cloud.google.com/sdk/install"
    val uploadChecksums: Boolean = true

    def toHttpsUri(originalUri: URI): URI =
      new URI("https", originalUri.getAuthority, originalUri.getPath, originalUri.getQuery, originalUri.getFragment)
  }

  case object GitHub extends PrivateRepoScheme {
    val scheme: String = "github"
    val cliCommand: String = "gh"
    val displayName: String = "GitHub Packages"
    val installUrl: String = "https://cli.github.com/"
    val uploadChecksums: Boolean = true

    def toHttpsUri(originalUri: URI): URI = {
      // github://owner/repo -> https://maven.pkg.github.com/owner/repo
      val path = Option(originalUri.getAuthority).getOrElse("") + Option(originalUri.getPath).getOrElse("")
      URI.create(s"https://maven.pkg.github.com/$path")
    }
  }

  case object GitLab extends PrivateRepoScheme {
    val scheme: String = "gitlab"
    val cliCommand: String = "glab"
    val displayName: String = "GitLab Package Registry"
    val installUrl: String = "https://gitlab.com/gitlab-org/cli"
    val uploadChecksums: Boolean = false

    def toHttpsUri(originalUri: URI): URI =
      new URI("https", originalUri.getAuthority, originalUri.getPath, originalUri.getQuery, originalUri.getFragment)
  }

  val all: List[PrivateRepoScheme] = List(ArtifactRegistry, GitHub, GitLab)

  private val byScheme: Map[String, PrivateRepoScheme] =
    all.map(s => s.scheme -> s).toMap

  def fromUri(uri: URI): Option[PrivateRepoScheme] =
    Option(uri.getScheme).flatMap(byScheme.get)

  def isPrivateScheme(uri: URI): Boolean =
    fromUri(uri).isDefined
}
