package bleep
package publishing

import bleep.model.PrivateRepoScheme
import coursier.core.Authentication
import ryddig.Logger

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.charset.StandardCharsets
import java.util.Base64

/** Publishes Maven artifacts to a remote repository via HTTP PUT.
  *
  * Works with any Maven-layout repository that accepts PUT uploads: Google Artifact Registry, GitHub Packages, GitLab Package Registry, or any standard Maven
  * repository.
  */
class MavenRemotePublisher(logger: Logger) {

  private val httpClient: HttpClient = HttpClient.newBuilder().build()

  /** Upload all artifacts to the given repository base URI with authentication.
    *
    * @param baseUri
    *   repository root, e.g. `https://europe-north1-maven.pkg.dev/project/repo`
    * @param artifacts
    *   map from relative path (e.g. `com/example/lib/1.0/lib-1.0.jar`) to file contents
    * @param authentication
    *   coursier Authentication containing user/password or headers
    */
  def publish(
      baseUri: URI,
      artifacts: Map[RelPath, Array[Byte]],
      authentication: Authentication
  ): Unit = {
    val total = artifacts.size
    var uploaded = 0
    var failed = 0

    artifacts.foreach { case (relPath, content) =>
      val targetUri = resolveArtifactUri(baseUri, relPath)
      val request = buildRequest(targetUri, content, authentication)

      logger.debug(s"PUT $targetUri (${content.length} bytes)")

      val response = httpClient.send(request, HttpResponse.BodyHandlers.ofString())

      response.statusCode() match {
        case code if code >= 200 && code < 300 =>
          uploaded += 1
          logger.debug(s"  $code OK ($uploaded/$total)")
        case code =>
          failed += 1
          val body = response.body()
          throw new BleepException.Text(
            s"Failed to publish $relPath to $targetUri: HTTP $code\n$body"
          )
      }
    }

    logger.info(s"Published $uploaded artifact(s) to $baseUri")
  }

  private def resolveArtifactUri(baseUri: URI, relPath: RelPath): URI = {
    val base = baseUri.toString.stripSuffix("/")
    URI.create(s"$base/${relPath.asString}")
  }

  private def buildRequest(
      uri: URI,
      content: Array[Byte],
      authentication: Authentication
  ): HttpRequest = {
    val builder = HttpRequest
      .newBuilder()
      .uri(uri)
      .PUT(HttpRequest.BodyPublishers.ofByteArray(content))
      .header("Content-Type", "application/octet-stream")

    // Apply authentication: prefer headers, then basic auth
    if (authentication.httpHeaders.nonEmpty) {
      authentication.httpHeaders.foreach { case (name, value) =>
        builder.header(name, value)
      }
    }

    authentication.passwordOpt match {
      case Some(password) if authentication.user.nonEmpty =>
        val encoded = Base64.getEncoder.encodeToString(
          s"${authentication.user}:$password".getBytes(StandardCharsets.UTF_8)
        )
        builder.header("Authorization", s"Basic $encoded")
      case _ => ()
    }

    builder.build()
  }
}

object MavenRemotePublisher {

  case class ResolvedTarget(httpsUri: URI, authentication: Authentication, uploadChecksums: Boolean)

  /** Resolve the HTTPS base URI, authentication, and capabilities for the target repository.
    *
    *   - Private-repo schemes (`artifactregistry://`, `github://`, `gitlab://`) shell out to the relevant CLI for a token.
    *   - Plain `http://` / `https://` (Artifactory, Sonatype Nexus, any vanilla Maven repo) use static credentials looked up by exact URI match in the
    *     `authentications:` section of the user's bleep config. If no entry matches, the request is sent with no `Authorization` header and the server decides.
    */
  def resolveTarget(
      repoUri: URI,
      credentialProvider: CredentialProvider
  ): ResolvedTarget =
    PrivateRepoScheme.fromUri(repoUri) match {
      case Some(scheme) =>
        val httpsUri = scheme.toHttpsUri(repoUri)
        val auth = credentialProvider.resolve(scheme, repoUri)
        ResolvedTarget(httpsUri, auth, scheme.uploadChecksums)
      case None if repoUri.getScheme == "http" || repoUri.getScheme == "https" =>
        val auth = credentialProvider.staticAuth(repoUri).getOrElse(Authentication(""))
        ResolvedTarget(repoUri, auth, uploadChecksums = true)
      case None =>
        throw new BleepException.Text(
          s"Cannot publish to $repoUri: unrecognized scheme. " +
            s"Supported: http://, https://, ${PrivateRepoScheme.all.map(_.scheme + "://").mkString(", ")}"
        )
    }
}
