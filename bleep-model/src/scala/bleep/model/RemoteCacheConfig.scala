package bleep
package model

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}

import java.net.URI

/** Remote build cache configuration. Stored in `bleep.yaml` (shared by all contributors).
  *
  * Credentials are stored separately in `~/.config/bleep/config.yaml` or via `BLEEP_REMOTE_CACHE_S3_ACCESS_KEY_ID` / `BLEEP_REMOTE_CACHE_S3_SECRET_ACCESS_KEY`
  * environment variables.
  */
case class RemoteCacheConfig(
    uri: URI,
    region: Option[String]
)

object RemoteCacheConfig {

  /** `user.home` as a `file://` URI's path component. Already valid as-is on macOS/Linux (`/Users/x`), but on Windows it's `C:\Users\x` — backslashes, which
    * `URI.create` rejects outright, and no leading slash before the drive letter, which the path component needs (`file:///C:/Users/x`, matching what
    * `Path.toUri()` itself produces for a Windows path). Normalized here, on the raw string, rather than via `Path.toUri()` — that queries the filesystem to
    * decide whether to append a trailing slash, which this should not depend on. `private[model]` so the test asserting the substitution's exact result can
    * compute the same expected value instead of hardcoding the raw, pre-Windows-fix form. `private[bleep]`, not `private[model]`: the test lives in `bleep`,
    * one package up from `bleep.model`.
    */
  private[bleep] def homeDirUriPath: String = {
    val home = System.getProperty("user.home").replace('\\', '/')
    if (home.startsWith("/")) home else s"/$home"
  }

  /** `uri:` is checked in to `bleep.yaml`, shared by every contributor, so it cannot bake in one person's absolute home directory — but a `file://` cache
    * (built for git worktrees, see `docs/usage/remote-cache.mdx`) is inherently per-machine. `${HOME_DIR}` lets the shared config name a path under whoever's
    * home it happens to run in: `file://${HOME_DIR}/.cache/bleep-remote-cache`.
    *
    * Plain string substitution before `URI.create`, not [[Replacements.fill]]: that needs a build directory to expand `${BUILD_DIR}`-style tokens, which this
    * decoder — run before a build is even loaded — does not have. `${HOME_DIR}` alone needs nothing but a JVM system property, so it is handled directly.
    * `URI.create` is also stricter about `{`/`}` than this scheme tolerates, so the substitution must happen first, on the raw string, not after parsing.
    */
  implicit val uriDecoder: Decoder[URI] = Decoder[String].map { raw =>
    URI.create(raw.replace(Replacements.known.HomeDir, homeDirUriPath))
  }
  implicit val uriEncoder: Encoder[URI] = Encoder[String].contramap(_.toString)

  implicit val decoder: Decoder[RemoteCacheConfig] = deriveDecoder
  implicit val encoder: Encoder[RemoteCacheConfig] = deriveEncoder
}

/** Remote cache credentials. Stored in user config (`~/.config/bleep/config.yaml`). Falls back to `BLEEP_REMOTE_CACHE_S3_ACCESS_KEY_ID` /
  * `BLEEP_REMOTE_CACHE_S3_SECRET_ACCESS_KEY` environment variables.
  */
case class RemoteCacheCredentials(
    accessKeyId: String,
    secretAccessKey: String
)

object RemoteCacheCredentials {
  implicit val decoder: Decoder[RemoteCacheCredentials] = deriveDecoder
  implicit val encoder: Encoder[RemoteCacheCredentials] = deriveEncoder

  def fromEnv(): Option[RemoteCacheCredentials] =
    for {
      key <- sys.env.get("BLEEP_REMOTE_CACHE_S3_ACCESS_KEY_ID")
      secret <- sys.env.get("BLEEP_REMOTE_CACHE_S3_SECRET_ACCESS_KEY")
    } yield RemoteCacheCredentials(key, secret)
}
