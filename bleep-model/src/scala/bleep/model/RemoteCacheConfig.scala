package bleep
package model

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}

import java.net.URI

/** A `uri:` value as written in `bleep.yaml` — possibly still carrying a literal `${HOME_DIR}` token (or any other [[Replacements.known]] token), since `uri:`
  * is checked in and cannot bake in one person's absolute home directory, but a `file://` cache (built for git worktrees, see `docs/usage/remote-cache.mdx`) is
  * inherently per-machine.
  *
  * Deliberately a distinct type from `java.net.URI`, not a type alias or a bare `String`: a token-bearing value cannot even construct a `URI` — `URI.create`
  * rejects `{`/`}` outright — so the type itself marks "not resolved yet" instead of leaving it a convention a caller could forget. [[resolve]] takes the
  * already-built [[Replacements]] to apply — the same token-substitution machinery every other `${TOKEN}` expansion in this codebase goes through
  * (`ResolveProjects`, `CoursierResolver`'s repository URIs, ...) — rather than hand-rolling its own, so it stays a trivial, generic one-liner with no
  * JVM-environment dependency of its own: the caller decides what "the home directory" (or any other token) resolves to.
  */
case class TemplatedUri(raw: String) {
  def resolve(replacements: Replacements): URI = URI.create(replacements.fill.string(raw))
}

object TemplatedUri {
  implicit val decoder: Decoder[TemplatedUri] = Decoder[String].map(TemplatedUri(_))
  implicit val encoder: Encoder[TemplatedUri] = Encoder[String].contramap(_.raw)
}

/** Remote build cache configuration. Stored in `bleep.yaml` (shared by all contributors).
  *
  * Credentials are stored separately in `~/.config/bleep/config.yaml` or via `BLEEP_REMOTE_CACHE_S3_ACCESS_KEY_ID` / `BLEEP_REMOTE_CACHE_S3_SECRET_ACCESS_KEY`
  * environment variables.
  */
case class RemoteCacheConfig(
    uri: TemplatedUri,
    region: Option[String]
)

object RemoteCacheConfig {
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
