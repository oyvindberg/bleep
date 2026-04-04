package bleep
package model

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}

import java.net.URI

/** Remote build cache configuration. Stored in `bleep.yaml` (shared by all contributors).
  *
  * Credentials are stored separately in `~/.config/bleep/config.yaml` or via `BLEEP_REMOTE_CACHE_S3_ACCESS_KEY_ID` / `BLEEP_REMOTE_CACHE_S3_SECRET_ACCESS_KEY` environment variables.
  */
case class RemoteCacheConfig(
    uri: URI,
    region: Option[String]
)

object RemoteCacheConfig {
  implicit val uriDecoder: Decoder[URI] = Decoder[String].map(URI.create)
  implicit val uriEncoder: Encoder[URI] = Encoder[String].contramap(_.toString)

  implicit val decoder: Decoder[RemoteCacheConfig] = deriveDecoder
  implicit val encoder: Encoder[RemoteCacheConfig] = deriveEncoder
}

/** Remote cache credentials. Stored in user config (`~/.config/bleep/config.yaml`). Falls back to `BLEEP_REMOTE_CACHE_S3_ACCESS_KEY_ID` / `BLEEP_REMOTE_CACHE_S3_SECRET_ACCESS_KEY` environment
  * variables.
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
