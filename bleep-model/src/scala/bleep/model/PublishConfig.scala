package bleep.model

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

/** Publishing configuration for a project. All fields are mergeable via templates using SetLike semantics.
  *
  * A project is publishable if it has `publish` config with `enabled` not set to `false`. Set `enabled: false` to opt out of publishing while still inheriting
  * other publish fields from a template (e.g. for a scripts module).
  */
case class PublishConfig(
    enabled: Option[Boolean],
    groupId: Option[String],
    description: Option[String],
    url: Option[String],
    organization: Option[String],
    developers: JsonSet[PublishConfig.Developer],
    licenses: JsonSet[PublishConfig.License],
    sonatypeProfileName: Option[String],
    sonatypeCredentialHost: Option[String]
) extends SetLike[PublishConfig] {

  /** Whether this project should actually be published. True unless explicitly disabled. */
  def isEnabled: Boolean = enabled.getOrElse(true)

  override def intersect(other: PublishConfig): PublishConfig =
    PublishConfig(
      enabled = if (enabled == other.enabled) enabled else None,
      groupId = if (groupId == other.groupId) groupId else None,
      description = if (description == other.description) description else None,
      url = if (url == other.url) url else None,
      organization = if (organization == other.organization) organization else None,
      developers = developers.intersect(other.developers),
      licenses = licenses.intersect(other.licenses),
      sonatypeProfileName = if (sonatypeProfileName == other.sonatypeProfileName) sonatypeProfileName else None,
      sonatypeCredentialHost = if (sonatypeCredentialHost == other.sonatypeCredentialHost) sonatypeCredentialHost else None
    )

  override def removeAll(other: PublishConfig): PublishConfig =
    PublishConfig(
      enabled = if (enabled == other.enabled) None else enabled,
      groupId = if (groupId == other.groupId) None else groupId,
      description = if (description == other.description) None else description,
      url = if (url == other.url) None else url,
      organization = if (organization == other.organization) None else organization,
      developers = developers.removeAll(other.developers),
      licenses = licenses.removeAll(other.licenses),
      sonatypeProfileName = if (sonatypeProfileName == other.sonatypeProfileName) None else sonatypeProfileName,
      sonatypeCredentialHost = if (sonatypeCredentialHost == other.sonatypeCredentialHost) None else sonatypeCredentialHost
    )

  override def union(other: PublishConfig): PublishConfig =
    PublishConfig(
      enabled = enabled.orElse(other.enabled),
      groupId = groupId.orElse(other.groupId),
      description = description.orElse(other.description),
      url = url.orElse(other.url),
      organization = organization.orElse(other.organization),
      developers = developers.union(other.developers),
      licenses = licenses.union(other.licenses),
      sonatypeProfileName = sonatypeProfileName.orElse(other.sonatypeProfileName),
      sonatypeCredentialHost = sonatypeCredentialHost.orElse(other.sonatypeCredentialHost)
    )

  override def isEmpty: Boolean =
    enabled.isEmpty && groupId.isEmpty && description.isEmpty && url.isEmpty &&
      organization.isEmpty && developers.isEmpty && licenses.isEmpty &&
      sonatypeProfileName.isEmpty && sonatypeCredentialHost.isEmpty
}

object PublishConfig {
  case class Developer(id: String, name: String, url: String)
  object Developer {
    implicit val decoder: Decoder[Developer] = deriveDecoder
    implicit val encoder: Encoder[Developer] = deriveEncoder
    implicit val ordering: Ordering[Developer] = Ordering.by(_.id)
  }

  case class License(name: String, url: Option[String], distribution: Option[String])
  object License {
    implicit val decoder: Decoder[License] = deriveDecoder
    implicit val encoder: Encoder[License] = deriveEncoder
    implicit val ordering: Ordering[License] = Ordering.by(_.name)
  }

  implicit val decoder: Decoder[PublishConfig] = deriveDecoder
  implicit val encoder: Encoder[PublishConfig] = deriveEncoder
}
