package bleep.model

import coursier.core.Authentication

/** An authentication entry in bleep config.
  *
  * [[Static]] wraps a standard coursier [[Authentication]] with user/password/headers. [[PrivateRepo]] holds provider-specific config (e.g. GCP account) for
  * repositories using a private scheme (artifactregistry://, github://, gitlab://).
  */
sealed trait AuthEntry

object AuthEntry {

  /** Standard HTTP credentials (user/password/headers). Used for plain https:// repositories. */
  final case class Static(authentication: Authentication) extends AuthEntry

  /** Provider-specific config for private repository schemes. The actual token is acquired at resolution time by shelling out to CLI tools (gcloud, gh, glab).
    *
    * @param account
    *   GCP account email for Artifact Registry. None means use default gcloud account.
    */
  final case class PrivateRepo(account: Option[String]) extends AuthEntry
}
