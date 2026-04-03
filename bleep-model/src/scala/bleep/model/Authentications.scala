package bleep
package model

import coursier.core.Authentication
import io.circe.*

import java.net.URI

/** Authentication configuration for repositories.
  *
  * Entries keyed by a private repo scheme URI (artifactregistry://, github://, gitlab://) are decoded as [[AuthEntry.PrivateRepo]] with provider-specific
  * fields. All other entries are decoded as [[AuthEntry.Static]] with standard HTTP credentials (user/password/headers). The URI key supports prefix matching
  * for private repos -- configure `artifactregistry://host/project` to cover all repos under that path.
  */
final case class Authentications(configs: Map[URI, AuthEntry])

object Authentications {
  val empty: Authentications = Authentications(Map.empty)

  implicit val keyEncoder: KeyEncoder[URI] = KeyEncoder.encodeKeyString.contramap(_.toString)
  implicit val keyDecoder: KeyDecoder[URI] = KeyDecoder.decodeKeyString.map(URI.create)

  private val authenticationCodec: Codec[Authentication] =
    Codec.forProduct7[Authentication, Option[String], Option[String], Option[Map[String, String]], Option[Boolean], Option[String], Option[Boolean], Option[
      Boolean
    ]](
      "user",
      "password",
      "headers",
      "optional",
      "realm",
      "httpsOnly",
      "passOnRedirect"
    )((user, pass, headers, optional, realm, httpsOnly, redirect) =>
      Authentication(
        user.getOrElse(""),
        pass,
        headers.map(_.toList).getOrElse(Nil),
        optional.getOrElse(false),
        realm,
        httpsOnly.getOrElse(true),
        redirect.getOrElse(true)
      )
    )(x => (Some(x.user), x.passwordOpt, Some(x.httpHeaders.toMap), Some(x.optional), x.realmOpt, Some(x.httpsOnly), Some(x.passOnRedirect)))

  private val privateRepoCodec: Codec[AuthEntry.PrivateRepo] =
    Codec.forProduct1[AuthEntry.PrivateRepo, Option[String]]("account")(account => AuthEntry.PrivateRepo(account))(x => x.account)

  implicit val authEntryCodec: Codec[AuthEntry] = Codec.from(
    // Decoding: can't know scheme here, so try PrivateRepo first (has optional account field),
    // then Static. The top-level codec uses URI key scheme to pick the right one.
    privateRepoCodec.map(x => x: AuthEntry).or(authenticationCodec.map(a => AuthEntry.Static(a): AuthEntry)),
    Encoder.instance {
      case AuthEntry.Static(auth)    => authenticationCodec.apply(auth)
      case pr: AuthEntry.PrivateRepo => privateRepoCodec.apply(pr)
    }
  )

  /** Keyed codec that dispatches on URI scheme: private repo schemes use [[AuthEntry.PrivateRepo]], everything else uses [[AuthEntry.Static]]. */
  private val keyAwareEntryDecoder: KeyDecoder[URI] => Decoder[Map[URI, AuthEntry]] =
    _ =>
      Decoder.instance { c =>
        c.as[Map[String, Json]].flatMap { rawMap =>
          val results = rawMap.toList.map { case (uriStr, valueJson) =>
            val uri = URI.create(uriStr)
            val entryResult =
              if (PrivateRepoScheme.isPrivateScheme(uri))
                valueJson.as[AuthEntry.PrivateRepo](privateRepoCodec).map(x => x: AuthEntry)
              else
                valueJson.as[Authentication](authenticationCodec).map(a => AuthEntry.Static(a): AuthEntry)
            entryResult.map(entry => uri -> entry)
          }
          results.foldLeft(Right(Map.empty[URI, AuthEntry]): Either[DecodingFailure, Map[URI, AuthEntry]]) { case (acc, entry) =>
            for {
              m <- acc
              e <- entry
            } yield m + e
          }
        }
      }

  private val mapEncoder: Encoder[Map[URI, AuthEntry]] =
    Encoder.instance { m =>
      val fields = m.toList.map { case (uri, entry) =>
        val valueJson = entry match {
          case AuthEntry.Static(auth)    => authenticationCodec.apply(auth)
          case pr: AuthEntry.PrivateRepo => privateRepoCodec.apply(pr)
        }
        uri.toString -> valueJson
      }
      Json.obj(fields: _*)
    }

  implicit val codec: Codec[Authentications] =
    Codec.from(
      Decoder.instance { c =>
        c.as[Option[Json]].flatMap {
          case None => Right(empty)
          case Some(json) =>
            keyAwareEntryDecoder(keyDecoder).decodeJson(json).map(m => Authentications(m))
        }
      },
      Encoder.instance {
        case auth if auth.configs.isEmpty => Json.Null
        case auth                         => mapEncoder.apply(auth.configs)
      }
    )
}
