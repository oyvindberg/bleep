package bleep
package model

import coursier.core.Authentication
import io.circe.*

import java.net.URI

final case class Authentications(configs: Map[URI, Authentication])

object Authentications {
  val empty: Authentications = Authentications(Map.empty)

  implicit val authenticationCodec: Codec[Authentication] =
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

  implicit val keyEncoder: KeyEncoder[URI] = KeyEncoder.encodeKeyString.contramap(_.toString)
  implicit val keyDecoder: KeyDecoder[URI] = KeyDecoder.decodeKeyString.map(URI.create)

  implicit val codec: Codec[Authentications] =
    Codec
      .from(Decoder[Option[Map[URI, Authentication]]], Encoder[Option[Map[URI, Authentication]]])
      .iemap {
        case None    => Right(empty)
        case Some(m) => Right(Authentications(m))
      } {
        case auth if auth.configs.isEmpty => None
        case auth                         => Some(auth.configs)
      }
}
