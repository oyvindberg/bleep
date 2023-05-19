package bleep.internal

import coursier.core.*
import io.circe.*

import java.io.File
import java.net.URI
import java.nio.file.Path
import scala.util.Try

object codecs {
  implicit val codecFile: Codec[File] = Codec.from(Decoder[String].emapTry(str => Try(new File(str))), Encoder[String].contramap(_.toString))
  implicit val codecURI: Codec[URI] = Codec.from(Decoder[String].emapTry(str => Try(URI.create(str))), Encoder[String].contramap(_.toString))
  implicit val codecPath: Codec[Path] = Codec.from(Decoder[String].emapTry(str => Try(Path.of(str))), Encoder[String].contramap(_.toString))
  implicit val codecKeyEncoder: KeyEncoder[Path] = KeyEncoder[String].contramap(_.toString)
  implicit val codecKeyDecoder: KeyDecoder[Path] = KeyDecoder[String].map(Path.of(_))

  implicit val codecOrganization: Codec[Organization] = Codec.from(Decoder[String].map(Organization.apply), Encoder[String].contramap(_.value))
  implicit val keyDecoderOrganization: KeyDecoder[Organization] = KeyDecoder.decodeKeyString.map(Organization.apply)
  implicit val keyEncoderOrganization: KeyEncoder[Organization] = KeyEncoder.encodeKeyString.contramap(_.value)
  implicit val codecModuleName: Codec[ModuleName] = Codec.from(Decoder[String].map(ModuleName.apply), Encoder[String].contramap(_.value))
  implicit val codecType: Codec[Type] = Codec.from(Decoder[String].map(Type.apply), Encoder[String].contramap(_.value))
  implicit val codecExtension: Codec[Extension] = Codec.from(Decoder[String].map(Extension.apply), Encoder[String].contramap(_.value))
  implicit val codecClassifier: Codec[Classifier] = Codec.from(Decoder[String].map(Classifier.apply), Encoder[String].contramap(_.value))
  implicit val codecConfiguration: Codec[Configuration] = Codec.from(Decoder[String].map(Configuration.apply), Encoder[String].contramap(_.value))
}
