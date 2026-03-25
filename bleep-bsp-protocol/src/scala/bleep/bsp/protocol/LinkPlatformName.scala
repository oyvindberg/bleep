package bleep.bsp.protocol

import io.circe._

/** Link platform identifier — replaces stringly-typed "Scala.js"/"Scala Native"/etc. */
sealed trait LinkPlatformName {
  def wireValue: String
}

object LinkPlatformName {
  case object ScalaJs extends LinkPlatformName { val wireValue: String = "Scala.js" }
  case object ScalaNative extends LinkPlatformName { val wireValue: String = "Scala Native" }
  case object KotlinJs extends LinkPlatformName { val wireValue: String = "Kotlin/JS" }
  case object KotlinNative extends LinkPlatformName { val wireValue: String = "Kotlin/Native" }
  case object Jvm extends LinkPlatformName { val wireValue: String = "JVM" }

  def fromString(s: String): LinkPlatformName = s match {
    case "Scala.js"      => ScalaJs
    case "Scala Native"  => ScalaNative
    case "Kotlin/JS"     => KotlinJs
    case "Kotlin/Native" => KotlinNative
    case "JVM"           => Jvm
    case other           => throw new IllegalArgumentException(s"Unknown LinkPlatformName: $other")
  }

  implicit val encoder: Encoder[LinkPlatformName] = Encoder.encodeString.contramap(_.wireValue)
  implicit val decoder: Decoder[LinkPlatformName] = Decoder.decodeString.emap { s =>
    scala.util.Try(fromString(s)).toEither.left.map(_.getMessage)
  }
  implicit val codec: Codec[LinkPlatformName] = Codec.from(decoder, encoder)
}
