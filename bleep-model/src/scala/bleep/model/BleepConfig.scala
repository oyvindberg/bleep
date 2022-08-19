package bleep.model

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

case class BleepConfig(
    compileServerMode: CompileServerMode,
    compileServerJvm: Option[Jvm],
    authentications: Option[Authentications]
)

object BleepConfig {
  val default = BleepConfig(
    compileServerMode = CompileServerMode.Shared,
    compileServerJvm = None,
    authentications = None
  )

  implicit val decoder: Decoder[BleepConfig] = deriveDecoder
  implicit val encoder: Encoder[BleepConfig] = deriveEncoder
}
