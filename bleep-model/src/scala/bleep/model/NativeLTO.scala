package bleep.model

import io.circe.*

sealed abstract class NativeLTO(val id: String)
object NativeLTO {
  case object None extends NativeLTO("none")
  case object Thin extends NativeLTO("thin")
  case object Full extends NativeLTO("full")

  val All: List[String] =
    List(None.id, Thin.id, Full.id)

  implicit val encodeNativeLTO: Encoder[NativeLTO] = Encoder.instance {
    case NativeLTO.None => Json.obj(("nativeLTO", Json.fromString("none")))
    case NativeLTO.Thin => Json.obj(("nativeLTO", Json.fromString("thin")))
    case NativeLTO.Full => Json.obj(("nativeLTO", Json.fromString("full")))
  }

  implicit val decodeNativeLTO: Decoder[NativeLTO] = Decoder.instance { h =>
    h.get[String]("nativeBuildTarget").flatMap {
      case "none" => Right(NativeLTO.None)
      case "thin" => Right(NativeLTO.Thin)
      case "full" => Right(NativeLTO.Full)
      case _      => Left(DecodingFailure("NativeLTO", h.history))
    }
  }
}
