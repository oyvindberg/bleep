package bleep.model

import io.circe.*

sealed abstract class NativeLTO(val id: String)
object NativeLTO {
  case object None extends NativeLTO("none")
  case object Thin extends NativeLTO("thin")
  case object Full extends NativeLTO("full")

  val All: List[NativeLTO] =
    List(None, Thin, Full)

  val AllMap: Map[String, NativeLTO] =
    All.map(x => x.id -> x).toMap

  implicit val encodeNativeLTO: Encoder[NativeLTO] = Encoder.instance { x =>
    Json.obj(("nativeLTO", Json.fromString(x.id)))
  }

  implicit val decodeNativeLTO: Decoder[NativeLTO] = Decoder.instance { h =>
    h.get[String]("nativeLTO").flatMap { s =>
      AllMap.get(s).toRight(DecodingFailure("NativeLTO", h.history))
    }
  }
}
