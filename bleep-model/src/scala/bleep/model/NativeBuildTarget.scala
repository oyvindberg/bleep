package bleep.model

import io.circe.*

sealed abstract class NativeBuildTarget(val id: String)
object NativeBuildTarget {
  case object Application extends NativeBuildTarget("application")
  case object LibraryDynamic extends NativeBuildTarget("library-dynamic")
  case object LibraryStatic extends NativeBuildTarget("library-static")

  val All: List[NativeBuildTarget] =
    List(Application, LibraryDynamic, LibraryStatic)

  val AllMap: Map[String, NativeBuildTarget] =
    All.map(x => x.id -> x).toMap

  implicit val encodeNativeBuildTarget: Encoder[NativeBuildTarget] = Encoder.instance { x =>
    Json.obj(("nativeBuildTarget", Json.fromString(x.id)))
  }

  implicit val decodeNativeBuildTarget: Decoder[NativeBuildTarget] = Decoder.instance { h =>
    h.get[String]("nativeBuildTarget").flatMap { s =>
      AllMap.get(s).toRight(DecodingFailure("NativeBuildTarget", h.history))
    }
  }
}
