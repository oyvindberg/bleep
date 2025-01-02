package bleep.model

import io.circe.*

sealed abstract class NativeBuildTarget(val id: String)
object NativeBuildTarget {
  case object Application extends NativeBuildTarget("application")
  case object LibraryDynamic extends NativeBuildTarget("dynamic")
  case object LibraryStatic extends NativeBuildTarget("static")

  val All: List[String] =
    List(Application.id, LibraryDynamic.id, LibraryStatic.id)

  implicit val encodeNativeBuildTarget: Encoder[NativeBuildTarget] = Encoder.instance {
    case NativeBuildTarget.Application    => Json.obj(("nativeBuildTarget", Json.fromString("application")))
    case NativeBuildTarget.LibraryDynamic => Json.obj(("nativeBuildTarget", Json.fromString("dynamic")))
    case NativeBuildTarget.LibraryStatic  => Json.obj(("nativeBuildTarget", Json.fromString("static")))
  }

  implicit val decodeNativeBuildTarget: Decoder[NativeBuildTarget] = Decoder.instance { h =>
    h.get[String]("nativeBuildTarget").flatMap {
      case "application" => Right(NativeBuildTarget.Application)
      case "dynamic"     => Right(NativeBuildTarget.LibraryDynamic)
      case "static"      => Right(NativeBuildTarget.LibraryStatic)
      case _             => Left(DecodingFailure("NativeBuildTarget", h.history))
    }
  }
}
