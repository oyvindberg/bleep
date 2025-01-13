package bleep.model

import io.circe.*

sealed abstract class NativeBuildTarget(val id: String)
object NativeBuildTarget {
  case object Application extends NativeBuildTarget("application")
  case object LibraryDynamic extends NativeBuildTarget("LibraryDynamic")
  case object LibraryStatic extends NativeBuildTarget("libraryStatic")

  val All: List[String] =
    List(Application.id, LibraryDynamic.id, LibraryStatic.id)

  implicit val encodeNativeBuildTarget: Encoder[NativeBuildTarget] = Encoder.instance {
    case NativeBuildTarget.Application    => Json.obj(("nativeBuildTarget", Json.fromString("application")))
    case NativeBuildTarget.LibraryDynamic => Json.obj(("nativeBuildTarget", Json.fromString("libraryDynamic")))
    case NativeBuildTarget.LibraryStatic  => Json.obj(("nativeBuildTarget", Json.fromString("libraryStatic")))
  }

  implicit val decodeNativeBuildTarget: Decoder[NativeBuildTarget] = Decoder.instance { h =>
    h.get[String]("nativeBuildTarget").flatMap {
      case "application"    => Right(NativeBuildTarget.Application)
      case "libraryDynamic" => Right(NativeBuildTarget.LibraryDynamic)
      case "libraryStatic"  => Right(NativeBuildTarget.LibraryStatic)
      case _                => Left(DecodingFailure("NativeBuildTarget", h.history))
    }
  }
}
