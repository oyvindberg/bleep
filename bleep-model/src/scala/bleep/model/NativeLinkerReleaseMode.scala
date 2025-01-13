package bleep.model

import io.circe.*

sealed abstract class NativeLinkerReleaseMode(val id: String)
object NativeLinkerReleaseMode {
  case object ReleaseFast extends NativeLinkerReleaseMode("release-fast")
  case object ReleaseSize extends NativeLinkerReleaseMode("release-size")
  case object ReleaseFull extends NativeLinkerReleaseMode("release-full")

  val All: List[String] =
    List(ReleaseFast.id, ReleaseSize.id, ReleaseFull.id)

  implicit val encodeNativeLinkerReleaseMode: Encoder[NativeLinkerReleaseMode] = Encoder.instance {
    case NativeLinkerReleaseMode.ReleaseFast => Json.obj(("nativeReleaseMode", Json.fromString("release-fast")))
    case NativeLinkerReleaseMode.ReleaseSize => Json.obj(("nativeReleaseMode", Json.fromString("release-size")))
    case NativeLinkerReleaseMode.ReleaseFull => Json.obj(("nativeReleaseMode", Json.fromString("release-full")))
  }

  implicit val decodeNativeLinkerReleaseMode: Decoder[NativeLinkerReleaseMode] = Decoder.instance { h =>
    h.get[String]("nativeBuildTarget").flatMap {
      case "release-fast" => Right(NativeLinkerReleaseMode.ReleaseFast)
      case "release-size" => Right(NativeLinkerReleaseMode.ReleaseSize)
      case "release-full" => Right(NativeLinkerReleaseMode.ReleaseFull)
      case _              => Left(DecodingFailure("NativeLinkerReleaseMode", h.history))
    }
  }
}
