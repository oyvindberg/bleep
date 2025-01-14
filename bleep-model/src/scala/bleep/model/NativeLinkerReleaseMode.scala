package bleep.model

import io.circe.*

sealed abstract class NativeLinkerReleaseMode(val id: String)
object NativeLinkerReleaseMode {
  case object ReleaseFast extends NativeLinkerReleaseMode("release-fast")
  case object ReleaseSize extends NativeLinkerReleaseMode("release-size")
  case object ReleaseFull extends NativeLinkerReleaseMode("release-full")

  val All: List[NativeLinkerReleaseMode] =
    List(ReleaseFast, ReleaseSize, ReleaseFull)

  val AllMap: Map[String, NativeLinkerReleaseMode] =
    All.map(x => x.id -> x).toMap

  implicit val encodeNativeLinkerReleaseMode: Encoder[NativeLinkerReleaseMode] = Encoder.instance { x =>
    Json.obj(("nativeLinkerReleaseMode", Json.fromString(x.id)))
  }

  implicit val decodeNativeLinkerReleaseMode: Decoder[NativeLinkerReleaseMode] = Decoder.instance { h =>
    h.get[String]("nativeLinkerReleaseMode").flatMap { s =>
      AllMap.get(s).toRight(DecodingFailure("NativeLinkerReleaseMode", h.history))
    }
  }
}
