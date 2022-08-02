package bleep.model

import bleep.internal.EnumCodec
import io.circe.Codec

sealed abstract class LinkerMode(val id: String)

object LinkerMode {
  case object Debug extends LinkerMode("debug")

  case object Release extends LinkerMode("release")

  val All: List[LinkerMode] = List(Debug, Release)
  implicit val linkerModeCodec: Codec[LinkerMode] =
    EnumCodec.codec(All.map(x => x.id -> x).toMap)
}
