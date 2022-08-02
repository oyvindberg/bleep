package bleep.model

import bleep.internal.EnumCodec
import io.circe.Codec

sealed abstract class PlatformId(val value: String)

object PlatformId {
  case object Jvm extends PlatformId("jvm")

  case object Js extends PlatformId("js")

  case object Native extends PlatformId("native")

  val All = List[PlatformId](Jvm, Js, Native)

  def fromName(str: String): Option[PlatformId] = All.find(_.value == str)

  implicit val ordering: Ordering[PlatformId] = Ordering.by(All.indexOf)
  implicit val codec: Codec[PlatformId] = EnumCodec.codec(All.map(x => (x.value, x)).toMap)
}
