package bleep.model

import bleep.internal.EnumCodec
import io.circe.Codec

sealed abstract class CompileOrder(val id: String)

object CompileOrder {
  case object Mixed extends CompileOrder("mixed")

  case object JavaThenScala extends CompileOrder("java->scala")

  case object ScalaThenJava extends CompileOrder("scala->java")

  final val All: Seq[CompileOrder] = List(Mixed, JavaThenScala, ScalaThenJava)

  implicit val codec: Codec[CompileOrder] =
    EnumCodec.codec(All.map(x => x.id -> x).toMap)
}
