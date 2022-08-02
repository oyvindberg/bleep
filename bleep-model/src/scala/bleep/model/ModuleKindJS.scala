package bleep.model

import bleep.internal.EnumCodec
import io.circe.Codec

sealed abstract class ModuleKindJS(val id: String)

object ModuleKindJS {
  case object NoModule extends ModuleKindJS("none")

  case object CommonJSModule extends ModuleKindJS("commonjs")

  case object ESModule extends ModuleKindJS("esmodule")

  val All: Seq[ModuleKindJS] = List(NoModule, CommonJSModule, ESModule)
  implicit val codec: Codec[ModuleKindJS] =
    EnumCodec.codec(All.map(x => x.id -> x).toMap)
}
