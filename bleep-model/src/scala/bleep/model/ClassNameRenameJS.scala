package bleep.model

import io.circe.*
import io.circe.generic.semiauto.deriveCodec

/** One step of Scala.js's `RuntimeClassNameMapper`: what `getClass.getName` reports at run time for a class whose name matches `regex` (Java syntax), with the
  * match replaced by `replacement` (`$1` refers to a group). `RuntimeClassNameMapper.regexReplace`.
  */
case class ClassNameRenameJS(regex: String, replacement: String)

object ClassNameRenameJS {
  implicit val codec: Codec.AsObject[ClassNameRenameJS] = deriveCodec
}
