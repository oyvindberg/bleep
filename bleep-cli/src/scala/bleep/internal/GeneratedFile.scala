package bleep.internal

import bleep.RelPath
import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec

/** This structure exists to pick up existing generated sources/resources in a build.
  *
  * The files are made relative to build directory, and will be placed inside the corresponding folder for generated sources/resources for the given project.
  *
  * This is meant to back the generated script which will put
  */
case class GeneratedFile(isResource: Boolean, contents: String, toRelPath: RelPath)

object GeneratedFile {
  implicit val codec: Codec.AsObject[GeneratedFile] = deriveCodec[GeneratedFile]
}
