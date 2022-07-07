package bleep.internal

import bleep.RelPath

/** This structure exists to pick up existing generated sources/resources in a build.
  *
  * The files are made relative to build directory, and will be placed inside the corresponding folder for generated sources/resources for the given project.
  *
  * This is meant to back the generated script which will put
  */
case class GeneratedFile(isResource: Boolean, contents: String, toRelPath: RelPath)
