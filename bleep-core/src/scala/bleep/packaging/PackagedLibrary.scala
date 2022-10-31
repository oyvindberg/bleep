package bleep
package packaging

import coursier.core.Dependency

case class PackagedLibrary(asDependency: Dependency, files: Layout[RelPath, Array[Byte]])
