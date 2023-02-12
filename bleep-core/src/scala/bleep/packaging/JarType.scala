package bleep.packaging

sealed trait JarType

object JarType {
  case object Jar extends JarType

  case object SourcesJar extends JarType

  case object DocsJar extends JarType
}
