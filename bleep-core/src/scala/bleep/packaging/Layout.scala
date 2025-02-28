package bleep
package packaging

import coursier.core.Dependency

trait Layout[F, V] {
  type Self[f, v] <: Layout[f, v]

  def all: Map[F, V]

  def map[FF, VV](f: (F, V) => (FF, VV)): Self[FF, VV]

  final def mapFiles[FF](f: F => FF): Self[FF, V] =
    map { case (k, v) => f(k) -> v }

  final def mapValues[VV](f: (F, V) => VV): Self[F, VV] =
    map { case (k, v) => k -> f(k, v) }
}

final case class MapLayout[F, V](all: Map[F, V]) extends Layout[F, V] {
  override type Self[f, v] = MapLayout[f, v]

  override def map[FF, VV](f: (F, V) => (FF, VV)): MapLayout[FF, VV] =
    MapLayout[FF, VV](all.map { case (k, v) => f(k, v) })
}

final case class IvyLayout[F, V](jarFile: (F, V), sourceFile: (F, V), ivyFile: (F, V), pomFile: (F, V), docFile: (F, V)) extends Layout[F, V] {
  override type Self[f, v] = IvyLayout[f, v]
  override def all: Map[F, V] = List(jarFile, sourceFile, ivyFile, pomFile, docFile).toMap
  override def map[FF, VV](f: (F, V) => (FF, VV)): IvyLayout[FF, VV] =
    this match {
      case IvyLayout((_1k, _1v), (_2k, _2v), (_3k, _3v), (_4k, _4v), (_5k, _5v)) =>
        IvyLayout(f(_1k, _1v), f(_2k, _2v), f(_3k, _3v), f(_4k, _4v), f(_5k, _5v))
    }
}

object IvyLayout {
  def unit(p: Dependency): IvyLayout[RelPath, Unit] =
    apply(p, (), (), (), (), ())

  def apply[T](self: Dependency, jarFile: T, sourceFile: T, ivyFile: T, pomFile: T, docFile: T): IvyLayout[RelPath, T] = {
    val libraryPath = RelPath.of(self.module.organization.value, self.module.name.value, self.version)
    IvyLayout(
      jarFile = libraryPath / "jars" / s"${self.module.name.value}.jar" -> jarFile,
      sourceFile = libraryPath / "srcs" / s"${self.module.name.value}-sources.jar" -> sourceFile,
      ivyFile = libraryPath / "ivys" / "ivy.xml" -> ivyFile,
      pomFile = libraryPath / "poms" / s"${self.module.name.value}.pom" -> pomFile,
      docFile = libraryPath / "docs" / s"${self.module.name.value}-javadoc.jar" -> docFile
    )
  }
}

final case class MavenLayout[F, V](jarFile: (F, V), sourceFile: (F, V), pomFile: (F, V), docFile: (F, V)) extends Layout[F, V] {
  override def all: Map[F, V] = List(jarFile, sourceFile, pomFile, docFile).toMap
  override type Self[f, v] = MavenLayout[f, v]
  override def map[FF, VV](f: (F, V) => (FF, VV)): MavenLayout[FF, VV] =
    this match {
      case MavenLayout((_1k, _1v), (_2k, _2v), (_3k, _3v), (_4k, _4v)) => MavenLayout(f(_1k, _1v), f(_2k, _2v), f(_3k, _3v), f(_4k, _4v))
    }
}

object MavenLayout {
  def apply[T](self: Dependency, jarFile: T, sourceFile: T, pomFile: T, docFile: T): MavenLayout[RelPath, T] = {
    val orgFragment: RelPath =
      self.module.organization.value.split("\\.").foldLeft(RelPath.empty)(_ / _)

    def baseFile(ext: String): RelPath =
      orgFragment / self.module.name.value / self.version / s"${self.module.name.value}-${self.version}$ext"

    MavenLayout(
      jarFile = baseFile(".jar") -> jarFile,
      sourceFile = baseFile("-sources.jar") -> sourceFile,
      pomFile = baseFile(".pom") -> pomFile,
      docFile = baseFile("-javadoc.jar") -> docFile
    )
  }
}
