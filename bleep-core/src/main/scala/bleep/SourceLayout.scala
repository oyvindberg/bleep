package bleep

import bleep.internal.EnumCodec

sealed abstract class SourceLayout(val id: String) {
  protected def scalaSources(scalaVersion: Versions.Scala, scope: String): List[RelPath]
  protected def scalaResources(scalaVersion: Versions.Scala, scope: String): List[RelPath]

  final def sources(maybeScalaVersion: Option[Versions.Scala], maybeScope: Option[String]): List[RelPath] = {
    val scope = maybeScope.getOrElse("")
    maybeScalaVersion match {
      case Some(scalaVersion) => scalaSources(scalaVersion, scope)
      case None               => List(RelPath.force(s"src/$scope/java"))
    }
  }

  final def resources(maybeScalaVersion: Option[Versions.Scala], maybeScope: Option[String]): List[RelPath] = {
    val scope = maybeScope.getOrElse("")
    maybeScalaVersion match {
      case Some(scalaVersion) => scalaResources(scalaVersion, scope)
      case None               => List(RelPath.force(s"src/$scope/resources"))
    }
  }
}

object SourceLayout extends EnumCodec[SourceLayout] {
  override val All: Map[String, SourceLayout] =
    List(CrossPure, CrossFull, Normal, Java).map(x => x.id -> x).toMap

  case object Java extends SourceLayout("java") {
    override protected def scalaSources(scalaVersion: Versions.Scala, scope: String): List[RelPath] =
      List(RelPath.force(s"src/$scope/java"))
    override protected def scalaResources(scalaVersion: Versions.Scala, scope: String): List[RelPath] =
      List(RelPath.force(s"src/$scope/resources"))
  }

  case object Normal extends SourceLayout("normal") {
    override protected def scalaSources(scalaVersion: Versions.Scala, scope: String): List[RelPath] =
      List(
        RelPath.force(s"src/$scope/scala"),
        RelPath.force(s"src/$scope/java"),
        RelPath.force(s"src/$scope/scala-${scalaVersion.binVersion}"),
        RelPath.force(s"src/$scope/scala-${scalaVersion.epoch}"),
        RelPath.force(s"target/scala-${scalaVersion.binVersion}/src_managed/$scope")
      )

    override protected def scalaResources(scalaVersion: Versions.Scala, scope: String): List[RelPath] =
      List(
        RelPath.force(s"target/scala-${scalaVersion.binVersion}/resource_managed/$scope"),
        RelPath.force(s"src/$scope/resources")
      )
  }

  case object CrossPure extends SourceLayout("cross-pure") {
    override protected def scalaSources(scalaVersion: Versions.Scala, scope: String) =
      List(
        RelPath.force(s"src/$scope/scala"),
        RelPath.force(s"src/$scope/scala-${scalaVersion.binVersion}"),
        RelPath.force(s"src/$scope/scala-${scalaVersion.epoch}"),
        RelPath.force(s"src/$scope/java"),
        RelPath.force(s"../src/$scope/scala-${scalaVersion.binVersion}"),
        RelPath.force(s"../src/$scope/scala-${scalaVersion.epoch}"),
        RelPath.force(s"../src/$scope/scala"),
        RelPath.force(s"target/scala-${scalaVersion.binVersion}/src_managed/$scope")
      )

    override protected def scalaResources(scalaVersion: Versions.Scala, scope: String): List[RelPath] =
      List(
        RelPath.force(s"src/$scope/resources"),
        RelPath.force(s"../src/$scope/resources"),
        RelPath.force(s"target/scala-${scalaVersion.binVersion}/resource_managed/$scope")
      )
  }

  case object CrossFull extends SourceLayout("cross-full") {
    override protected def scalaSources(scalaVersion: Versions.Scala, scope: String) =
      List(
        RelPath.force(s"src/$scope/scala"),
        RelPath.force(s"src/$scope/scala-${scalaVersion.binVersion}"),
        RelPath.force(s"src/$scope/scala-${scalaVersion.epoch}"),
        RelPath.force(s"src/$scope/java"),
        RelPath.force(s"../shared/src/$scope/scala-${scalaVersion.binVersion}"),
        RelPath.force(s"../shared/src/$scope/scala-${scalaVersion.epoch}"),
        RelPath.force(s"../shared/src/$scope/scala"),
        RelPath.force(s"target/scala-${scalaVersion.binVersion}/src_managed/$scope")
      )

    override protected def scalaResources(scalaVersion: Versions.Scala, scope: String): List[RelPath] =
      List(
        RelPath.force(s"src/$scope/resources"),
        RelPath.force(s"../shared/src/$scope/resources"),
        RelPath.force(s"target/scala-${scalaVersion.binVersion}/resource_managed/$scope")
      )
  }
}
