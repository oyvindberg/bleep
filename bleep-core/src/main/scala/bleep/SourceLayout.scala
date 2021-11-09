package bleep

import io.circe.{Decoder, Encoder}

sealed abstract class SourceLayout(val id: String) {
  protected def scalaSources(scalaVersion: Versions.Scala, scope: String): JsonSet[RelPath]
  protected def scalaResources(scalaVersion: Versions.Scala, scope: String): JsonSet[RelPath]

  final def sources(maybeScalaVersion: Option[Versions.Scala], maybeScope: Option[String]): JsonSet[RelPath] = {
    val scope = maybeScope.getOrElse("")
    maybeScalaVersion match {
      case Some(scalaVersion) => scalaSources(scalaVersion, scope)
      case None               => JsonSet(RelPath.force(s"src/$scope/java"))
    }
  }

  final def resources(maybeScalaVersion: Option[Versions.Scala], maybeScope: Option[String]): JsonSet[RelPath] = {
    val scope = maybeScope.getOrElse("")
    maybeScalaVersion match {
      case Some(scalaVersion) => scalaResources(scalaVersion, scope)
      case None               => JsonSet(RelPath.force(s"src/$scope/resources"))
    }
  }
}

object SourceLayout {
  val All = List(CrossPure, CrossFull, Normal, Java, None_).map(x => x.id -> x).toMap

  implicit val decoder: Decoder[SourceLayout] =
    Decoder[Option[String]].emap {
      case Some(str) => All.get(str).toRight(s"$str not among ${All.keys.mkString(", ")}")
      case None      => Right(Normal)
    }

  implicit val encoder: Encoder[SourceLayout] =
    Encoder[Option[String]].contramap {
      case Normal => None
      case other  => Some(other.id)
    }

  case object None_ extends SourceLayout("none") {
    override protected def scalaSources(scalaVersion: Versions.Scala, scope: String): JsonSet[RelPath] = JsonSet.empty
    override protected def scalaResources(scalaVersion: Versions.Scala, scope: String): JsonSet[RelPath] = JsonSet.empty
  }

  case object Java extends SourceLayout("java") {
    override protected def scalaSources(scalaVersion: Versions.Scala, scope: String): JsonSet[RelPath] =
      JsonSet(RelPath.force(s"src/$scope/java"))
    override protected def scalaResources(scalaVersion: Versions.Scala, scope: String): JsonSet[RelPath] =
      JsonSet(RelPath.force(s"src/$scope/resources"))
  }

  case object Normal extends SourceLayout("normal") {
    override protected def scalaSources(scalaVersion: Versions.Scala, scope: String): JsonSet[RelPath] =
      JsonSet(
        RelPath.force(s"src/$scope/scala"),
        RelPath.force(s"src/$scope/java"),
        RelPath.force(s"src/$scope/scala-${scalaVersion.binVersion}"),
        RelPath.force(s"src/$scope/scala-${scalaVersion.epoch}"),
        srcManaged(scalaVersion, scope)
      )

    override protected def scalaResources(scalaVersion: Versions.Scala, scope: String): JsonSet[RelPath] =
      JsonSet(
        resourceManaged(scalaVersion, scope),
        RelPath.force(s"src/$scope/resources")
      )
  }

  case object CrossPure extends SourceLayout("cross-pure") {
    override protected def scalaSources(scalaVersion: Versions.Scala, scope: String) =
      JsonSet(
        RelPath.force(s"src/$scope/scala"),
        RelPath.force(s"src/$scope/scala-${scalaVersion.binVersion}"),
        RelPath.force(s"src/$scope/scala-${scalaVersion.epoch}"),
        RelPath.force(s"src/$scope/java"),
        RelPath.force(s"../src/$scope/scala-${scalaVersion.binVersion}"),
        RelPath.force(s"../src/$scope/scala-${scalaVersion.epoch}"),
        RelPath.force(s"../src/$scope/scala"),
        srcManaged(scalaVersion, scope)
      )

    override protected def scalaResources(scalaVersion: Versions.Scala, scope: String): JsonSet[RelPath] =
      JsonSet(
        RelPath.force(s"src/$scope/resources"),
        RelPath.force(s"../src/$scope/resources"),
        resourceManaged(scalaVersion, scope)
      )
  }

  case object CrossFull extends SourceLayout("cross-full") {
    override protected def scalaSources(scalaVersion: Versions.Scala, scope: String) =
      JsonSet(
        RelPath.force(s"src/$scope/scala"),
        RelPath.force(s"src/$scope/scala-${scalaVersion.binVersion}"),
        RelPath.force(s"src/$scope/scala-${scalaVersion.epoch}"),
        RelPath.force(s"src/$scope/java"),
        RelPath.force(s"../shared/src/$scope/scala-${scalaVersion.binVersion}"),
        RelPath.force(s"../shared/src/$scope/scala-${scalaVersion.epoch}"),
        RelPath.force(s"../shared/src/$scope/scala"),
        srcManaged(scalaVersion, scope)
      )

    override protected def scalaResources(scalaVersion: Versions.Scala, scope: String): JsonSet[RelPath] =
      JsonSet(
        RelPath.force(s"src/$scope/resources"),
        RelPath.force(s"../shared/src/$scope/resources"),
        resourceManaged(scalaVersion, scope)
      )
  }

  private def resourceManaged(scalaVersion: Versions.Scala, scope: String): RelPath = {
    // seems like a surprising default in sbt
    val v = if (scalaVersion.is3) scalaVersion.scalaVersion else scalaVersion.binVersion
    RelPath.force(s"target/scala-$v/resource_managed/$scope")
  }

  private def srcManaged(scalaVersion: Versions.Scala, scope: String): RelPath = {
    // seems like a surprising default in sbt
    val v = if (scalaVersion.is3) scalaVersion.scalaVersion else scalaVersion.binVersion
    RelPath.force(s"target/scala-$v/src_managed/$scope")
  }
}
