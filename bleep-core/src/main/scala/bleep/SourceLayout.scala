package bleep

import io.circe.{Decoder, Encoder}

sealed abstract class SourceLayout(val id: String) {
  def sources(maybeScalaVersion: Option[Versions.Scala], maybePlatformId: Option[model.PlatformId], scope: String): JsonSet[RelPath]
  def resources(maybeScalaVersion: Option[Versions.Scala], maybePlatformId: Option[model.PlatformId], scope: String): JsonSet[RelPath]

  final def sources(maybeScalaVersion: Option[Versions.Scala], maybePlatformId: Option[model.PlatformId], scope: Option[String]): JsonSet[RelPath] =
    sources(maybeScalaVersion, maybePlatformId, scope.getOrElse(""))
  final def resources(maybeScalaVersion: Option[Versions.Scala], maybePlatformId: Option[model.PlatformId], scope: Option[String]): JsonSet[RelPath] =
    resources(maybeScalaVersion, maybePlatformId, scope.getOrElse(""))
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
    override def sources(maybeScalaVersion: Option[Versions.Scala], maybePlatformId: Option[model.PlatformId], scope: String): JsonSet[RelPath] = JsonSet.empty
    override def resources(maybeScalaVersion: Option[Versions.Scala], maybePlatformId: Option[model.PlatformId], scope: String): JsonSet[RelPath] =
      JsonSet.empty
  }

  case object Java extends SourceLayout("java") {
    override def sources(maybeScalaVersion: Option[Versions.Scala], maybePlatformId: Option[model.PlatformId], scope: String): JsonSet[RelPath] =
      JsonSet(
        RelPath.force(s"src/$scope/java"),
        srcManaged(scope)
      )

    override def resources(maybeScalaVersion: Option[Versions.Scala], maybePlatformId: Option[model.PlatformId], scope: String): JsonSet[RelPath] =
      JsonSet(
        RelPath.force(s"src/$scope/resources"),
        resourceManaged(scope)
      )
  }

  case object Normal extends SourceLayout("normal") {
    override def sources(maybeScalaVersion: Option[Versions.Scala], maybePlatformId: Option[model.PlatformId], scope: String): JsonSet[RelPath] =
      maybeScalaVersion match {
        case Some(scalaVersion) =>
          JsonSet(
            RelPath.force(s"src/$scope/scala"),
            RelPath.force(s"src/$scope/java"),
            RelPath.force(s"src/$scope/scala-${scalaVersion.binVersion}"),
            RelPath.force(s"src/$scope/scala-${scalaVersion.epoch}"),
            srcManaged(scalaVersion, scope),
            srcManaged(scope)
          )
        case None => JsonSet.empty
      }
    override def resources(maybeScalaVersion: Option[Versions.Scala], maybePlatformId: Option[model.PlatformId], scope: String): JsonSet[RelPath] =
      maybeScalaVersion match {
        case Some(scalaVersion) =>
          JsonSet(
            resourceManaged(scalaVersion, scope),
            resourceManaged(scope),
            RelPath.force(s"src/$scope/resources")
          )

        case None => JsonSet.empty
      }
  }

  case object CrossPure extends SourceLayout("cross-pure") {
    override def sources(maybeScalaVersion: Option[Versions.Scala], maybePlatformId: Option[model.PlatformId], scope: String): JsonSet[RelPath] =
      (maybeScalaVersion, maybePlatformId) match {
        case (Some(scalaVersion), Some(platformId)) =>
          val dotPlatform = "." + platformId.value
          JsonSet(
            RelPath.force(s"$dotPlatform/src/$scope/scala"),
            RelPath.force(s"$dotPlatform/src/$scope/scala-${scalaVersion.binVersion}"),
            RelPath.force(s"$dotPlatform/src/$scope/scala-${scalaVersion.epoch}"),
            RelPath.force(s"$dotPlatform/src/$scope/java"),
            RelPath.force(s"src/$scope/scala-${scalaVersion.binVersion}"),
            RelPath.force(s"src/$scope/scala-${scalaVersion.epoch}"),
            RelPath.force(s"src/$scope/scala"),
            srcManaged(scalaVersion, scope).prepended(dotPlatform),
            srcManaged(scope).prepended(dotPlatform)
          )
        case _ => JsonSet.empty
      }

    override def resources(maybeScalaVersion: Option[Versions.Scala], maybePlatformId: Option[model.PlatformId], scope: String): JsonSet[RelPath] =
      (maybeScalaVersion, maybePlatformId) match {
        case (Some(scalaVersion), Some(platformId)) =>
          val dotPlatform = "." + platformId.value
          JsonSet(
            RelPath.force(s"$dotPlatform/src/$scope/resources"),
            RelPath.force(s"src/$scope/resources"),
            resourceManaged(scalaVersion, scope).prepended(dotPlatform),
            resourceManaged(scope).prepended(dotPlatform)
          )
        case _ => JsonSet.empty
      }
  }

  case object CrossFull extends SourceLayout("cross-full") {
    override def sources(maybeScalaVersion: Option[Versions.Scala], maybePlatformId: Option[model.PlatformId], scope: String): JsonSet[RelPath] =
      (maybeScalaVersion, maybePlatformId) match {
        case (Some(scalaVersion), Some(platformId)) =>
          JsonSet(
            RelPath.force(s"${platformId.value}/src/$scope/scala"),
            RelPath.force(s"${platformId.value}/src/$scope/scala-${scalaVersion.binVersion}"),
            RelPath.force(s"${platformId.value}/src/$scope/scala-${scalaVersion.epoch}"),
            RelPath.force(s"${platformId.value}/src/$scope/java"),
            RelPath.force(s"shared/src/$scope/scala-${scalaVersion.binVersion}"),
            RelPath.force(s"shared/src/$scope/scala-${scalaVersion.epoch}"),
            RelPath.force(s"shared/src/$scope/scala"),
            srcManaged(scalaVersion, scope).prepended(platformId.value),
            srcManaged(scope).prepended(platformId.value)
          )
        case _ => JsonSet.empty
      }

    override def resources(maybeScalaVersion: Option[Versions.Scala], maybePlatformId: Option[model.PlatformId], scope: String): JsonSet[RelPath] =
      (maybeScalaVersion, maybePlatformId) match {
        case (Some(scalaVersion), Some(platformId)) =>
          JsonSet(
            RelPath.force(s"${platformId.value}/src/$scope/resources"),
            RelPath.force(s"shared/src/$scope/resources"),
            resourceManaged(scalaVersion, scope).prepended(platformId.value),
            resourceManaged(scope).prepended(platformId.value)
          )
        case _ => JsonSet.empty
      }
  }

  private def resourceManaged(scalaVersion: Versions.Scala, scope: String): RelPath = {
    // seems like a surprising default in sbt
    val v = if (scalaVersion.is3) scalaVersion.scalaVersion else scalaVersion.binVersion
    RelPath.force(s"target/scala-$v/resource_managed/$scope")
  }

  private def resourceManaged(scope: String): RelPath =
    RelPath.force(s"target/resource_managed/$scope")

  private def srcManaged(scalaVersion: Versions.Scala, scope: String): RelPath = {
    // seems like a surprising default in sbt
    val v = if (scalaVersion.is3) scalaVersion.scalaVersion else scalaVersion.binVersion
    RelPath.force(s"target/scala-$v/src_managed/$scope")
  }

  private def srcManaged(scope: String): RelPath =
    RelPath.force(s"target/src_managed/$scope")
}
