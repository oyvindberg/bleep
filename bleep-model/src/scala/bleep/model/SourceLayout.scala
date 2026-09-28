package bleep
package model

import io.circe.{Decoder, Encoder}

sealed abstract class SourceLayout(val id: String) {
  def sources(
      maybeScalaVersion: Option[VersionScala],
      maybePlatformId: Option[model.PlatformId],
      crossPlatforms: Set[model.PlatformId],
      scope: String
  ): JsonSet[RelPath]
  def resources(
      maybeScalaVersion: Option[VersionScala],
      maybePlatformId: Option[model.PlatformId],
      crossPlatforms: Set[model.PlatformId],
      scope: String
  ): JsonSet[RelPath]

  final def sources(
      maybeScalaVersion: Option[VersionScala],
      maybePlatformId: Option[model.PlatformId],
      crossPlatforms: Set[model.PlatformId],
      scope: Option[String]
  ): JsonSet[RelPath] =
    sources(maybeScalaVersion, maybePlatformId, crossPlatforms, scope.getOrElse(""))
  final def resources(
      maybeScalaVersion: Option[VersionScala],
      maybePlatformId: Option[model.PlatformId],
      crossPlatforms: Set[model.PlatformId],
      scope: Option[String]
  ): JsonSet[RelPath] =
    resources(maybeScalaVersion, maybePlatformId, crossPlatforms, scope.getOrElse(""))
}

object SourceLayout {
  val All = List(SbtMatrix, CrossPure, CrossFull, Normal, Java, Kotlin, None_).map(x => x.id -> x).toMap

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
    override def sources(
        maybeScalaVersion: Option[VersionScala],
        maybePlatformId: Option[model.PlatformId],
        crossPlatforms: Set[model.PlatformId],
        scope: String
    ): JsonSet[RelPath] = JsonSet.empty
    override def resources(
        maybeScalaVersion: Option[VersionScala],
        maybePlatformId: Option[model.PlatformId],
        crossPlatforms: Set[model.PlatformId],
        scope: String
    ): JsonSet[RelPath] =
      JsonSet.empty
  }

  case object Java extends SourceLayout("java") {
    override def sources(
        maybeScalaVersion: Option[VersionScala],
        maybePlatformId: Option[model.PlatformId],
        crossPlatforms: Set[model.PlatformId],
        scope: String
    ): JsonSet[RelPath] =
      JsonSet(
        RelPath.force(s"src/$scope/java")
      )

    override def resources(
        maybeScalaVersion: Option[VersionScala],
        maybePlatformId: Option[model.PlatformId],
        crossPlatforms: Set[model.PlatformId],
        scope: String
    ): JsonSet[RelPath] =
      JsonSet(
        RelPath.force(s"src/$scope/resources")
      )
  }

  case object Kotlin extends SourceLayout("kotlin") {
    override def sources(
        maybeScalaVersion: Option[VersionScala],
        maybePlatformId: Option[model.PlatformId],
        crossPlatforms: Set[model.PlatformId],
        scope: String
    ): JsonSet[RelPath] =
      JsonSet(
        RelPath.force(s"src/$scope/kotlin"),
        RelPath.force(s"src/$scope/java")
      )

    override def resources(
        maybeScalaVersion: Option[VersionScala],
        maybePlatformId: Option[model.PlatformId],
        crossPlatforms: Set[model.PlatformId],
        scope: String
    ): JsonSet[RelPath] =
      JsonSet(
        RelPath.force(s"src/$scope/resources")
      )
  }

  case object Normal extends SourceLayout("normal") {
    override def sources(
        maybeScalaVersion: Option[VersionScala],
        maybePlatformId: Option[model.PlatformId],
        crossPlatforms: Set[model.PlatformId],
        scope: String
    ): JsonSet[RelPath] =
      maybeScalaVersion match {
        case Some(scalaVersion) =>
          JsonSet(
            RelPath.force(s"src/$scope/scala"),
            RelPath.force(s"src/$scope/java"),
            RelPath.force(s"src/$scope/scala-${scalaVersion.binVersion}"),
            RelPath.force(s"src/$scope/scala-${scalaVersion.epoch}")
          )
        case None => JsonSet.empty
      }
    override def resources(
        maybeScalaVersion: Option[VersionScala],
        maybePlatformId: Option[model.PlatformId],
        crossPlatforms: Set[model.PlatformId],
        scope: String
    ): JsonSet[RelPath] =
      JsonSet(
        RelPath.force(s"src/$scope/resources")
      )
  }

  case object CrossPure extends SourceLayout("cross-pure") {
    override def sources(
        maybeScalaVersion: Option[VersionScala],
        maybePlatformId: Option[model.PlatformId],
        crossPlatforms: Set[model.PlatformId],
        scope: String
    ): JsonSet[RelPath] =
      maybePlatformId match {
        case Some(platformId) =>
          val fromNormal = Normal.sources(maybeScalaVersion, maybePlatformId, crossPlatforms, scope)
          fromNormal ++ fromNormal.map(path => path.prefixed("." + platformId.value))
        case _ => JsonSet.empty
      }

    override def resources(
        maybeScalaVersion: Option[VersionScala],
        maybePlatformId: Option[model.PlatformId],
        crossPlatforms: Set[model.PlatformId],
        scope: String
    ): JsonSet[RelPath] =
      maybePlatformId match {
        case Some(platformId) =>
          val fromNormal = Normal.resources(maybeScalaVersion, maybePlatformId, crossPlatforms, scope)
          fromNormal ++ fromNormal.map(path => path.prefixed("." + platformId.value))
        case _ => JsonSet.empty
      }
  }

  /** The layout of sbt-crossproject's `CrossType.Full`:
    *   - `shared/`, for all platforms
    *   - `<platform>/`, for one
    *   - a directory for each group of some but not all of the platforms the project is built for, named after them in order: `js-jvm/`, `js-native/` and
    *     `jvm-native/` for a project built for all three. A jvm project reads `js-jvm/` and `jvm-native/`. A project built for two platforms shares all its
    *     code in `shared/`, and has none
    */
  case object CrossFull extends SourceLayout("cross-full") {

    /** The directories a platform shares with some but not all of the others, as sbt-crossproject has them (`makePartiallySharedSettings` in its
      * `CrossProject.scala`)
      */
    def partiallySharedDirs(platformId: model.PlatformId, crossPlatforms: Set[model.PlatformId]): List[String] = {
      if (!crossPlatforms(platformId))
        throw new BleepException.Text(
          s"A ${platformId.value} project must be built for its own platform, but was said to be built for ${crossPlatforms.map(_.value).mkString(", ")}"
        )
      crossPlatforms
        .subsets()
        .filter(group => group.size > 1 && group.size < crossPlatforms.size && group(platformId))
        .map(group => group.toList.map(_.value).sorted.mkString("-"))
        .toList
        .sorted
    }

    private def dirs(platformId: model.PlatformId, crossPlatforms: Set[model.PlatformId]): List[String] =
      "shared" :: partiallySharedDirs(platformId, crossPlatforms) ++ List(platformId.value)

    override def sources(
        maybeScalaVersion: Option[VersionScala],
        maybePlatformId: Option[model.PlatformId],
        crossPlatforms: Set[model.PlatformId],
        scope: String
    ): JsonSet[RelPath] =
      maybePlatformId match {
        case Some(platformId) =>
          val fromNormal = Normal.sources(maybeScalaVersion, maybePlatformId, crossPlatforms, scope)
          JsonSet.fromIterable(dirs(platformId, crossPlatforms).flatMap(dir => fromNormal.values.map(_.prefixed(dir))))
        case _ => JsonSet.empty
      }

    override def resources(
        maybeScalaVersion: Option[VersionScala],
        maybePlatformId: Option[model.PlatformId],
        crossPlatforms: Set[model.PlatformId],
        scope: String
    ): JsonSet[RelPath] =
      maybePlatformId match {
        case Some(platformId) =>
          val fromNormal = Normal.resources(maybeScalaVersion, maybePlatformId, crossPlatforms, scope)
          JsonSet.fromIterable(dirs(platformId, crossPlatforms).flatMap(dir => fromNormal.values.map(_.prefixed(dir))))
        case _ => JsonSet.empty
      }
  }

  /** The layout of sbt-projectmatrix: one directory for all platforms and scala versions, with the platform and scala version in the names of source
    * directories. `ProjectMatrix.makeSources` adds `scala<platform>`, `scala<platform>-<scala binary version>` and `java<platform>`
    */
  case object SbtMatrix extends SourceLayout("sbt-matrix") {
    override def sources(
        maybeScalaVersion: Option[VersionScala],
        maybePlatformId: Option[model.PlatformId],
        crossPlatforms: Set[model.PlatformId],
        scope: String
    ): JsonSet[RelPath] =
      maybeScalaVersion match {
        case Some(scalaVersion) =>
          val fromNormal = Normal.sources(maybeScalaVersion, maybePlatformId, crossPlatforms, scope)
          val fromMatrix = maybePlatformId match {
            case Some(platformId) =>
              JsonSet(
                RelPath.force(s"src/$scope/scala${platformId.value}"),
                RelPath.force(s"src/$scope/scala${platformId.value}-${scalaVersion.binVersion}"),
                RelPath.force(s"src/$scope/scala${platformId.value}-${scalaVersion.epoch}"),
                RelPath.force(s"src/$scope/java${platformId.value}")
              )

            case None => JsonSet.empty[RelPath]
          }
          fromNormal ++ fromMatrix
        case None => JsonSet.empty
      }
    override def resources(
        maybeScalaVersion: Option[VersionScala],
        maybePlatformId: Option[model.PlatformId],
        crossPlatforms: Set[model.PlatformId],
        scope: String
    ): JsonSet[RelPath] =
      JsonSet(
        RelPath.force(s"src/$scope/resources")
      )
  }
}
