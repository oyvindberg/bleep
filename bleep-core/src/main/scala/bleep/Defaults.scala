package bleep

import java.net.URI

object Defaults {
  val MavenCentral = URI.create("https://repo1.maven.org/maven2")
  val BuildFileName = "bleep.json"

  val BloopFolder = ".bloop"

  def sourceDirs(maybeScalaVersion: Option[Versions.Scala], scope: String): List[RelPath] =
    List(
      List(RelPath.force(s"src/$scope/java")),
      maybeScalaVersion match {
        case Some(scalaVersion) =>
          List(
            RelPath.force(s"src/$scope/scala"),
            RelPath.force(s"src/$scope/scala-${scalaVersion.binVersion}"),
            RelPath.force(s"src/$scope/scala-${scalaVersion.epoch}"),
            RelPath.force(s"target/scala-${scalaVersion.binVersion}/src_managed/$scope")
          )
        case None =>
          Nil
      }
    ).flatten

  def resourceDirs(maybeScalaVersion: Option[Versions.Scala], scope: String): List[RelPath] =
    List(
      List(RelPath.force(s"src/$scope/resources")),
      maybeScalaVersion match {
        case Some(scalaVersion) =>
          List(RelPath.force(s"target/scala-${scalaVersion.binVersion}/resource_managed/$scope"))
        case None => Nil
      }
    ).flatten
}
