package bleep
package internal

import ryddig.Logger

/** bleep compiles scala 2.12, 2.13 and 3. An imported build which is also built for an older scala version is imported without those cross projects */
object dropUnsupportedScala {
  def isSupported(version: model.VersionScala): Boolean =
    version.is3 || version.binVersion == "2.13" || version.binVersion == "2.12"

  def apply(logger: Logger, build: model.Build.Exploded): model.Build.Exploded = {
    val (unsupported, supported) = build.explodedProjects.partition { case (_, p) => p.scala.flatMap(_.version).exists(v => !isSupported(v)) }
    if (unsupported.nonEmpty) {
      val versions = unsupported.values.flatMap(_.scala.flatMap(_.version)).map(_.scalaVersion).toList.distinct.sorted
      logger
        .withContext("projects", unsupported.keys.toList.sorted.map(_.value).mkString(", "))
        .warn(s"bleep compiles scala 2.12 and newer, so ${unsupported.size} cross projects for scala ${versions.mkString(", ")} are left out")
    }
    build.copy(explodedProjects = supported)
  }
}
