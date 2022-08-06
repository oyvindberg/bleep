package bleep

import coursier.{LocalRepositories, Repositories}

import java.net.URI
import java.nio.file.Path

object constants {
  val BleepVersionTemplate = "$BLEEP_VERSION"
  val ScalaVersionTemplate = "$SCALA_VERSION"
  val Node = "18.4.0"

  private val ivyLocalUri =
    URI.create(LocalRepositories.ivy2Local.pattern.chunks.head.string)

  val ivy2Path = Path.of(ivyLocalUri)
  val DefaultRepos = List(
    model.Repository.Ivy(None, ivyLocalUri),
    model.Repository.Maven(None, URI.create(Repositories.central.root))
  )

  val ScalaPluginPrefix = "-Xplugin"
}
