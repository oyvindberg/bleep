package bleep

import coursier.{LocalRepositories, Repositories}

import java.net.URI
import java.nio.file.Path

object constants {
  val version = "0.0.1-M3"
  val $schema = "https://raw.githubusercontent.com/oyvindberg/bleep/master/schema.json"

  private val ivyLocalUri =
    URI.create(LocalRepositories.ivy2Local.pattern.chunks.head.string)

  val ivy2Path = Path.of(ivyLocalUri)
  val DefaultRepos = List(
    model.Repository.Ivy(None, ivyLocalUri),
    model.Repository.Maven(None, URI.create(Repositories.central.root))
  )

  val BuildFileName = "bleep.json"

  val ScalaPluginPrefix = "-Xplugin"
}
