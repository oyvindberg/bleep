package bleep

import coursier.{LocalRepositories, Repositories}

import java.net.URI

object constants {
  val version = "0.0.1"
  val $schema = "https://raw.githubusercontent.com/oyvindberg/bleep/master/schema.json"

  val DefaultRepos = List(
    model.Repository.Ivy(None, URI.create(LocalRepositories.ivy2Local.pattern.chunks.head.string)),
    model.Repository.Maven(None, URI.create(Repositories.central.root))
  )

  val BuildFileName = "bleep.json"

  val ScalaPluginPrefix = "-Xplugin"
}
