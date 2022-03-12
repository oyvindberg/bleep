package bleep

import coursier.{LocalRepositories, Repositories}

import java.net.URI

object constants {
  val version = "0.0.1"
  val $schema = "https://raw.githubusercontent.com/oyvindberg/bleep/master/schema.json"

  val DefaultRepos = List(
    model.Repository.Ivy(URI.create(LocalRepositories.ivy2Local.pattern.chunks.head.string)),
    model.Repository.Maven(URI.create(Repositories.central.root))
//    model.Repository.Ivy(URI.create(Repositories.sbtPlugin("releases").pattern.chunks.head.string))
  )

  val BuildFileName = "bleep.json"

  val ScalaPluginPrefix = "-Xplugin"
}
