package bleep

import java.net.URI

object constants {
  val version = "0.0.1"
  val $schema = "https://raw.githubusercontent.com/oyvindberg/bleep/master/schema.json"

  val DefaultRepos = List(
    model.Repository.Maven(uri = URI.create("https://repo1.maven.org/maven2")),
    model.Repository.Ivy(URI.create("https://repo.scala-sbt.org/scalasbt/sbt-plugin-releases/"))
  )

  val BuildFileName = "bleep.json"

  val ScalaPluginPrefix = "-Xplugin"
}
