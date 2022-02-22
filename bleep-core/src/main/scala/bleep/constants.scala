package bleep

import java.net.URI

object constants {
  val version = "0.0.1"
  val $schema = "https://raw.githubusercontent.com/oyvindberg/bleep/master/schema.json"
  val MavenCentral = URI.create("https://repo1.maven.org/maven2")
  val BuildFileName = "bleep.json"

  val ScalaPluginPrefix = "-Xplugin:"
}
