package bleep

import java.net.URI

object Defaults {
  val MavenCentral = URI.create("https://repo1.maven.org/maven2")
  val BuildFileName = "bleep.json"

  val BloopFolder = ".bloop"
  val ScalaPluginPrefix = "-Xplugin:"
}
