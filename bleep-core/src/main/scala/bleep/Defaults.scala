package bleep

import bloop.config.Config

import java.net.URI

object Defaults {
  val MavenCentral = URI.create("https://repo1.maven.org/maven2")
  val BuildFileName = "bleep.json"

  val BloopFolder = ".bloop"
  val ScalaPluginPrefix = "-Xplugin:"

  val DefaultCompileSetup = model.CompileSetup(
    order = Some(Config.CompileSetup.empty.order),
    addLibraryToBootClasspath = Some(Config.CompileSetup.empty.addLibraryToBootClasspath),
    addCompilerToClasspath = Some(Config.CompileSetup.empty.addCompilerToClasspath),
    addExtraJarsToClasspath = Some(Config.CompileSetup.empty.addExtraJarsToClasspath),
    manageBootClasspath = Some(Config.CompileSetup.empty.manageBootClasspath),
    filterLibraryFromClasspath = Some(Config.CompileSetup.empty.filterLibraryFromClasspath)
  )
}
