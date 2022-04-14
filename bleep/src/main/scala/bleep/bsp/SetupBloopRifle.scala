package bleep
package bsp

import bleep.internal.{dependencyOrdering, Lazy}
import coursier.core.Dependency
import coursier.parse.ModuleParser

import java.io.File
import scala.build.blooprifle.BloopRifleConfig

object SetupBloopRifle {
  def apply(javaPath: String, buildPaths: BuildPaths, resolver: Lazy[CoursierResolver], address: BloopRifleConfig.Address): BloopRifleConfig =
    BloopRifleConfig.default(address, bloopClassPath(resolver), buildPaths.dotBleepDir.toFile).copy(javaPath = javaPath)

  def bloopClassPath(resolver: Lazy[CoursierResolver])(bloopVersion: String): Either[BuildException, Seq[File]] = {
    val modString = BloopRifleConfig.defaultModule
    ModuleParser
      .module(modString, BloopRifleConfig.defaultScalaVersion)
      .left
      .map(msg => new BuildException.ModuleFormatError(modString, msg))
      .flatMap { mod =>
        resolver.forceGet(JsonSet(Dependency(mod, bloopVersion)), forceScalaVersion = None) match {
          case Left(coursierError) => Left(new BuildException.ResolveError(coursierError, "installing bloop"))
          case Right(res)          => Right(res.jarFiles)
        }
      }
  }
}
