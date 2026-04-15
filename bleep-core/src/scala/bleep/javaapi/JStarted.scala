package bleep.javaapi

import bleep.Started

import java.nio.file.Path
import scala.jdk.CollectionConverters.*

final class JStarted(private[javaapi] val underlying: Started) extends bleepscript.Started {
  override def logger(): bleepscript.Logger = new JLogger(underlying.logger)

  override def build(): bleepscript.Build = JModel.build(underlying.build)

  override def buildPaths(): bleepscript.BuildPaths = JModel.buildPaths(underlying.buildPaths)

  override def userPaths(): bleepscript.UserPaths = JModel.userPaths(underlying.userPaths)

  override def projectPaths(cross: bleepscript.CrossProjectName): bleepscript.ProjectPaths =
    JModel.projectPaths(underlying.projectPaths(JModel.toCross(cross)))

  override def jvmCommand(): Path = underlying.jvmCommand

  override def resolvedJvm(): bleepscript.ResolvedJvm = JModel.resolvedJvm(underlying.resolvedJvm.forceGet)

  override def fetchNode(nodeVersion: String): Path = underlying.pre.fetchNode(nodeVersion)

  override def activeProjects(): java.util.List[bleepscript.CrossProjectName] =
    underlying.activeProjectsFromPath match {
      case Some(arr) => arr.toList.map(JModel.crossProjectName).asJava
      case None      => java.util.Collections.emptyList()
    }

  override def exploded(cross: bleepscript.CrossProjectName): bleepscript.Project = {
    val c = JModel.toCross(cross)
    JModel.project(c, underlying.build.explodedProjects(c))
  }

  override def resolved(cross: bleepscript.CrossProjectName): bleepscript.ResolvedProject =
    JModel.resolvedProject(underlying.resolvedProject(JModel.toCross(cross)))

  override def bleepExecutable(): Path = underlying.bleepExecutable.forceGet.command
}
