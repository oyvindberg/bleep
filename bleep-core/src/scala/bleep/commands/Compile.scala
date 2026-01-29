package bleep
package commands

import bleep.bsp.BspCommandFailed
import bleep.internal.{DoSourceGen, TransitiveProjects}
import bleep.nosbt.librarymanagement.ScalaArtifacts
import bloop.rifle.BuildServer
import ch.epfl.scala.bsp4j
import coursier.core.Version

import scala.jdk.CollectionConverters.*

case class Compile(watch: Boolean, projects: Array[model.CrossProjectName]) extends BleepCommandRemote(watch) with BleepCommandRemote.OnlyChanged {

  override def watchableProjects(started: Started): TransitiveProjects =
    TransitiveProjects(started.build, projects)

  override def onlyChangedProjects(started: Started, isChanged: model.CrossProjectName => Boolean): Compile =
    copy(projects = watchableProjects(started).transitiveFilter(isChanged).direct)

  override def runWithServer(started: Started, bloop: BuildServer): Either[BleepException, Unit] =
    for {
      _ <- DoSourceGen(started, bloop, watchableProjects(started))
      _ <- Compile.validateSIP51(started, projects)
      res <- {
        val targets = BleepCommandRemote.buildTargets(started.buildPaths, projects)
        val params = new bsp4j.CompileParams(targets)
        params.setArguments(List("--show-rendered-message").asJava)
        val result = bloop.buildTargetCompile(params).get()

        result.getStatusCode match {
          case bsp4j.StatusCode.OK => Right(started.logger.info("Compilation succeeded"))
          case other               => Left(new BspCommandFailed(s"compile", projects, BspCommandFailed.StatusCode(other)))
        }
      }
    } yield res
}

object Compile {

  /** SIP-51 validation: For Scala 2.13 and 3, check if scala.version < resolved scala-library version.
    *
    * This validation runs before compilation to ensure the Scala compiler is not older than the scala-library on the dependency classpath, which would cause
    * runtime errors or macro expansion failures due to backwards-only binary compatibility (SIP-51).
    */
  def validateSIP51(started: Started, projects: Array[model.CrossProjectName]): Either[BleepException, Unit] = {
    val violations: List[String] =
      projects.toList.flatMap { projectName =>
        val project = started.build.explodedProjects(projectName)

        for {
          sv <- model.VersionCombo.fromExplodedProject(project).toOption.flatMap(_.asScala).map(_.scalaVersion)
          if sv.is3Or213

          scalaVersionStr = sv.scalaVersion

          resolution <- started.bloopProject(projectName).resolution

          scalaLibraryArtifactName = ScalaArtifacts.LibraryID
          module <- resolution.modules.find(module => module.organization == ScalaArtifacts.Organization && module.name == scalaLibraryArtifactName)
          if Version(module.version) > Version(scalaVersionStr)

        } yield s"""Cannot compile ${projectName.value}: scala.version needs to be upgraded to ${module.version}.
                   |
                   |To support backwards-only binary compatibility (SIP-51), the Scala compiler cannot be older
                   |than $scalaLibraryArtifactName on the dependency classpath. The current scala.version is $scalaVersionStr,
                   |but $scalaLibraryArtifactName was resolved to ${module.version} by dependency management.
                   |
                   |To understand why $scalaLibraryArtifactName was upgraded, run:
                   |  bleep build evicted ${projectName.value}
                   |
                   |To fix this issue, upgrade scala.version to ${module.version} or higher in your bleep.yaml.
                   |""".stripMargin
      }

    violations match {
      case Nil        => Right(())
      case violations => Left(new BleepException.Text(violations.mkString("\n")))
    }
  }
}
