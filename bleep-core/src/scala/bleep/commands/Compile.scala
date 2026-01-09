package bleep
package commands

import bleep.bsp.BspCommandFailed
import bleep.internal.{DoSourceGen, TransitiveProjects}
import bleep.nosbt.librarymanagement.ScalaArtifacts
import bloop.rifle.BuildServer
import ch.epfl.scala.bsp4j
import coursier.core.Version

case class Compile(watch: Boolean, projects: Array[model.CrossProjectName]) extends BleepCommandRemote(watch) with BleepCommandRemote.OnlyChanged {

  override def watchableProjects(started: Started): TransitiveProjects =
    TransitiveProjects(started.build, projects)

  override def onlyChangedProjects(started: Started, isChanged: model.CrossProjectName => Boolean): Compile =
    copy(projects = watchableProjects(started).transitiveFilter(isChanged).direct)

  override def runWithServer(started: Started, bloop: BuildServer): Either[BleepException, Unit] =
    DoSourceGen(started, bloop, watchableProjects(started)).flatMap { _ =>
      // SIP-51 validation: Check for scala-library version mismatches before compiling
      Compile.validateSIP51(started, projects) match {
        case Left(err) => Left(err)
        case Right(_) =>
          val targets = BleepCommandRemote.buildTargets(started.buildPaths, projects)

          val result = bloop.buildTargetCompile(new bsp4j.CompileParams(targets)).get()

          result.getStatusCode match {
            case bsp4j.StatusCode.OK => Right(started.logger.info("Compilation succeeded"))
            case other               => Left(new BspCommandFailed(s"compile", projects, BspCommandFailed.StatusCode(other)))
          }
      }
    }
}

object Compile {
  /** SIP-51 validation: For Scala 2.13 and 3, check if scala.version < resolved scala-library version.
    *
    * This validation runs before compilation to ensure the Scala compiler is not older than the scala-library
    * on the dependency classpath, which would cause runtime errors or macro expansion failures due to
    * backwards-only binary compatibility (SIP-51).
    */
  def validateSIP51(started: Started, projects: Array[model.CrossProjectName]): Either[BleepException, Unit] = {
    val violations = projects.flatMap { projectName =>
      val project = started.build.explodedProjects(projectName)
      val versionCombo = model.VersionCombo.fromExplodedProject(project).toOption

      versionCombo match {
        case Some(vc @ model.VersionCombo.Scala(_, _, scalaVersion, _)) if scalaVersion.is3Or213 =>
          // Get resolved modules from Bloop config
          val bloopProject = started.bloopProject(projectName)
          val scalaVersionStr = scalaVersion.scalaVersion
          val scalaVersionParsed = Version(scalaVersionStr)

          // Check if scala-library was resolved to a higher version than scala.version
          ScalaArtifacts.Artifacts.iterator.flatMap { artifactName =>
            bloopProject.resolution.modules.find { module =>
              module.organization == ScalaArtifacts.Organization && module.name == artifactName
            }.flatMap { module =>
              val resolvedVersion = module.version
              val resolvedParsed = Version(resolvedVersion)
              if (resolvedParsed > scalaVersionParsed)
                Some((projectName, artifactName, resolvedVersion, scalaVersionStr))
              else
                None
            }
          }.toList.headOption

        case _ => None
      }
    }

    violations.headOption match {
      case Some((projectName, artifactName, resolvedVersion, scalaVersionStr)) =>
        Left(
          new BleepException.Text(
            s"""|Cannot compile ${projectName.value}: scala.version needs to be upgraded to $resolvedVersion.
                |
                |To support backwards-only binary compatibility (SIP-51), the Scala compiler cannot be older
                |than $artifactName on the dependency classpath. The current scala.version is $scalaVersionStr,
                |but $artifactName was resolved to $resolvedVersion by dependency management.
                |
                |To understand why $artifactName was upgraded, run:
                |  bleep evicted ${projectName.value}
                |
                |To fix this issue, upgrade scala.version to $resolvedVersion or higher in your bleep.yaml.
                |""".stripMargin
          )
        )
      case None => Right(())
    }
  }
}
