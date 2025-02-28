package bleep
package depcheck

import bleep.nosbt.librarymanagement
import bleep.nosbt.util.ShowLines.*
import coursier.core.{Configuration, Dependency, Module}
import coursier.error.CoursierError
import coursier.{Fetch, ModuleName, Organization}
import ryddig.Logger

import scala.collection.mutable

object CheckEvictions {
  def apply(
      versionCombo: model.VersionCombo,
      dependencies: Seq[Dependency],
      versionSchemes: Seq[model.LibraryVersionScheme],
      res: Fetch.Result,
      logger: Logger
  ): Either[BleepEvictionError, Unit] = {
    val ee = librarymanagement.EvictionError(
      report = updateRun(dependencies, res, logger),
      module = dummyModuleDescriptor(versionCombo),
      schemes = versionSchemes.map(asModule(versionCombo))
    )
    if (ee.incompatibleEvictions.nonEmpty) Left(BleepEvictionError(ee.lines)) else Right(())
  }

  def asModule(versionCombo: model.VersionCombo)(v: model.LibraryVersionScheme): librarymanagement.ModuleID =
    librarymanagement.ModuleID(
      organization = v.dep.organization.value,
      name = v.dep.asJava(versionCombo).orThrowText.moduleName.value,
      revision = v.scheme.value
    )

  def warnings(
      ewo: librarymanagement.EvictionWarningOptions,
      versionCombo: model.VersionCombo,
      dependencies: Seq[Dependency],
      res: Fetch.Result,
      logger: Logger
  ): Unit = {
    val finalReport: librarymanagement.UpdateReport = updateRun(dependencies, res, logger)

    val module: librarymanagement.ModuleDescriptor = dummyModuleDescriptor(versionCombo)

    val ew: librarymanagement.EvictionWarning = librarymanagement.EvictionWarning(module, ewo, finalReport)
    ew.lines.foreach(logger.warn(_))
    infoAllTheThings(ew).foreach(logger.info(_))
  }

  def updateRun(dependencies: Seq[Dependency], res: Fetch.Result, logger: Logger): librarymanagement.UpdateReport = {
    val params = UpdateParams(
      thisModule = (Module(Organization("fake.org"), ModuleName("project"), Map.empty), ""),
      artifacts = res.fullDetailedArtifacts.collect { case (_, _, a, Some(f)) => a -> f }.toMap,
      fullArtifacts = Some(res.fullDetailedArtifacts.map { case (d, p, a, f) => (d, p, a) -> f }.toMap),
      classifiers = None,
      configs = Map(Configuration.compile -> Set.empty),
      dependencies = dependencies.map(d => (Configuration.compile, d)),
      forceVersions = Map.empty,
      interProjectDependencies = Nil,
      res = Map(Configuration.compile -> res.resolution),
      includeSignatures = false,
      sbtBootJarOverrides = Map.empty,
      classpathOrder = true,
      missingOk = false,
      classLoaders = Nil
    )
    UpdateRun.update(params, verbosityLevel = 1, logger)
  }

  private def dummyModuleDescriptor(versionCombo: model.VersionCombo) = {
    val module = new librarymanagement.ModuleDescriptor {
      override def directDependencies: Vector[librarymanagement.ModuleID] = Vector.empty

      override def scalaModuleInfo: Option[librarymanagement.ScalaModuleInfo] =
        versionCombo match {
          case model.VersionCombo.Java =>
            None
          case scala: model.VersionCombo.Scala =>
            Some(
              librarymanagement.ScalaModuleInfo(
                scalaFullVersion = scala.scalaVersion.scalaVersion,
                scalaBinaryVersion = scala.scalaVersion.binVersion,
                configurations = Vector.empty,
                checkExplicit = false,
                filterImplicit = false,
                overrideScalaVersion = false,
                scalaOrganization = scala.scalaVersion.scalaOrganization,
                scalaArtifacts = null
              )
            )
        }

      override def moduleSettings: librarymanagement.ModuleSettings = null

      override def extraInputHash: Long = 0L
    }
    module
  }

  case class BleepEvictionError(lines: Seq[String]) extends CoursierError(lines.mkString("\n"))

  // copied from sbt because it's private
  def infoAllTheThings(warning: librarymanagement.EvictionWarning): List[String] =
    if (warning.options.infoAllEvictions) {
      val out: mutable.ListBuffer[String] = mutable.ListBuffer()
      warning.allEvictions.foreach { evictionPair =>
        if (warning.scalaEvictions.contains[librarymanagement.EvictionPair](evictionPair) && warning.options.warnScalaVersionEviction) ()
        else if (warning.directEvictions.contains[librarymanagement.EvictionPair](evictionPair) && warning.options.warnDirectEvictions) ()
        else if (warning.transitiveEvictions.contains[librarymanagement.EvictionPair](evictionPair) && warning.options.warnTransitiveEvictions)
          ()
        else {
          out ++= evictionPair.lines
        }
      }
      if (out.isEmpty) Nil
      else List("Here are other dependency conflicts that were resolved:", "") ::: out.toList
    } else Nil
}
