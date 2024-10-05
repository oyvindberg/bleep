package bleep.depcheck

import bleep.nosbt.librarymanagement
import coursier.core.*
import coursier.core.Resolution.ModuleVersion
import coursier.util.Print
import ryddig.Logger

object UpdateRun {

  // Move back to coursier.util (in core module) after 1.0?
  private def allDependenciesByConfig(
      res: Map[Configuration, Resolution],
      depsByConfig: Map[Configuration, Seq[Dependency]],
      configs: Map[Configuration, Set[Configuration]]
  ): Map[Configuration, Set[Dependency]] = {

    val allDepsByConfig = depsByConfig.map { case (config, deps) =>
      config -> res(config).subset(deps).minDependencies
    }

    val filteredAllDepsByConfig = allDepsByConfig.map { case (config, allDeps) =>
      val allExtendedConfigs = configs.getOrElse(config, Set.empty) - config
      val inherited = allExtendedConfigs
        .flatMap(allDepsByConfig.getOrElse(_, Set.empty))

      config -> (allDeps -- inherited)
    }

    filteredAllDepsByConfig
  }

  // Move back to coursier.util (in core module) after 1.0?
  private def dependenciesWithConfig(
      res: Map[Configuration, Resolution],
      depsByConfig: Map[Configuration, Seq[Dependency]],
      configs: Map[Configuration, Set[Configuration]]
  ): Set[Dependency] =
    allDependenciesByConfig(res, depsByConfig, configs)
      .flatMap { case (config, deps) =>
        deps.map(dep => dep.withConfiguration(config --> dep.configuration))
      }
      .groupBy(_.withConfiguration(Configuration.empty))
      .map { case (dep, l) =>
        dep.withConfiguration(Configuration.join(l.map(_.configuration).toSeq: _*))
      }
      .toSet

  def update(
      params: UpdateParams,
      verbosityLevel: Int,
      log: Logger
  ): librarymanagement.UpdateReport = {

    if (verbosityLevel >= 2) {
      val depsByConfig = grouped(params.dependencies)
      val finalDeps = dependenciesWithConfig(params.res, depsByConfig, params.configs)
      val projCache = params.res.values.foldLeft(Map.empty[ModuleVersion, Project])(_ ++ _.projectCache.map { case (k, (_, v)) => (k, v) })
      val repr = Print.dependenciesUnknownConfigs(finalDeps.toVector, projCache)
      log.info(repr.split('\n').map("  " + _).mkString("\n"))
    }

    SbtUpdateReport(
      params.thisModule,
      params.res.toVector.sortBy(_._1.value), // FIXME Order by config topologically?
      params.interProjectDependencies.toVector,
      params.classifiers,
      params.artifactFileOpt,
      params.fullArtifacts,
      log,
      includeSignatures = params.includeSignatures,
      classpathOrder = params.classpathOrder,
      missingOk = params.missingOk,
      params.forceVersions,
      params.classLoaders
    )
  }

  private def grouped[K, V](map: Seq[(K, V)]): Map[K, Seq[V]] =
    map
      .groupBy(_._1)
      .map { case (k, v) => (k, v.map { case (_, vv) => vv }) }
}
