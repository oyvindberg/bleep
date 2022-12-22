package bleep
package commands

import bleep.logging.Logger
import bleep.model.{CrossProjectName, Dep}
import bleep.rewrites.{normalizeBuild, UpgradeDependencies}
import coursier.Repository
import coursier.cache.FileCache
import coursier.core.{Dependency, Versions}
import coursier.util.Task

import scala.collection.immutable
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Success

case object BuildUpdateDeps extends BleepBuildCommand {
  override def run(started: Started): Either[BleepException, Unit] = {
    val build = started.build.requireFileBacked("command update-deps")
    // a bleep dependency may be instantiated into several different coursier dependencies
    // depending on which scala versions and platforms are plugging in
    // collect all instantiations into this structure
    val allDeps: Map[UpgradeDependencies.ContextualDep, Dependency] =
      instantiateAllDependencies(build)

    val config = started.lazyConfig.forceGet
    val repos = CoursierResolver.coursierRepos(build.resolvers.values, config.authentications).filter(_.repr.contains("http"))
    val fileCache = FileCache[Task]().withLogger(new BleepCacheLogger(started.logger))

    val foundByDep: Map[UpgradeDependencies.ContextualDep, (Dependency, Versions)] = {
      implicit val ec: ExecutionContext = started.executionContext
      Await.result(fetchAllVersions(fileCache, repos, allDeps), Duration.Inf)
    }

    val upgrades: Map[UpgradeDependencies.ContextualDep, model.Dep] =
      foundByDep.flatMap { case (tuple @ (bleepDep, _), (_, version)) =>
        val latest = version.latest
        if (latest == bleepDep.version) None
        else Some(tuple -> bleepDep.withVersion(latest))
      }

    val newBuild = UpgradeDependencies(new UpgradeLogger(started.logger), upgrades)(build)
    val newBuild1 = normalizeBuild(newBuild)
    yaml.writeShortened(newBuild1.file, started.buildPaths.bleepYamlFile)

    Right(())
  }
  class UpgradeLogger(logger: Logger) extends UpgradeDependencies.UpgradeLogger {
    override def upgraded(project: CrossProjectName, dep: Dep, newVersion: String): Unit =
      logger
        .withContext(project)
        .info(s"${dep.organization.value}:${dep.baseModuleName.value} ${dep.version} => $newVersion")
  }

  def instantiateAllDependencies(build: model.Build): Map[UpgradeDependencies.ContextualDep, Dependency] =
    build.explodedProjects
      .flatMap { case (crossName, p) =>
        val versionCombo = model.VersionCombo.fromExplodedProject(p).orThrowTextWithContext(crossName)
        p.dependencies.values.iterator.collect {
          case dep if !dep.version.contains("$") => ((dep, versionCombo), dep.asDependency(versionCombo).orThrowTextWithContext(crossName))
        }
      }

  def fetchAllVersions[K](fileCache: FileCache[Task], repos: List[Repository], allDeps: Map[K, Dependency])(implicit
      ec: ExecutionContext
  ): Future[Map[K, (Dependency, Versions)]] = {
    val futures: immutable.Iterable[Future[List[(K, (Dependency, Versions))]]] =
      allDeps
        // deduplicate to save network traffic
        .groupBy { case (_, dep) => dep }
        .map { case (csDep, bleepDeps) =>
          fetchVersions(fileCache, repos, csDep).map { maybeFound =>
            for {
              found <- maybeFound.toList
              bleepDep <- bleepDeps
            } yield (bleepDep._1, (csDep, found))
          }
        }

    Future.sequence(futures).map(_.flatten.toMap)
  }

  def fetchVersions(fileCache: FileCache[Task], repos: List[Repository], csDep: Dependency)(implicit ec: ExecutionContext): Future[Option[Versions]] =
    repos match {
      case repo :: restRepos =>
        repo.versions(csDep.module, fileCache.fetch, versionsCheckHasModule = true).run.future().transformWith {
          case Success(Right((versions, _))) => Future.successful(Some(versions))
          case _                             => fetchVersions(fileCache, restRepos, csDep)
        }
      case Nil => Future.successful(None)
    }
}
