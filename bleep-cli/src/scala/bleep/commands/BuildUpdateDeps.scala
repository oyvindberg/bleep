package bleep
package commands

import bleep.commands.BuildUpdateDeps.ContextualDep
import bleep.internal.CoursierLogger
import bleep.logging.Logger
import bleep.rewrites.{normalizeBuild, BuildRewrite}
import coursier.Repository
import coursier.cache.FileCache
import coursier.core.{Dependency, Versions}
import coursier.util.Task

import scala.collection.immutable
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Success

case class BuildUpdateDeps(started: Started) extends BleepCommand {
  override def run(): Either[BleepException, Unit] = {
    val build = started.build.requireFileBacked("command update-deps")
    // a bleep dependency may be instantiated into several different coursier dependencies
    // depending on which scala versions and platforms are plugging in
    // collect all instantiations into this structure
    val allDeps: Map[ContextualDep, Dependency] =
      BuildUpdateDeps.instantiateAllDependencies(build)

    val config = started.lazyConfig.forceGet
    val repos = CoursierResolver.coursierRepos(build.resolvers.values, config.authentications).filter(_.repr.contains("http"))
    val fileCache = FileCache[Task]().withLogger(new CoursierLogger(started.logger))

    val foundByDep: Map[ContextualDep, (Dependency, Versions)] = {
      implicit val ec: ExecutionContext = started.executionContext
      Await.result(BuildUpdateDeps.fetchAllVersions(fileCache, repos, allDeps), Duration.Inf)
    }

    val upgrades: Map[ContextualDep, model.Dep] =
      foundByDep.flatMap { case (tuple @ (bleepDep, _), (_, version)) =>
        val latest = version.latest
        if (latest == bleepDep.version) None
        else Some(tuple -> bleepDep.withVersion(latest))
      }

    val newBuild = BuildUpdateDeps.upgradedBuild(started.logger, upgrades, build)
    val newBuild1 = normalizeBuild(newBuild)
    yaml.writeShortened(newBuild1.file, started.buildPaths.bleepYamlFile)

    Right(())
  }
}

object BuildUpdateDeps {
  type ContextualDep = (model.Dep, model.VersionCombo)

  def instantiateAllDependencies(build: model.Build): Map[ContextualDep, Dependency] =
    build.explodedProjects
      .flatMap { case (crossName, p) =>
        val versionCombo = model.VersionCombo.unsafeFromExplodedProject(p, crossName)
        p.dependencies.values.iterator.collect {
          case dep if !dep.version.contains("$") => ((dep, versionCombo), dep.unsafeAsDependency(Some(crossName), versionCombo))
        }
      }

  def upgradedBuild(logger: Logger, upgrades: Map[ContextualDep, model.Dep], build: model.Build.FileBacked): model.Build.FileBacked = {
    val newProjects = build.explodedProjects.map { case (crossName, p) =>
      val versionCombo = model.VersionCombo.unsafeFromExplodedProject(p, crossName)
      val newDeps = p.dependencies.map { dep =>
        upgrades.get((dep, versionCombo)) match {
          case Some(newDep) =>
            logger
              .withContext("project", crossName)
              .info(s"${dep.organization.value}:${dep.baseModuleName.value} ${dep.version} => ${newDep.version}")
            newDep
          case None => dep
        }
      }
      (crossName, p.copy(dependencies = newDeps))
    }

    BuildRewrite.withProjects(build, newProjects)
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
