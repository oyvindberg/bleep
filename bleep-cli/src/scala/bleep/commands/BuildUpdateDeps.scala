package bleep
package commands

import bleep.internal.{CoursierLogger, FileUtils}
import bleep.toYaml.asYamlString
import coursier.Repository
import coursier.cache.FileCache
import coursier.core.{Dependency, Versions}
import coursier.util.Task

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Success

case class BuildUpdateDeps(started: Started) extends BleepCommand {
  override def run(): Either[BleepException, Unit] = {
    // a bleep dependency may be instantiated into several different coursier dependencies
    // depending on which scala versions and platforms are plugging in
    // collect all instantiations into this structure
    val allDeps: Map[model.Dep, List[Dependency]] =
      BuildUpdateDeps.instantiateAllDependencies(started.build)

    val config = started.lazyConfig.forceGet
    val repos = CoursierResolver.coursierRepos(started.rawBuild.resolvers.values, config.authentications).filter(_.repr.contains("http"))
    val fileCache = FileCache[Task]().withLogger(new CoursierLogger(started.logger))

    val foundByDep: Map[model.Dep, List[Versions]] = {
      implicit val ec: ExecutionContext = started.executionContext
      Await.result(BuildUpdateDeps.fetchAllVersions(fileCache, repos, allDeps), Duration.Inf)
    }

    val upgrades: Map[model.Dep, model.Dep] =
      foundByDep.flatMap { case (bleepDep, versions) =>
        // one particular scala/platform combination may be dropped. let's say it'll hold the rest back for now, it's the easiest.
        val latest = versions.map(_.latest).min
        if (latest == bleepDep.version) None
        else {
          started.logger.withContext("dep", bleepDep.repr).info(s"Upgraded to $latest")
          Some(bleepDep -> bleepDep.withVersion(latest))
        }
      }

    val newBuild = BuildUpdateDeps.upgradedBuild(upgrades, started.rawBuild)

    FileUtils.writeString(
      started.buildPaths.bleepYamlFile,
      asYamlString(newBuild)
    )

    Right(())
  }
}

object BuildUpdateDeps {
  def instantiateAllDependencies(build: model.ExplodedBuild): Map[model.Dep, List[Dependency]] =
    build.projects.toList
      .flatMap { case (crossName, p) =>
        model.VersionScalaPlatform.fromExplodedProject(p) match {
          case Left(err) =>
            throw new BleepException.Text(crossName, err)
          case Right(scalaPlatform) =>
            p.dependencies.values.iterator.collect {
              case dep if !dep.version.contains("$") => (dep, dep.dependencyForce(crossName, scalaPlatform))
            }
        }
      }
      .groupBy { case (bleepDep, _) => bleepDep }
      .map { case (bleepDep, list) => (bleepDep, list.map { case (_, csDep) => csDep }.distinct) }

  def upgradedBuild(upgrades: Map[model.Dep, model.Dep], build: model.Build): model.Build = {
    def go(p: model.Project): model.Project =
      p.copy(
        cross = model.JsonMap(p.cross.value.map { case (crossId, p) => (crossId, go(p)) }),
        dependencies = p.dependencies.map(dep => upgrades.getOrElse(dep, dep))
      )

    build.copy(
      projects = model.JsonMap(build.projects.value.map { case (projectName, p) => (projectName, go(p)) }),
      templates = model.JsonMap(build.templates.value.map { case (templateId, p) => (templateId, go(p)) })
    )
  }

  def fetchAllVersions(fileCache: FileCache[Task], repos: List[Repository], allDeps: Map[model.Dep, List[Dependency]])(implicit
      ec: ExecutionContext
  ): Future[Map[model.Dep, List[Versions]]] =
    Future
      .sequence {
        allDeps.map { case (bleepDep, csDeps) =>
          Future.sequence(csDeps.map(csDep => fetchVersions(fileCache, repos, csDep))).map(ofs => (bleepDep, ofs))
        }
      }
      .map(_.collect { case (dep, maybeFounds) => (dep, maybeFounds.flatten) }.toMap)

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
