package bleep
package commands

import bleep.internal.writeYamlLogged
import bleep.internal.Version
import bleep.rewrites.{normalizeBuild, UpgradeDependencies}
import coursier.Repository
import coursier.cache.FileCache
import coursier.core.{Dependency, Versions}
import coursier.util.Task
import ryddig.Logger

import scala.collection.immutable
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Success
import cats.parse.Parser
import bleep.model.Build

case class BuildUpdateDeps(scalaStewardMode: Boolean, allowPrerelease: Boolean, singleDep: Option[String]) extends BleepBuildCommand {

  override def run(started: Started): Either[BleepException, Unit] = {
    val build: Build.FileBacked = started.build.requireFileBacked("command update-deps")
    // a bleep dependency may be instantiated into several different coursier dependencies
    // depending on which scala versions and platforms are plugging in
    // collect all instantiations into this structure
    val allDeps: Map[UpgradeDependencies.ContextualDep, Dependency] =
      instantiateAllDependencies(build)

    val repos = CoursierResolver.coursierRepos(build.resolvers.values, started.config.authentications).filter(_.repr.contains("http"))
    val fileCache = FileCache[Task]().withLogger(started.pre.cacheLogger)

    val foundByDep: Map[UpgradeDependencies.ContextualDep, (Dependency, Versions)] = {
      implicit val ec: ExecutionContext = started.executionContext
      Await.result(fetchAllVersions(fileCache, repos, allDeps), Duration.Inf)
    }

    DependencyUpgrader.runUpgrade(singleDep, foundByDep, runUgrades(started, build))
  }

  private def runUgrades(
      started: Started,
      build: Build.FileBacked
  )(
      foundByDep: Map[UpgradeDependencies.ContextualDep, (Dependency, Versions)]
  ): Right[BleepException, Unit] = {

    val upgrades: Map[UpgradeDependencies.ContextualDep, model.Dep] =
      foundByDep.flatMap { case (tuple @ (bleepDep, _), (_, coursierVersion)) =>
        val version = Version(bleepDep.version)
        val latest =
          if (scalaStewardMode) {
            version.selectNext(coursierVersion.available.map(Version.apply), allowPrerelease)
          } else {
            version.selectLatest(coursierVersion.available.map(Version.apply), allowPrerelease)
          }
        latest.map(latestV => tuple -> bleepDep.withVersion(latestV.value))
      }

    val newBuild = UpgradeDependencies(new UpgradeLogger(started.logger), upgrades)(build, started.buildPaths)
    val newBuild1 = normalizeBuild(newBuild, started.buildPaths)
    Right(writeYamlLogged(started.logger, "Wrote update build", newBuild1.file, started.buildPaths.bleepYamlFile))
  }

  class UpgradeLogger(logger: Logger) extends UpgradeDependencies.UpgradeLogger {
    override def upgraded(project: model.CrossProjectName, dep: model.Dep, newVersion: String): Unit =
      logger
        .withContext("project", project.value)
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

  object DependencyUpgrader {
    private val sepParser = Parser.char(':').rep.void

    val parseSingleDep = Parser.anyChar.repUntil(sepParser).string ~ (sepParser *> Parser.anyChar.repUntil(Parser.end).string).?

    def runUpgrade(
        singleDep: Option[String],
        foundByDep: Map[UpgradeDependencies.ContextualDep, (Dependency, Versions)],
        runAllGivenUpgrades: Map[UpgradeDependencies.ContextualDep, (Dependency, Versions)] => Either[BleepException, Unit]
    ): Either[BleepException, Unit] = {
      val singleDepParsed = singleDep.map(dep => parseSingleDep.parseAll(dep))

      singleDepParsed match {
        case None            => runAllGivenUpgrades(foundByDep)
        case Some(parsedDep) => runSingleUpgrade(foundByDep, parsedDep, singleDep.getOrElse(""), runAllGivenUpgrades)
      }
    }

    private def runSingleUpgrade(
        foundByDep: Map[UpgradeDependencies.ContextualDep, (Dependency, Versions)],
        parsedDep: Either[Parser.Error, (String, Option[String])],
        depName: String,
        runAllGivenUpgrades: Map[UpgradeDependencies.ContextualDep, (Dependency, Versions)] => Either[BleepException, Unit]
    ) =
      parsedDep match {
        case Left(_) => Left(new BleepException.Text(s"${depName} is not a valid dependency name"))
        case Right((org, module)) =>
          val filteredDeps = foundByDep.filter { case ((bleepDep, _), _) =>
            module.map(bleepDep.baseModuleName.value.equalsIgnoreCase).getOrElse(true) && bleepDep.organization.value.equalsIgnoreCase(org)

          }
          runAllGivenUpgrades(filteredDeps)
      }

  }
}
