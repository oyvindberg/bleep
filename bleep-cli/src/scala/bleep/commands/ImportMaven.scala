package bleep
package commands

import ryddig.Logger

import java.nio.file.{Files, Path}
import scala.concurrent.ExecutionContext

case class ImportMaven(
    mavenBuildDir: Path,
    destinationPaths: BuildPaths,
    userPaths: UserPaths,
    logger: Logger,
    options: mavenimport.MavenImportOptions,
    bleepVersion: model.BleepVersion
) extends BleepCommand {
  override def run(): Either[BleepException, Unit] = {
    if (!options.skipMvn) {
      mavenimport.runMaven(logger, mavenBuildDir, destinationPaths, options.mvnPath)
    }

    val effectivePomPath = destinationPaths.bleepImportMavenDir / "effective-pom.xml"
    val mavenProjects = mavenimport.parsePom(mavenimport.MavenFs.Real, effectivePomPath)

    logger.info(s"Parsed ${mavenProjects.size} Maven module(s)")

    val generatedBuildFiles = mavenimport
      .generateBuildFromMaven(
        destinationPaths = destinationPaths,
        logger = logger,
        options = options,
        bleepVersion = bleepVersion,
        bleepTasksVersion = model.BleepVersion(model.Replacements.known.BleepVersion),
        fs = mavenimport.MavenFs.Real,
        mavenProjects = mavenProjects,
        dependencyList = destinationPaths.bleepImportMavenDir / "dependency-list.txt"
      )
      .map { case (path, content) => (RelPath.relativeTo(destinationPaths.buildDir, path), content) }

    FileSync
      .syncStrings(destinationPaths.buildDir, generatedBuildFiles, deleteUnknowns = FileSync.DeleteUnknowns.No, soft = false)
      .log(logger, "Wrote build files")

    reportResolution(mavenProjects)
  }

  /** Resolves the imported build and says where it resolves libraries differently from maven. See [[mavenimport.ResolutionReport]] */
  private def reportResolution(mavenProjects: List[mavenimport.MavenProject]): Either[BleepException, Unit] = {
    val pre = Prebootstrapped(logger, userPaths, destinationPaths, BuildLoader.Existing(destinationPaths.bleepYamlFile), ExecutionContext.global)
    val config = BleepConfigOps.loadOrDefault(userPaths).orThrow
    bootstrap.from(pre, ResolveProjects.InMemory, rewrites = Nil, config, CoursierResolver.Factory.default).map { started =>
      val classpaths = started.resolvedProjects.collect {
        case (crossName, resolved) if crossName.name.value != "scripts" =>
          val project = resolved.forceGet
          (crossName, (project.classpath(Usage.Compile) ++ project.classpath(Usage.Runtime)).distinct)
      }
      val mavenResolved = mavenimport.parseDependencyList(Files.readString(destinationPaths.bleepImportMavenDir / "dependency-list.txt"))
      val report = mavenimport.ResolutionReport(mavenProjects, mavenResolved, classpaths)
      val reportFile = destinationPaths.bleepImportMavenDir / "resolution-report.txt"
      Files.writeString(reportFile, report.render)
      if (report.isEmpty) logger.info(s"bleep resolves the same libraries as maven for all ${report.compared} projects")
      else {
        logger
          .withContext("report", reportFile)
          .warn(
            s"bleep resolves some libraries differently from maven in ${report.differences.size} of ${report.compared} projects: maven lets the nearest version win, bleep the highest. Most common:"
          )
        report.mostCommon.take(10).foreach { case (difference, projects) => logger.warn(f"$projects%5d  $difference") }
      }
    }
  }
}
