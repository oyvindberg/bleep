package bleep
package commands

import bleep.internal.traverseish
import bleep.packaging.*
import bleep.publishing.MavenRemotePublisher
import coursier.core.Info

import java.net.URI
import java.nio.file.Path
import scala.collection.immutable.SortedMap

object Publish {
  sealed trait Target
  object Target {
    case object LocalIvy extends Target
    case class Resolver(name: model.ResolverName) extends Target
  }

  val ReservedNames: Set[String] = Set("local-ivy", "sonatype", "setup")

  case class Options(
      versionOverride: Option[String],
      versionFallback: Option[() => String],
      assertRelease: Boolean,
      dryRun: Boolean,
      target: Target,
      projectNames: Array[model.CrossProjectName],
      manifestCreator: ManifestCreator
  )

  /** Resolve which projects to publish. If explicit projects given, use those. Otherwise find all projects with `publish` config. */
  def resolveProjects(
      build: model.Build,
      explicitProjects: Array[model.CrossProjectName]
  ): Array[model.CrossProjectName] =
    if (explicitProjects.nonEmpty) explicitProjects
    else
      build.explodedProjects
        .collect {
          case (name, project)
              if project.publish.exists(_.isEnabled) &&
                !project.isTestProject.getOrElse(false) =>
            name
        }
        .toArray
        .sorted

  /** Validate that published projects don't depend on unpublished projects. */
  def validateDependencies(
      build: model.Build,
      publishableProjects: Set[model.CrossProjectName]
  ): Either[BleepException, Unit] = {
    val errors = List.newBuilder[String]
    publishableProjects.foreach { projectName =>
      val deps = build.resolvedDependsOn(projectName)
      deps.foreach { depName =>
        val depProject = build.explodedProjects(depName)
        val depIsPublished = depProject.publish.exists(_.isEnabled) && !depProject.isTestProject.getOrElse(false)
        if (!depIsPublished && !publishableProjects.contains(depName)) {
          errors += s"${projectName.value} depends on ${depName.value} which is not published"
        }
      }
    }
    val allErrors = errors.result()
    if (allErrors.nonEmpty)
      Left(
        new BleepException.Text(
          "Published projects must not depend on unpublished projects:\n" +
            allErrors.map(e => s"  - $e").mkString("\n")
        )
      )
    else Right(())
  }

  /** Extract groupId from a project's publish config. */
  private def requirePublishConfig(
      projectName: model.CrossProjectName,
      project: model.Project
  ): Either[BleepException, (model.PublishConfig, String)] =
    for {
      publishConfig <- project.publish.toRight(
        new BleepException.Text(s"Project ${projectName.value} has no 'publish' config"): BleepException
      )
      groupId <- publishConfig.groupId.toRight(
        new BleepException.Text(s"Project ${projectName.value} has no groupId in publish config"): BleepException
      )
    } yield (publishConfig, groupId)

  /** Convert a project's PublishConfig to coursier Info for POM generation. */
  def toInfo(publishConfig: model.PublishConfig, inferredScm: Option[Info.Scm]): Info =
    Info(
      description = publishConfig.description.getOrElse(""),
      homePage = publishConfig.url.getOrElse(""),
      developers = publishConfig.developers.values.toList.map { dev =>
        Info.Developer(dev.id, dev.name, dev.url)
      },
      publication = None,
      scm = inferredScm,
      licenseInfo = publishConfig.licenses.values.toList.map { lic =>
        Info.License(lic.name, lic.url, lic.distribution, None)
      }
    )

  /** Render a dry-run report showing what would be published. */
  def renderDryRun(
      logger: ryddig.Logger,
      projects: Array[model.CrossProjectName],
      version: String,
      target: Target,
      allArtifacts: Map[model.CrossProjectName, Map[RelPath, Array[Byte]]]
  ): Unit = {
    val targetDesc = target match {
      case Target.LocalIvy       => "local-ivy (~/.ivy2)"
      case Target.Resolver(name) => s"resolver '${name.value}'"
    }

    logger.info(s"Dry run: would publish to $targetDesc")
    logger.info(s"Version: $version")
    logger.info("")

    var totalFiles = 0
    var totalBytes = 0L

    allArtifacts.toList.sortBy(_._1.value).foreach { case (projectName, files) =>
      logger.info(s"${projectName.value}:")

      val sortedFiles = files.toList.sortBy(_._1.asString)
      val paths = sortedFiles.map(_._1.asString)
      val commonPrefix = paths.headOption.flatMap { first =>
        val parts = first.split("/")
        if (parts.length > 2) Some(parts.dropRight(1).mkString("/") + "/") else None
      }

      commonPrefix.foreach(p => logger.info(s"  $p"))
      val indent = if (commonPrefix.isDefined) "    " else "  "

      sortedFiles.zipWithIndex.foreach { case ((relPath, content), idx) =>
        val fileName = commonPrefix match {
          case Some(prefix) => relPath.asString.stripPrefix(prefix)
          case None         => relPath.asString
        }
        val isLast = idx == sortedFiles.size - 1
        val connector = if (isLast) "└── " else "├── "
        val size = formatSize(content.length)

        val isJar = fileName.endsWith(".jar")
        val isChecksum = fileName.endsWith(".md5") || fileName.endsWith(".sha1")

        if (isChecksum) {
          val checksumValue = new String(content, "UTF-8").trim
          logger.info(s"$indent$connector$fileName  ($size)  $checksumValue")
        } else if (isJar) {
          logger.info(s"$indent$connector$fileName  ($size)")
        } else {
          logger.info(s"$indent$connector$fileName  ($size)")
          val contentStr = new String(content, "UTF-8")
          val lines = contentStr.split("\n")
          val contentIndent = indent + (if (isLast) "    " else "│   ")
          val maxLines = 30
          val displayLines = if (lines.length > maxLines) lines.take(maxLines) else lines
          displayLines.foreach { line =>
            logger.info(s"$contentIndent$line")
          }
          if (lines.length > maxLines) {
            logger.info(s"$contentIndent... (${lines.length - maxLines} more lines)")
          }
        }

        totalFiles += 1
        totalBytes += content.length
      }
      logger.info("")
    }

    logger.info(s"Total: $totalFiles files, ${formatSize(totalBytes)}")
  }

  private def formatSize(bytes: Long): String =
    if (bytes < 1024) s"${bytes}B"
    else if (bytes < 1024 * 1024) f"${bytes / 1024.0}%.1fKB"
    else f"${bytes / (1024.0 * 1024.0)}%.1fMB"
}

/** Publishes artifacts locally or to a named resolver. */
case class Publish(watch: Boolean, options: Publish.Options, buildOpts: CommonBuildOpts) extends BleepBuildCommand {
  override def run(started: Started): Either[BleepException, Unit] = {
    val projects = Publish.resolveProjects(started.build, options.projectNames)
    if (projects.isEmpty)
      return Left(new BleepException.Text("No publishable projects found. Add 'publish' config to projects or specify project names."))

    for {
      _ <- Publish.validateDependencies(started.build, projects.toSet)
      _ <-
        if (watch) WatchMode.run(started, s => bleep.internal.TransitiveProjects(s.build, projects))(runOnce(projects))
        else runOnce(projects)(started)
    } yield ()
  }

  private def runOnce(projects: Array[model.CrossProjectName])(started: Started): Either[BleepException, Unit] =
    for {
      _ <- ReactiveBsp
        .compile(
          watch = false,
          projects = projects,
          displayMode = buildOpts.displayMode,
          flamegraph = buildOpts.flamegraph,
          cancel = buildOpts.cancel
        )
        .run(started)
      version <- resolveVersion(started)
      _ <- checkAssertRelease(version)
      _ <-
        if (options.dryRun) dryRun(started, projects, version)
        else
          options.target match {
            case Publish.Target.LocalIvy =>
              publishLocal(started, projects, version, constants.ivy2Path, PublishLayout.Ivy)
            case Publish.Target.Resolver(resolverName) =>
              publishToResolver(started, projects, version, resolverName)
          }
    } yield ()

  private def resolveVersion(started: Started): Either[BleepException, String] =
    options.versionOverride
      .orElse(options.versionFallback.map(_.apply()))
      .toRight(new BleepException.Text("No --version specified and no git tags found for automatic versioning."): BleepException)

  private def checkAssertRelease(version: String): Either[BleepException, Unit] =
    if (options.assertRelease && options.versionOverride.isEmpty && (version.contains("+") || version.endsWith("-SNAPSHOT")))
      Left(
        new BleepException.Text(
          s"--assert-release: version '$version' is not a release. " +
            "Ensure you are on a clean git tag (no commits after tag, no dirty files)."
        )
      )
    else Right(())

  /** Package all projects in one call, returning per-project artifacts. Reads groupId from each project's publish config. */
  private def packageAll(
      started: Started,
      projects: Array[model.CrossProjectName],
      version: String,
      layout: PublishLayout
  ): Either[BleepException, SortedMap[model.CrossProjectName, PackagedLibrary]] = {
    // Validate first project has publish config (for fallback groupId)
    val firstProject = started.build.explodedProjects(projects.head)
    Publish.requirePublishConfig(projects.head, firstProject).map { case (_, fallbackGroupId) =>
      packageLibraries(
        started,
        coordinatesFor = CoordinatesFor.FromModel(version = version, fallbackGroupId = fallbackGroupId),
        shouldInclude = projects.toSet,
        publishLayout = layout,
        manifestCreator = options.manifestCreator
      )
    }
  }

  private def dryRun(
      started: Started,
      projects: Array[model.CrossProjectName],
      version: String
  ): Either[BleepException, Unit] = {
    val firstProject = started.build.explodedProjects(projects.head)
    val publishConfig = firstProject.publish.getOrElse(
      return Left(new BleepException.Text(s"Project ${projects.head.value} has no 'publish' config"))
    )
    val info = Publish.toInfo(publishConfig, None)

    packageAll(started, projects, version, PublishLayout.Maven(info)).map { packagedLibraries =>
      val allArtifacts = packagedLibraries.map { case (pName, PackagedLibrary(_, files)) =>
        pName -> Checksums(files.all, List(Checksums.Algorithm.Md5, Checksums.Algorithm.Sha1))
      }
      Publish.renderDryRun(started.logger, projects, version, options.target, allArtifacts)
    }
  }

  private def publishLocal(
      started: Started,
      projects: Array[model.CrossProjectName],
      version: String,
      targetPath: Path,
      layout: PublishLayout
  ): Either[BleepException, Unit] =
    packageAll(started, projects, version, layout).map { packagedLibraries =>
      packagedLibraries.foreach { case (pName, PackagedLibrary(_, files)) =>
        FileSync
          .syncBytes(targetPath, files.all, deleteUnknowns = FileSync.DeleteUnknowns.No, soft = false)
          .log(started.logger.withContext("projectName", pName.value).withContext("version", version), "Published locally")
      }
    }

  private def publishToResolver(
      started: Started,
      projects: Array[model.CrossProjectName],
      version: String,
      resolverName: model.ResolverName
  ): Either[BleepException, Unit] = {
    val resolvers = started.build.resolvers.values
    for {
      repo <- resolvers
        .find(_.name.contains(resolverName))
        .toRight(
          new BleepException.Text(
            s"Resolver '${resolverName.value}' not found in bleep.yaml. " +
              s"Available named resolvers: ${resolvers.flatMap(_.name.map(_.value)).mkString(", ")}"
          ): BleepException
        )
      _ <- repo match {
        case model.Repository.Maven(_, uri) =>
          publishRemote(started, projects, version, uri)
        case model.Repository.Ivy(_, uri) =>
          publishRemote(started, projects, version, uri)
        case model.Repository.MavenFolder(_, path) =>
          publishLocal(started, projects, version, path, PublishLayout.Maven())
      }
    } yield ()
  }

  private def publishRemote(
      started: Started,
      projects: Array[model.CrossProjectName],
      version: String,
      repoUri: URI
  ): Either[BleepException, Unit] = {
    val credentialProvider = new CredentialProvider(started.logger, started.config.authentications)
    val resolved = MavenRemotePublisher.resolveTarget(repoUri, credentialProvider)
    val publisher = new MavenRemotePublisher(started.logger)

    val firstProject = started.build.explodedProjects(projects.head)
    val publishConfig = firstProject.publish.getOrElse(
      return Left(new BleepException.Text(s"Project ${projects.head.value} has no 'publish' config"))
    )
    val info = Publish.toInfo(publishConfig, None)

    packageAll(started, projects, version, PublishLayout.Maven(info)).map { packagedLibraries =>
      packagedLibraries.foreach { case (pName, PackagedLibrary(_, files)) =>
        val artifacts =
          if (resolved.uploadChecksums) Checksums(files.all, List(Checksums.Algorithm.Md5, Checksums.Algorithm.Sha1))
          else files.all
        val projectLogger = started.logger
          .withContext("projectName", pName.value)
          .withContext("version", version)
          .withContext("repository", resolved.httpsUri)
        projectLogger.info(s"Publishing ${artifacts.size} files")
        publisher.publish(resolved.httpsUri, artifacts, resolved.authentication)
      }
    }
  }
}
