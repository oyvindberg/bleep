package bleep.packaging

import bleep.internal.FileUtils
import bleep.logging.Logger
import bloop.config.Config
import bloop.config.Config.Platform

import java.nio.file._
import java.nio.file.attribute.FileTime
import java.util.jar.{Attributes, JarOutputStream, Manifest}
import java.util.zip.ZipEntry
import scala.util.Using

object PackagePlugin {
  private val epochTime = FileTime.fromMillis(0)

  def run(logger: Logger, projects: List[Config.Project], cmd: PackageCommand): Either[String, Unit] = {
    val filtered: List[Config.Project] = cmd match {
      case PackageCommand.Jars(Nil) =>
        projects
      case PackageCommand.Jars(requestedProjects) =>
        projects.collect { case p if requestedProjects.contains(p.name) => p }
      case PackageCommand.Dist(project, _, _) =>
        projects.collect { case p if p.name == project => p }
    }

    val dependencyLookup = projects.map(p => p.classesDir -> p).toMap

    filtered.foreach { project =>
      cmd match {
        case PackageCommand.Jars(_) =>
          jar(logger, project) match {
            case Right(jar)  => logger.withContext(jar).info("built jar file")
            case Left(error) => logger.error(error)
          }
        case PackageCommand.Dist(_, programs, distPath) =>
          val distDir = distPath.map(_.resolve(project.name)).getOrElse(project.out.resolve("dist"))
          Files.createDirectories(distDir)
          val lib = distDir.resolve("lib")
          FileUtils.deleteDirectory(lib)
          Files.createDirectories(lib)

          val jarFiles = dependenciesFor(logger, project, dependencyLookup).distinct

          jarFiles.foreach { src =>
            Files.copy(src, lib.resolve(src.getFileName), StandardCopyOption.COPY_ATTRIBUTES)
          }
          if (programs.nonEmpty) {
            val bin = distDir.resolve("bin")
            FileUtils.deleteDirectory(bin)
            Files.createDirectories(bin)
            Scripts.writeScripts(bin, "", programs)
          }

          logger.withContext(distDir).info("dist complete")
      }
    }
    Right(())
  }

  def dependenciesFor(logger: Logger, project: Config.Project, lookup: Map[Path, (Config.Project)]): List[Path] = {
    val (dirs, jars) = project.classpath.partition(Files.isDirectory(_))
    val mayBeJar = jar(logger, project)
    mayBeJar.toOption.toList ::: jars ::: dirs
      .flatMap(dir => lookup.get(dir).toList)
      .flatMap(dependantProject => dependenciesFor(logger, dependantProject, lookup))
  }

  def buildManifest(project: Config.Project, jvm: Config.Platform.Jvm): Manifest = {
    val manifest = new Manifest()
    manifest.getMainAttributes.put(Attributes.Name.IMPLEMENTATION_TITLE, project.name)
    jvm.mainClass.foreach { cls =>
      manifest.getMainAttributes.put(Attributes.Name.MAIN_CLASS, cls)
    }
    manifest
  }

  def jar(logger: Logger, project: Config.Project): Either[String, Path] =
    project.platform match {
      case Some(jvm: Platform.Jvm) =>
        val jarFile = project.out.resolve(s"${project.name}-jvm.jar")
        val resourceDirectories = project.resources.getOrElse(Nil)
        if (Files.deleteIfExists(jarFile)) {
          logger.withContext(jarFile).debug("Deleted existing file")
        }
        val manifest = buildManifest(project, jvm)
        Using.resource(new JarOutputStream(Files.newOutputStream(jarFile, StandardOpenOption.CREATE_NEW), manifest)) { os =>
          addFilesToJar(project.classesDir, os)
          resourceDirectories.filter(Files.exists(_)).foreach { resourceDir =>
            addFilesToJar(resourceDir, os)
          }
        }
        logger.withContext(jarFile).debug("Wrote file")
        Right(jarFile)
      case other =>
        Left(s"Cannot package for platform ${other.map(_.name)}")
    }

  private def addFilesToJar(root: Path, os: JarOutputStream): Unit =
    Files.walk(root).forEachOrdered { file =>
      val name = root.relativize(file).toString
      if (name != "bloop-internal-classes" && name.nonEmpty) {
        addJarEntry(os, file, name, Files.isDirectory(file, LinkOption.NOFOLLOW_LINKS))
      }
    }

  private def addJarEntry(os: JarOutputStream, file: Path, name: String, directory: Boolean): Unit = {
    val entry = new ZipEntry(if (directory) s"$name/" else name)
    os.putNextEntry(entry)

    entry.setCreationTime(epochTime)
    entry.setLastModifiedTime(epochTime)
    entry.setLastAccessTime(epochTime)
    if (!directory) {
      entry.setMethod(ZipEntry.DEFLATED)
      entry.setSize(Files.size(file))
      Using.resource(Files.newInputStream(file))(is => is.transferTo(os))
    }
    ()
  }
}
