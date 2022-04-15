package bleep.packaging

import bleep.constants
import bleep.logging.Logger
import bloop.config.Config

import java.nio.file._
import java.nio.file.attribute.FileTime
import java.util.jar.{Attributes, JarOutputStream, Manifest}
import java.util.zip.ZipEntry
import scala.util.Using

object PackagePlugin {
  private val epochTime = FileTime.fromMillis(0)

  def apply(logger: Logger, projects: List[Config.Project]): List[Path] =
    projects.map { project =>
      val writtenJar = jar(logger, project)
      logger.withContext(writtenJar).info("built jar file")
      writtenJar
    }

  def buildManifest(projectName: String, mainClass: Option[String]): Manifest = {
    val manifest = new Manifest()
    val attrs = manifest.getMainAttributes
    attrs.put(Attributes.Name.IMPLEMENTATION_TITLE, projectName)
    attrs.put(new Attributes.Name("Created-By"), s"Bleep/${constants.version}")
    mainClass.foreach(cls => attrs.put(Attributes.Name.MAIN_CLASS, cls))
    manifest
  }

  def jar(logger: Logger, project: Config.Project): Path = {
    val dirs = project.classesDir :: project.resources.getOrElse(Nil)
    val jarFile = project.out.resolve(s"${project.name}.jar")
    if (Files.deleteIfExists(jarFile)) {
      logger.withContext(jarFile).debug("Deleted existing file")
    }
    val manifest = buildManifest(project.name, project.platform.flatMap(_.mainClass))
    Using.resource(new JarOutputStream(Files.newOutputStream(jarFile, StandardOpenOption.CREATE_NEW), manifest)) { os =>
      dirs.filter(Files.exists(_)).foreach { resourceDir =>
        addFilesToJar(resourceDir, os)
      }
    }
    jarFile
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
//      os.write(Files.readAllBytes(file))

      Using.resource(Files.newInputStream(file))(is => is.transferTo(os))
    }
    ()
  }
}
