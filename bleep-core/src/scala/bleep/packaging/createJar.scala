package bleep.packaging

import bleep.{model, RelPath}

import java.io.ByteArrayOutputStream
import java.nio.file.attribute.FileTime
import java.nio.file.{Files, Path}
import java.util.jar.{Attributes, JarEntry, JarOutputStream, Manifest}
import scala.collection.mutable

object createJar {
  // sbt/sbt#6254
  private val epochTime = FileTime.fromMillis(1262304000000L)

  def apply(fromFolders: Iterable[Path], projectName: Option[model.CrossProjectName] = None, mainClass: Option[String] = None): Array[Byte] = {
    val manifest = createManifest(projectName, mainClass)
    val seen = mutable.Set[RelPath](RelPath.force("META-INF") / "MANIFEST.MF")
    val baos = new ByteArrayOutputStream(1024 * 1024)
    val jar = new JarOutputStream(baos, manifest)

    try
      fromFolders.foreach { fromFolder =>
        if (Files.exists(fromFolder))
          Files.walk(fromFolder).forEachOrdered { file =>
            if (Files.isRegularFile(file)) {
              val mapping = RelPath.relativeTo(fromFolder, file)
              if (!seen(mapping)) {
                seen.add(mapping)
                val entry = new JarEntry(mapping.toString)
                entry.setCreationTime(epochTime)
                entry.setLastModifiedTime(epochTime)
                entry.setLastAccessTime(epochTime)

                jar.putNextEntry(entry)
                jar.write(Files.readAllBytes(file))
                jar.closeEntry()
              }
            }
          }
      }
    finally jar.close()

    baos.toByteArray
  }

  private def createManifest(projectName: Option[model.CrossProjectName], mainClass: Option[String]): Manifest = {
    val m = new java.util.jar.Manifest()
    val attrs = m.getMainAttributes
    attrs.put(java.util.jar.Attributes.Name.MANIFEST_VERSION, "1.0")
    attrs.putValue("Created-By", s"Bleep/${model.BleepVersion.current.value}")
    projectName.foreach(x => attrs.put(Attributes.Name.IMPLEMENTATION_TITLE, x.value))
    mainClass.foreach(x => attrs.put(Attributes.Name.MAIN_CLASS, x))
    m
  }
}
