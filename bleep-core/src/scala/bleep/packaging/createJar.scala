package bleep
package packaging

import java.io.ByteArrayOutputStream
import java.nio.file.attribute.FileTime
import java.nio.file.{Files, Path}
import java.util.jar.{JarEntry, JarOutputStream}
import scala.collection.mutable

object createJar {
  // sbt/sbt#6254
  private val epochTime = FileTime.fromMillis(1262304000000L)

  def apply(
      jarType: JarType,
      manifestCreator: ManifestCreator,
      fromFolders: Iterable[Path],
      projectName: Option[model.CrossProjectName] = None,
      mainClass: Option[String] = None
  ): Array[Byte] = {
    val manifest = manifestCreator(jarType, projectName, mainClass)
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
                seen.add(mapping).discard()
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
}
