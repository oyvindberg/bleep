package bleep

import java.io.ByteArrayOutputStream
import java.nio.file.{Files, Path}
import java.util.jar.{JarEntry, JarOutputStream, Manifest}
import scala.collection.mutable

object createJar {
  def apply(fromFolders: Iterable[Path], manifest: Manifest = createManifest()): Array[Byte] = {
    val seen = mutable.Set[RelPath](RelPath.force("META-INF") / "MANIFEST.MF")
    val baos = new ByteArrayOutputStream(1024 * 1024)
    val jar = new JarOutputStream(baos, manifest)

    try
      fromFolders.foreach { fromFolder =>
        if (Files.exists(fromFolder))
          Files.walk(fromFolder).forEach { file =>
            if (Files.isRegularFile(file)) {
              val mapping = RelPath.relativeTo(fromFolder, file)
              if (!seen(mapping)) {
                seen.add(mapping)
                val entry = new JarEntry(mapping.toString)
                entry.setTime(0L)
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

  private def createManifest(): Manifest = {
    val m = new java.util.jar.Manifest()
    m.getMainAttributes.put(java.util.jar.Attributes.Name.MANIFEST_VERSION, "1.0")
    m.getMainAttributes.putValue("Created-By", s"Bleep/${model.BleepVersion.current.value}")
    m
  }
}
