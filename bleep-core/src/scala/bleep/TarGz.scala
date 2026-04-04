package bleep

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.nio.file.{Files, Path}
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import scala.jdk.StreamConverters.*

/** Pack and unpack tar.gz archives from directory trees. Uses only JDK built-ins (no external dependencies).
  *
  * Implements only the subset of tar needed for caching: regular files with relative paths, no symlinks/hardlinks/devices.
  */
object TarGz {

  /** Create a tar.gz archive from a directory tree.
    *
    * @param root
    *   the directory to archive
    * @return
    *   gzipped tar bytes
    */
  def pack(root: Path): Array[Byte] = {
    val files = scala.util
      .Using(Files.walk(root)) { stream =>
        stream.toScala(List).filter(Files.isRegularFile(_)).sorted
      }
      .getOrElse(Nil)

    val bout = new ByteArrayOutputStream()
    val gzOut = new GZIPOutputStream(bout)

    files.foreach { file =>
      val relPath = root.relativize(file).toString.replace('\\', '/')
      val content = Files.readAllBytes(file)
      writeTarEntry(gzOut, relPath, content)
    }

    // Two 512-byte zero blocks mark end of archive
    gzOut.write(new Array[Byte](1024))
    gzOut.close()
    bout.toByteArray
  }

  /** Extract a tar.gz archive into a directory.
    *
    * @param archive
    *   gzipped tar bytes
    * @param targetDir
    *   directory to extract into (created if needed)
    */
  def unpack(archive: Array[Byte], targetDir: Path): Unit = {
    // Extract to a temp dir first, then rename for atomicity.
    // This prevents partial state if the process is killed mid-extraction.
    val tmpDir = targetDir.resolveSibling(targetDir.getFileName.toString + ".cache-tmp-" + ProcessHandle.current().pid())
    try {
      Files.createDirectories(tmpDir)
      val gzIn = new GZIPInputStream(new ByteArrayInputStream(archive))
      val headerBuf = new Array[Byte](512)

      var done = false
      while (!done) {
        val bytesRead = readFully(gzIn, headerBuf)
        if (bytesRead < 512 || isZeroBlock(headerBuf)) {
          done = true
        } else {
          val name = extractString(headerBuf, 0, 100)
          val size = extractOctal(headerBuf, 124, 12)

          if (name.nonEmpty && size >= 0) {
            val content = new Array[Byte](size.toInt)
            readFully(gzIn, content)

            // Skip padding to 512-byte boundary
            val padding = (512 - (size % 512).toInt) % 512
            if (padding > 0) gzIn.skipNBytes(padding)

            val targetFile = tmpDir.resolve(name)
            Files.createDirectories(targetFile.getParent)
            Files.write(targetFile, content)
        }
      }
    }

      gzIn.close()

      // Move extracted files into target dir
      Files.createDirectories(targetDir)
      scala.util.Using(Files.list(tmpDir)) { stream =>
        stream.forEach { entry =>
          val dest = targetDir.resolve(tmpDir.relativize(entry))
          // Delete existing target if present, then move
          if (Files.isDirectory(dest)) internal.FileUtils.deleteDirectory(dest)
          else Files.deleteIfExists(dest)
          Files.move(entry, dest, java.nio.file.StandardCopyOption.ATOMIC_MOVE)
        }
      }
    } finally {
      // Clean up temp dir
      try internal.FileUtils.deleteDirectory(tmpDir)
      catch { case _: Exception => () }
    }
  }

  // ============================================================================
  // Tar format helpers (POSIX ustar subset)
  // ============================================================================

  private def writeTarEntry(out: java.io.OutputStream, name: String, content: Array[Byte]): Unit = {
    val header = new Array[Byte](512)
    val nameBytes = name.getBytes("UTF-8")
    System.arraycopy(nameBytes, 0, header, 0, math.min(nameBytes.length, 100))

    // File mode: 0644
    writeOctal(header, 100, 8, 0x1a4)
    // UID/GID: 0
    writeOctal(header, 108, 8, 0)
    writeOctal(header, 116, 8, 0)
    // Size
    writeOctal(header, 124, 12, content.length.toLong)
    // Mtime: 0 (reproducible)
    writeOctal(header, 136, 12, 0)
    // Type: regular file
    header(156) = '0'.toByte
    // USTAR magic
    "ustar\u0000".getBytes("UTF-8").copyToArray(header, 257)
    // USTAR version
    "00".getBytes("UTF-8").copyToArray(header, 263)

    // Compute checksum (sum of all bytes in header, treating checksum field as spaces)
    java.util.Arrays.fill(header, 148, 156, ' '.toByte)
    val checksum = header.map(b => (b.toInt & 0xff).toLong).sum
    writeOctal(header, 148, 7, checksum)
    header(155) = ' '.toByte

    out.write(header)
    out.write(content)

    // Pad to 512-byte boundary
    val padding = (512 - (content.length % 512)) % 512
    if (padding > 0) out.write(new Array[Byte](padding))
  }

  private def writeOctal(buf: Array[Byte], offset: Int, length: Int, value: Long): Unit = {
    val octal = String.format(s"%${length - 1}o", value).replace(' ', '0')
    octal.getBytes("UTF-8").copyToArray(buf, offset)
    buf(offset + length - 1) = 0
  }

  private def extractString(buf: Array[Byte], offset: Int, length: Int): String = {
    val end = buf.indexOf(0, offset).min(offset + length)
    val actualEnd = if (end < offset) offset + length else end
    new String(buf, offset, actualEnd - offset, "UTF-8").trim
  }

  private def extractOctal(buf: Array[Byte], offset: Int, length: Int): Long = {
    val s = extractString(buf, offset, length).trim
    if (s.isEmpty) 0L
    else java.lang.Long.parseLong(s, 8)
  }

  private def isZeroBlock(buf: Array[Byte]): Boolean =
    buf.forall(_ == 0)

  private def readFully(in: java.io.InputStream, buf: Array[Byte]): Int = {
    var total = 0
    while (total < buf.length) {
      val n = in.read(buf, total, buf.length - total)
      if (n < 0) return total
      total += n
    }
    total
  }

  private implicit class ArrayIndexOfOps(val arr: Array[Byte]) extends AnyVal {
    def indexOf(b: Byte, from: Int): Int = {
      var i = from
      while (i < arr.length) {
        if (arr(i) == b) return i
        i += 1
      }
      -1
    }
  }
}
