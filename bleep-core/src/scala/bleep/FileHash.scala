package bleep

import java.nio.file.Path
import java.security.MessageDigest
import java.io.OutputStream
import java.security.DigestOutputStream
import java.io.FileInputStream
import scala.util.Using

object FileHash {

  def apply(path: Path): Int = {
    val digest = MessageDigest.getInstance("MD5")
    val dummyOS = new OutputStream {
      def write(b: Int): Unit = ()
    }
    val digestOut = new DigestOutputStream(dummyOS, digest)
    Using.resource(new FileInputStream(path.toFile())) { is =>
      val buffer = new Array[Byte](4096)
      while (
        is.read(buffer) match {
          case -1 => false
          case n =>
            digestOut.write(buffer, 0, n)
            true
        }
      ) ()
    }
    java.util.Arrays.hashCode(digest.digest())
  }

}
