package bleep
package packaging

import java.security.MessageDigest
import java.util

// note: adapted from https://github.com/apache/ant-ivy/blob/master/src/java/org/apache/ivy/util/ChecksumHelper.java
object Checksums {

  case class Digest(bytes: Array[Byte]) {
    lazy val hexString: String = byteArrayToHexString(bytes)

    override def hashCode(): Int =
      util.Arrays.hashCode(bytes)

    override def equals(obj: Any): Boolean = obj match {
      case x: Digest => util.Arrays.equals(bytes, x.bytes)
      case _         => false
    }
  }

  sealed abstract class Algorithm(val name: String)
  object Algorithm {
    case object Md5 extends Algorithm("md5")
    case object Sha1 extends Algorithm("sha1")
  }

  def apply(files: Map[RelPath, Array[Byte]], algorithms: List[Algorithm]): Map[RelPath, Array[Byte]] =
    files.flatMap { case in @ (relPath, content) =>
      val res = algorithms.map(alg => relPath.withLast(_ + "." + alg.name) -> compute(content, alg).hexString.getBytes)
      Map(in) ++ res
    }

  def compute(content: Array[Byte], algorithm: Algorithm): Digest = {
    val md = MessageDigest.getInstance(algorithm.name)
    md.reset()
    md.update(content)
    Digest(md.digest)
  }

  // byte to hex string converter
  val Chars = Array[Char]('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f')

  /** Convert a byte[] array to readable string format. This makes the "hex" readable!
    *
    * @return
    *   result String buffer in String format
    * @param in
    *   byte[] buffer to convert to string format
    */
  def byteArrayToHexString(in: Array[Byte]): String = {
    var ch: Byte = 0x00
    if (in == null || in.length <= 0) return null
    val out = new StringBuffer(in.length * 2)
    // CheckStyle:MagicNumber OFF
    for (i <- in.indices) {
      ch = (in(i) & 0xf0).toByte // Strip off high nibble
      ch = (ch >>> 4).toByte // shift the bits down
      ch = (ch & 0x0f).toByte // must do this is high order bit is on!
      out.append(Chars(ch.toInt)) // convert the nibble to a String Character
      ch = (in(i) & 0x0f).toByte // Strip off low nibble
      out.append(Chars(ch.toInt))
    }
    // CheckStyle:MagicNumber ON
    out.toString
  }
}
