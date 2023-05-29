package bleep.nosbt.io
/*
 * sbt IO
 *
 * Copyright 2011 - 2019, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 */

import java.io.*

object Hash {
  private val BufferSize = 8192

  /** Converts an array of `bytes` to a hexadecimal representation String. */
  def toHex(bytes: Array[Byte]): String = {
    val buffer = new StringBuilder(bytes.length * 2)
    for (i <- bytes.indices) {
      val b = bytes(i)
      val bi: Int = if (b < 0) b + 256 else b.toInt
      buffer append toHex((bi >>> 4).asInstanceOf[Byte])
      buffer append toHex((bi & 0x0f).asInstanceOf[Byte])
    }
    buffer.toString
  }

  /** Converts the provided hexadecimal representation `hex` to an array of bytes. The hexadecimal representation must have an even number of characters in the
    * range 0-9, a-f, or A-F.
    */
  def fromHex(hex: String): Array[Byte] = {
    require((hex.length & 1) == 0, "Hex string must have length 2n.")
    val array = new Array[Byte](hex.length >> 1)
    for (i <- 0 until hex.length by 2) {
      val c1 = hex.charAt(i)
      val c2 = hex.charAt(i + 1)
      array(i >> 1) = ((fromHex(c1) << 4) | fromHex(c2)).asInstanceOf[Byte]
    }
    array
  }

  /** Truncates the last half of `s` if the string has at least four characters.  Otherwise, the original string is returned. */
  def halve(s: String): String = if (s.length > 3) s.substring(0, s.length / 2) else s

  /** Computes the SHA-1 hash of `s` and returns the first `i` characters of the hexadecimal representation of the hash. */
  def trimHashString(s: String, i: Int): String = toHex(apply(s)).take(i)

  /** Computes the SHA-1 hash of `s` and truncates the hexadecimal representation of the hash via [[halve]]. */
  def halfHashString(s: String): String = halve(toHex(apply(s)))

  /** Calculates the SHA-1 hash of the given String. */
  def apply(s: String): Array[Byte] = apply(s.getBytes("UTF-8"))

  /** Calculates the SHA-1 hash of the given Array[Byte]. */
  def apply(as: Array[Byte]): Array[Byte] = apply(new ByteArrayInputStream(as))

  /** Calculates the SHA-1 hash of the given file. */
  def apply(file: File): Array[Byte] =
    try apply(new BufferedInputStream(new FileInputStream(file))) // apply closes the stream
    catch { case _: FileNotFoundException => apply("") }

  /** Calculates the SHA-1 hash of the given stream, closing it when finished. */
  def apply(stream: InputStream): Array[Byte] = {
    import java.security.{DigestInputStream, MessageDigest}
    val digest = MessageDigest.getInstance("SHA")
    try {
      val dis = new DigestInputStream(stream, digest)
      val buffer = new Array[Byte](BufferSize)
      while (dis.read(buffer) >= 0) {}
      dis.close()
      digest.digest
    } finally
      stream.close()
  }

  private def toHex(b: Byte): Char = {
    require(b >= 0 && b <= 15, "Byte " + b + " was not between 0 and 15")
    if (b < 10)
      ('0'.asInstanceOf[Int] + b).asInstanceOf[Char]
    else
      ('a'.asInstanceOf[Int] + (b - 10)).asInstanceOf[Char]
  }

  private def fromHex(c: Char): Int = {
    val b =
      if (c >= '0' && c <= '9')
        c - '0'
      else if (c >= 'a' && c <= 'f')
        (c - 'a') + 10
      else if (c >= 'A' && c <= 'F')
        (c - 'A') + 10
      else
        throw new RuntimeException("Invalid hex character: '" + c + "'.")
    b
  }
}
