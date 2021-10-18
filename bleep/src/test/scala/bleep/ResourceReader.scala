package bleep

import java.io.InputStream
import scala.io.{Codec, Source}

object ResourceReader {
  def resourceAsStream(resourcePath: String): InputStream = {
    val stream = getClass.getResourceAsStream(resourcePath)
    if (stream == null)
      throw new IllegalArgumentException(s"Resource not found: $resourcePath")
    stream
  }

  def resourceAsString(resourcePath: String, codec: Codec = Codec.UTF8): String =
    Source.fromInputStream(resourceAsStream(resourcePath), codec.name).mkString
}
