package bleep

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import scala.jdk.CollectionConverters.*

/** A cheap identity for build outputs: names, then every file under the given paths by relative path, size and modification time. Stat only, no content reads,
  * so it can be taken on every noop check. Not a portable cache key (it includes absolute roots and mtimes) — it answers "is this still the build I produced
  * from", on this machine.
  */
object PathFingerprint {
  def of(names: List[String], paths: List[Path]): String = {
    val digest = MessageDigest.getInstance("SHA-256")
    def mix(s: String): Unit = {
      digest.update(s.getBytes(StandardCharsets.UTF_8))
      digest.update(0.toByte)
    }
    names.foreach(mix)
    paths.foreach { root =>
      mix(root.toString)
      if (Files.isDirectory(root)) {
        val stream = Files.walk(root)
        try
          stream.iterator().asScala.filter(Files.isRegularFile(_)).toList.sortBy(_.toString).foreach { f =>
            mix(root.relativize(f).toString)
            mix(Files.size(f).toString)
            mix(Files.getLastModifiedTime(f).toMillis.toString)
          }
        finally stream.close()
      } else if (Files.isRegularFile(root)) {
        mix(Files.size(root).toString)
        mix(Files.getLastModifiedTime(root).toMillis.toString)
      } else mix("absent") // a resource directory nobody created, or the output of a project without sources: an empty entry, as on any classpath
    }
    digest.digest().take(8).map(b => f"$b%02x").mkString
  }
}
