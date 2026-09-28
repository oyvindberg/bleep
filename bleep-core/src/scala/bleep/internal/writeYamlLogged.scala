package bleep.internal

import bleep.yaml
import io.circe.Encoder
import ryddig.Logger

import java.nio.file.{Files, Path}

object writeYamlLogged {

  /** Writes `t` to `to`. If `to` exists, its comments are carried over to the new content; any that no longer have a place are logged as dropped. */
  def apply[T: Encoder](logger: Logger, message: String, t: T, to: Path): Unit =
    if (Files.exists(to)) {
      val printed = yaml.encodeShortenedKeepingComments(t, Files.readString(to))
      printed.orphans.foreach { orphan =>
        logger.withContext("path", to).withContext("was at", orphan.anchor.render).warn(s"Dropped comment, its place is gone:\n${orphan.comments.render}")
      }
      FileUtils.writeString(logger, Some(message), to, printed.yaml)
    } else FileUtils.writeString(logger, Some(message), to, yaml.encodeShortened(t))
}
