package bleep.internal

import bleep.logging.Logger
import bleep.yaml
import io.circe.Encoder

import java.nio.file.Path

object writeYamlLogged {
  def apply[T: Encoder](logger: Logger, message: String, t: T, to: Path): Unit =
    FileUtils.writeString(logger, Some(message), to, yaml.encodeShortened(t))
}
