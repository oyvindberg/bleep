package bleep.internal

import bloop.config.{Config, ConfigCodecs}
import com.github.plokhotnyuk.jsoniter_scala.core.readFromString

object parseBloopFile {
  def apply(contents: String): Config.File =
    readFromString(contents)(ConfigCodecs.codecFile)
}
