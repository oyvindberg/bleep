package bleep

import fansi.Str

package object logging {
  type Ctx = Map[String, Str]
  type Logger = TypedLogger[Unit]
  val Logger = TypedLogger
  type LoggerResource = TypedLoggerResource[Unit]
  val LoggerResource = TypedLoggerResource
}
