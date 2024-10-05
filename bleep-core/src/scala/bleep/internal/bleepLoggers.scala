package bleep
package internal

import ryddig.*

import java.io.{BufferedWriter, PrintStream}
import java.time.Instant

object bleepLoggers {
  /* Use this environment variable to communicate to subprocesses that parent accepts json events */
  val CallerProcessAcceptsJsonEvents = "CALLER_PROCESS_ACCEPTS_JSON_EVENTS"

  // all log events will start with this string following 0.0.1-M26.
  val BLEEP_JSON_EVENT = "BLEEP_JSON_EVENT:"

  def stdoutNoLogFile(bleepConfig: model.BleepConfig, commonOpts: CommonOpts): LoggerResource[PrintStream] =
    if (commonOpts.logAsJson) LoggerResource.flushable(Loggers.printJsonStream(BLEEP_JSON_EVENT, System.out))
    else baseStdout(bleepConfig, commonOpts).map(Loggers.decodeJsonStream(BLEEP_JSON_EVENT, _))

  def stdoutAndFileLogging(
      bleepConfig: model.BleepConfig,
      commonOpts: CommonOpts,
      buildPaths: BuildPaths
  ): LoggerResource[(PrintStream, Option[BufferedWriter])] =
    if (commonOpts.logAsJson) LoggerResource.pure(Loggers.printJsonStream(BLEEP_JSON_EVENT, System.out)).maybeZipWith(None)
    else {
      baseStdout(bleepConfig, commonOpts)
        .maybeZipWith(Some(Loggers.path(buildPaths.logFile, LogPatterns.logFile)))
        // it's an optimization to perform this before both loggers, so we don't need to do it twice
        .map(Loggers.decodeJsonStream(BLEEP_JSON_EVENT, _))
    }

  private def baseStdout(bleepConfig: model.BleepConfig, commonOpts: CommonOpts): LoggerResource[PrintStream] =
    Loggers
      .stdout(
        LogPatterns.interface(
          startRun = if (bleepConfig.logTiming.getOrElse(false)) Some(Instant.now) else None,
          noColor = commonOpts.noColor
        ),
        disableProgress = commonOpts.noBspProgress
      )
      .map(l => if (commonOpts.debug) l else l.withMinLogLevel(LogLevel.info))

  // we need to use stderr in some situations
  // 1) BSP where stdout is already used for BSP protocol
  // 2) auto-complete, when stdout is used to communicate completions
  // 3) early, right after boot if we haven't set up proper logging

  def stderrWarn(commonOpts: CommonOpts): TypedLogger[PrintStream] =
    if (commonOpts.logAsJson) Loggers.printJsonStream(BLEEP_JSON_EVENT, System.err)
    else Loggers.decodeJsonStream(BLEEP_JSON_EVENT, Loggers.stderr(LogPatterns.logFile).withMinLogLevel(LogLevel.warn))

  def stderrAll(commonOpts: CommonOpts): TypedLogger[PrintStream] =
    if (commonOpts.logAsJson) Loggers.printJsonStream(BLEEP_JSON_EVENT, System.err)
    else Loggers.decodeJsonStream(BLEEP_JSON_EVENT, Loggers.stderr(LogPatterns.logFile))

  def stderrAndFileLogging(
      bleepConfig: model.BleepConfig,
      commonOpts: CommonOpts,
      buildPaths: BuildPaths
  ): LoggerResource[(PrintStream, Option[BufferedWriter])] =
    if (commonOpts.logAsJson) LoggerResource.pure(Loggers.printJsonStream(BLEEP_JSON_EVENT, System.err)).maybeZipWith(None)
    else {
      LoggerResource
        .pure(
          Loggers.stderr(
            LogPatterns.interface(
              startRun = if (bleepConfig.logTiming.getOrElse(false)) Some(Instant.now) else None,
              noColor = commonOpts.noColor
            )
          )
        )
        .map(l => if (commonOpts.debug) l else l.withMinLogLevel(LogLevel.info))
        .maybeZipWith(Some(Loggers.path(buildPaths.logFile, LogPatterns.logFile)))
        // it's an optimization to perform this before both loggers so we don't need to do it twice
        .map(Loggers.decodeJsonStream(BLEEP_JSON_EVENT, _))
    }
}
