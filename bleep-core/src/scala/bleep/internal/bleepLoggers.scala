package bleep
package internal

import bleep.logging.{LogLevel, Loggers, TypedLogger, TypedLoggerResource}
import bleep.model.BleepConfig

import java.io.{BufferedWriter, PrintStream}
import java.time.Instant

object bleepLoggers {
  /* Use this environment variable to communicate to subprocesses that parent accepts json events */
  val CallerProcessAcceptsJsonEvents = "CALLER_PROCESS_ACCEPTS_JSON_EVENTS"

  def stdoutNoLogFile(bleepConfig: model.BleepConfig, commonOpts: CommonOpts): TypedLoggerResource[PrintStream] =
    if (commonOpts.logAsJson) TypedLoggerResource.flushable(Loggers.printJsonStream(System.out))
    else baseStdout(bleepConfig, commonOpts).map(Loggers.decodeJsonStream)

  def stdoutAndFileLogging(
      bleepConfig: BleepConfig,
      commonOpts: CommonOpts,
      buildPaths: BuildPaths
  ): TypedLoggerResource[(PrintStream, Option[BufferedWriter])] =
    if (commonOpts.logAsJson) TypedLoggerResource.pure(Loggers.printJsonStream(System.out)).maybeZipWith(None)
    else {
      baseStdout(bleepConfig, commonOpts)
        .maybeZipWith(Some(Loggers.path(buildPaths.logFile, LogPatterns.logFile)))
        // it's an optimization to perform this before both loggers so we don't need to do it twice
        .map(Loggers.decodeJsonStream)
    }

  private def baseStdout(bleepConfig: BleepConfig, commonOpts: CommonOpts): TypedLoggerResource[PrintStream] =
    Loggers
      .stdout(
        LogPatterns.interface(
          startRun = if (bleepConfig.logTiming.getOrElse(false)) Some(Instant.now) else None,
          noColor = commonOpts.noColor
        ),
        disableProgress = commonOpts.noBspProgress
      )
      .map(l => if (commonOpts.debug) l else l.minLogLevel(LogLevel.info))

  // we need to use stderr in some situations
  // 1) BSP where stdout is already used for BSP protocol
  // 2) auto-complete, when stdout is used to communicate completions
  // 3) early, right after boot if we haven't set up proper logging

  def stderrWarn(commonOpts: CommonOpts): TypedLogger[PrintStream] =
    if (commonOpts.logAsJson) Loggers.printJsonStream(System.err)
    else Loggers.decodeJsonStream(Loggers.stderr(LogPatterns.logFile).minLogLevel(LogLevel.warn))

  def stderrAll(commonOpts: CommonOpts): TypedLogger[PrintStream] =
    if (commonOpts.logAsJson) Loggers.printJsonStream(System.err)
    else Loggers.decodeJsonStream(Loggers.stderr(LogPatterns.logFile))

  def stderrAndFileLogging(commonOpts: CommonOpts, buildPaths: BuildPaths): TypedLoggerResource[(PrintStream, Option[BufferedWriter])] =
    if (commonOpts.logAsJson) TypedLoggerResource.pure(Loggers.printJsonStream(System.err)).maybeZipWith(None)
    else {
      TypedLoggerResource
        .pure(Loggers.stderr(LogPatterns.logFile))
        .map(l => if (commonOpts.debug) l else l.minLogLevel(LogLevel.info))
        .maybeZipWith(Some(Loggers.path(buildPaths.logFile, LogPatterns.logFile)))
        // it's an optimization to perform this before both loggers so we don't need to do it twice
        .map(Loggers.decodeJsonStream)
    }
}
