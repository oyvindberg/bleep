package bleep
package internal

import ryddig.*
import ryddig.jul.RyddigJulBridge

import java.io.{BufferedWriter, PrintStream}
import java.time.Instant

object bleepLoggers {

  /** Install JUL bridge to redirect java.util.logging through the given ryddig logger.
    *
    * Call with a file-only logger for TUI mode (prevents lsp4j warnings from clobbering the terminal). Call with a file+screen logger for non-TUI mode.
    */
  def installJulBridge(logger: Logger): Unit = {
    val rootLogger = java.util.logging.Logger.getLogger("")
    rootLogger.getHandlers.foreach(rootLogger.removeHandler)
    RyddigJulBridge.install(logger)
  }
  /* Use this environment variable to communicate to subprocesses that parent accepts json events */
  val CallerProcessAcceptsJsonEvents = "CALLER_PROCESS_ACCEPTS_JSON_EVENTS"

  // all log events will start with this string following 0.0.1-M26.
  val BLEEP_JSON_EVENT = "BLEEP_JSON_EVENT:"

  /** Create a logger that only writes to a file (no stdout/stderr output). Useful when running TUI that needs exclusive control of the terminal.
    */
  def fileOnly(logFile: java.nio.file.Path): LoggerResource[BufferedWriter] =
    Loggers.path(logFile, LogPatterns.logFile)

  /** Create a silent logger that discards all output. Useful when running TUI that handles its own display. Uses storing logger which captures logs in memory
    * instead of outputting.
    */
  def silent: Logger =
    Loggers.storing()

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
      val fileLoggerResource = Loggers.path(buildPaths.logFile, LogPatterns.logFile)
      baseStdout(bleepConfig, commonOpts)
        .maybeZipWith(Some(fileLoggerResource))
        .map { logger =>
          // Install JUL bridge to file+screen logger (non-TUI default).
          // ReactiveBsp reinstalls to file-only when entering TUI mode.
          installJulBridge(logger)
          Loggers.decodeJsonStream(BLEEP_JSON_EVENT, logger)
        }
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
        .map { logger =>
          // Install JUL bridge to capture java.util.logging to file
          installJulBridge(logger)
          Loggers.decodeJsonStream(BLEEP_JSON_EVENT, logger)
        }
    }
}
