package bleep
package internal

import org.slf4j.event.Level
import org.slf4j.helpers.{BasicMDCAdapter, BasicMarkerFactory, LegacyAbstractLogger, MessageFormatter}
import org.slf4j.spi.{MDCAdapter, SLF4JServiceProvider}
import org.slf4j.{ILoggerFactory, IMarkerFactory, Marker}
import ryddig.{LogLevel, Logger, Metadata}
import sourcecode.{Enclosing, File, Line}

import java.time.Instant
import scala.math.Ordering.Implicits.infixOrderingOps

/** SLF4J provider that logs through whichever ryddig logger [[Slf4jBridge]] holds. Registered in `META-INF/services/org.slf4j.spi.SLF4JServiceProvider`.
  *
  * Vendored instead of ryddig-slf4j2 0.0.6, which takes its logger from a public `var`, and formats SLF4J's `{}` placeholders with `String.formatted`, so every
  * parameterised message lost its arguments.
  */
class BleepSlf4jServiceProvider extends SLF4JServiceProvider {
  private val loggerFactory: ILoggerFactory = name => new BleepSlf4jLogger(name, () => Slf4jBridge.installed)
  private val markerFactory: IMarkerFactory = new BasicMarkerFactory
  private val mdcAdapter: MDCAdapter = new BasicMDCAdapter

  override def getLoggerFactory: ILoggerFactory = loggerFactory
  override def getMarkerFactory: IMarkerFactory = markerFactory
  override def getMDCAdapter: MDCAdapter = mdcAdapter
  override def getRequestedApiVersion: String = "2.0.99"
  override def initialize(): Unit = ()
}

/** Libraries ask for their loggers early, often in static initialisers, so `target` is asked per call. That way a logger handed out before
  * [[Slf4jBridge.install]] still follows the latest install, e.g. into the file-only logger in TUI mode.
  */
final class BleepSlf4jLogger(loggerName: String, target: () => Logger) extends LegacyAbstractLogger {
  name = loggerName

  private def enabled(level: LogLevel): Boolean = target().minLogLevel <= level

  override def isTraceEnabled: Boolean = enabled(LogLevel.debug)
  override def isDebugEnabled: Boolean = enabled(LogLevel.debug)
  override def isInfoEnabled: Boolean = enabled(LogLevel.info)
  override def isWarnEnabled: Boolean = enabled(LogLevel.warn)
  override def isErrorEnabled: Boolean = enabled(LogLevel.error)

  override protected def getFullyQualifiedCallerName: String = null

  override protected def handleNormalizedLoggingCall(
      level: Level,
      marker: Marker,
      messagePattern: String,
      arguments: Array[AnyRef],
      throwable: Throwable
  ): Unit = {
    val logLevel = level match {
      case Level.ERROR => LogLevel.error
      case Level.WARN  => LogLevel.warn
      case Level.INFO  => LogLevel.info
      case Level.DEBUG => LogLevel.debug
      case Level.TRACE => LogLevel.debug
    }
    val logger = target()
    val withMarker = if (marker == null) logger else logger.withContext("marker", marker.getName)
    val message = MessageFormatter.basicArrayFormat(messagePattern, arguments)
    val metadata = new Metadata(Instant.now, logLevel, Line(-1), File(loggerName), Enclosing("slf4j"))
    withMarker(message, Option(throwable), metadata)
  }
}
