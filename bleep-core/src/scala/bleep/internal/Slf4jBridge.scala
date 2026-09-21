package bleep
package internal

import ryddig.Logger

import java.util.concurrent.atomic.AtomicReference

/** Where SLF4J log calls from libraries go: plexus-archiver while coursier unpacks an archive, and anything else that logs through SLF4J.
  *
  * This is process-global state, and that is SLF4J's design rather than ours: a library asks `LoggerFactory` for a logger by name, with no way for us to pass
  * one in. `java.util.logging` is the same, and [[bleepLoggers.installLoggingBridges]] installs both bridges together.
  *
  * The provider that reads this lives in bleep-cli (`BleepSlf4jServiceProvider`). bleep-core is also a library that build scripts depend on, and it must not
  * claim SLF4J for them.
  */
object Slf4jBridge {
  private val target = new AtomicReference[Option[Logger]](None)

  /** Send SLF4J to `logger` from now on, replacing whatever was installed before. */
  def install(logger: Logger): Unit =
    target.set(Some(logger))

  /** The installed logger. Throws if nothing is installed yet: a library logging before bleep set up logging means a startup path that skipped
    * [[bleepLoggers.installLoggingBridges]], and that should be found rather than hidden.
    */
  def installed: Logger =
    target.get() match {
      case Some(logger) => logger
      case None         =>
        throw new IllegalStateException(
          "A library logged through SLF4J before bleep installed a logger for it. Call bleepLoggers.installLoggingBridges (or Slf4jBridge.install) first."
        )
    }
}
