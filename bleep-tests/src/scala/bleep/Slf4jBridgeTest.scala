package bleep

import bleep.internal.BleepSlf4jLogger
import org.scalatest.funsuite.AnyFunSuite
import ryddig.LogLevel

class Slf4jBridgeTest extends AnyFunSuite {

  test("SLF4J picks bleep's provider, so it neither warns about a missing one nor discards") {
    // Without the service file SLF4J prints "No SLF4J providers were found" here and hands back a NOPLogger.
    val logger = org.slf4j.LoggerFactory.getLogger("org.codehaus.plexus.Example")
    assert(logger.isInstanceOf[BleepSlf4jLogger], s"got ${logger.getClass.getName}")
  }

  test("fills in {} placeholders, keeps the level and the throwable") {
    val stored = ThreadSafeStoringLogger()
    val slf4j = new BleepSlf4jLogger("org.codehaus.plexus.Example", () => stored)
    val boom = new RuntimeException("boom")

    slf4j.warn("could not set {} on {}", "mode", "file.txt")
    slf4j.error("expand failed", boom)

    val logged = stored.underlying.toList
    assert(logged.map(_.message.plainText) == List("could not set mode on file.txt", "expand failed"))
    assert(logged.map(_.metadata.logLevel) == List(LogLevel.warn, LogLevel.error))
    assert(logged.map(_.throwable) == List(None, Some(boom)))
  }

  test("respects the target's minimum level") {
    val stored = ThreadSafeStoringLogger().withMinLogLevel(LogLevel.info)
    val slf4j = new BleepSlf4jLogger("x", () => stored)

    assert(!slf4j.isDebugEnabled)
    slf4j.debug("not {}", "shown")
    slf4j.info("shown")

    assert(stored.underlying.toList.map(_.message.plainText) == List("shown"))
  }
}
