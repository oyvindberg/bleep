package bleep
package internal

import org.scalatest.funsuite.AnyFunSuite
import ryddig.{LogLevel, Loggers}

class TerminalInfoTest extends AnyFunSuite {

  private val silentLogger = Loggers.stdout(ryddig.LogPatterns.logFile, disableProgress = true).acquire().value.withMinLogLevel(LogLevel.error)

  test("TerminalInfo.getWidthOrDefault returns positive value") {
    // Should always return a positive width
    assert(TerminalInfo.getWidthOrDefault(80) > 0)
    assert(TerminalInfo.getWidthOrDefault(120) > 0)
  }

  test("TerminalInfo.initialize does not throw exceptions") {
    // This should not throw even if terminal detection fails
    TerminalInfo.initialize(silentLogger)
    // After initialization, getWidthOrDefault should always return a positive value
    val width = TerminalInfo.getWidthOrDefault(80)
    assert(width > 0)
    // Either we detected a width or we're using the default
    assert(TerminalInfo.getWidth.isDefined || width == 80)
  }

  test("Terminal width detection with native-terminal") {
    // TerminalInfo might already be initialized from previous tests

    // Test that we can get terminal width using native-terminal
    val width = TerminalInfo.getWidth
    val defaultWidth = TerminalInfo.getWidthOrDefault(80)

    // Should always return a positive default
    assert(defaultWidth > 0)

    // Width might be None in CI environments
    width match {
      case Some(w) =>
        assert(w > 0)
        assert(w <= 1000) // Reasonable upper bound
        assert(defaultWidth == w) // getWidthOrDefault should return detected width
      case None =>
        // OK in environments where terminal is not available
        println("Terminal width not detected - likely in CI or non-terminal environment")
        assert(defaultWidth == 80) // Should use default
    }
  }

  test("Progress display width calculations") {
    // Test the width calculation logic used in BspClientDisplayProgress
    val termWidth = 80
    val prefix = "Compiling "
    val suffix = " +2"
    val fixedWidth = prefix.length + suffix.length
    val availableWidth = (termWidth - fixedWidth).max(20)

    assert(availableWidth > 0)
    assert(availableWidth == 67) // 80 - 10 - 3 = 67
  }
}
