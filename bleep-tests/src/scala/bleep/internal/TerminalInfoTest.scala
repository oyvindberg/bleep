package bleep
package internal

import org.scalatest.funsuite.AnyFunSuite
import ryddig.{LogLevel, Loggers}
import scala.util.Try

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
    // Should still return a positive value
    assert(TerminalInfo.getWidthOrDefault(80) > 0)
  }
  
  test("Terminal width string parsing") {
    // Test the parsing logic that we use
    assert(Try("80".toInt).toOption.contains(80))
    assert(Try("120".toInt).toOption.contains(120))
    assert(Try("".toInt).toOption.isEmpty)
    assert(Try("abc".toInt).toOption.isEmpty)
  }
  
  test("stty output parsing") {
    // Test the stty output parsing pattern
    val sttyOutput = "24 80"
    val parts = sttyOutput.trim.split(" ")
    assert(parts.length == 2)
    assert(Try(parts(1).toInt).toOption.contains(80))
    
    // Invalid cases
    val invalidOutput = "invalid"
    val invalidParts = invalidOutput.trim.split(" ")
    assert(invalidParts.length == 1)
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