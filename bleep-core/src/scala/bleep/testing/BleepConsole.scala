package bleep.testing

import bleep.PreBootstrapOpts

/** ANSI-color toggle that mirrors the fields of `scala.Console` we use directly in log messages, but returns empty strings when no-color mode is in effect
  * (`--no-color` CLI flag or `NO_COLOR` env var per no-color.org). Imported as `SConsole` / `C` at the existing reference sites in `BuildDisplay`,
  * `ReactiveBsp`, and `CompileDisplay` — every `SConsole.RED` / `C.GREEN` baked into a log message string is now ANSI-free in no-color mode without per-site
  * changes.
  *
  * The `on` flag is a JVM-local val captured at class-loading time, which is after `PreBootstrapOpts.parse` has decided. The pre-parse runs once at CLI startup
  * before any of these objects are touched, so the flag reflects the current invocation correctly.
  */
object BleepConsole {
  private val on: Boolean = !PreBootstrapOpts.noColorRequested
  val RED: String = if (on) scala.Console.RED else ""
  val GREEN: String = if (on) scala.Console.GREEN else ""
  val YELLOW: String = if (on) scala.Console.YELLOW else ""
  val BLUE: String = if (on) scala.Console.BLUE else ""
  val CYAN: String = if (on) scala.Console.CYAN else ""
  val MAGENTA: String = if (on) scala.Console.MAGENTA else ""
  val WHITE: String = if (on) scala.Console.WHITE else ""
  val BOLD: String = if (on) scala.Console.BOLD else ""
  val UNDERLINED: String = if (on) scala.Console.UNDERLINED else ""
  val RESET: String = if (on) scala.Console.RESET else ""

  private val AnsiCsi = "\u001b\\[[0-?]*[ -/]*[@-~]".r

  /** Strip ANSI from text bleep did not author, when no-color is in effect.
    *
    * The constants above cover bleep's own output. They cannot cover a *framework's*: munit, utest, weaver and zio-test colourise from their own settings and
    * never consult `sbt.testing.Logger.ansiCodesSupported`, so telling the forked runner that colour is unwanted quietens ScalaTest and hedgehog but not these.
    * Their escapes then reached a terminal the user had explicitly asked to keep plain. Applied only when no-color is on, so a normal run keeps every bit of
    * colour the framework meant to send.
    */
  def sanitize(line: String): String =
    if (on || line.indexOf(0x1b) < 0) line else AnsiCsi.replaceAllIn(line, "")
}
