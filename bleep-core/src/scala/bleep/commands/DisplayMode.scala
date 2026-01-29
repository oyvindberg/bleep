package bleep
package commands

sealed trait DisplayMode
object DisplayMode {

  /** Full TUI with live updates, spinners, progress bars */
  case object Tui extends DisplayMode

  /** Simple output, just failures and summary (for CI/agents) */
  case object NoTui extends DisplayMode

  /** Smart constructor - checks if TUI is supported */
  def resolve(requested: DisplayMode): DisplayMode = requested match {
    case Tui if !bleep.testing.FancyTestDisplay.isSupported => NoTui
    case other                                               => other
  }

  /** Parse from CLI flags */
  def fromFlags(noTui: Boolean): DisplayMode =
    if (noTui) NoTui else Tui
}
