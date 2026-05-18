package bleep
package commands

sealed trait DisplayMode
object DisplayMode {

  /** Full TUI with live updates, spinners, progress bars */
  case object Tui extends DisplayMode

  /** Simple output, just failures and summary (for CI/agents) */
  case object NoTui extends DisplayMode

  /** Watch mode with per-project diffs between cycles. Implies --no-tui and --watch. */
  case object DiffWatch extends DisplayMode

  /** Smart constructor - checks if TUI is supported */
  def resolve(requested: DisplayMode): DisplayMode = requested match {
    case Tui if !bleep.testing.FancyBuildDisplay.isSupported => NoTui
    case DiffWatch                                           => DiffWatch
    case other                                               => other
  }

  /** Parse from CLI flags. The TUI is a colored fullscreen interface, so if the user asked for no colors at all (either via `--no-color` on the CLI or via the
    * `NO_COLOR` env var per no-color.org), we silently downgrade to [[NoTui]] regardless of `--no-tui`. Otherwise the explicit `--no-tui` wins.
    */
  def fromFlags(noTui: Boolean): DisplayMode =
    if (noTui || PreBootstrapOpts.noColorRequested) NoTui else Tui
}
