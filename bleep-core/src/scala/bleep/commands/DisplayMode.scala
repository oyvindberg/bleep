package bleep
package commands

sealed trait DisplayMode
object DisplayMode {

  /** Full TUI with live updates, spinners, progress bars */
  case object Tui extends DisplayMode

  /** Plain streaming output: every project/suite event as a log line, then the summary (for CI/agents that want the full trace) */
  case object NoTui extends DisplayMode

  /** Only failures and the final summary: per-event streaming and daemon-connection chatter are suppressed. `--quiet` / `--summary-only`. */
  case object Quiet extends DisplayMode

  /** Watch mode with per-project diffs between cycles. Implies --no-tui and --watch. */
  case object DiffWatch extends DisplayMode

  /** Smart constructor - checks if TUI is supported */
  def resolve(requested: DisplayMode): DisplayMode = requested match {
    case Tui if !bleep.testing.FancyBuildDisplay.isSupported => NoTui
    case other                                               => other
  }

  /** Parse from CLI flags. `--quiet` wins over `--no-tui`: it promises less output, not just a different renderer. The TUI is a colored fullscreen interface,
    * so if the user asked for no colors at all (either via `--no-color` on the CLI or via the `NO_COLOR` env var per no-color.org), we silently downgrade to
    * [[NoTui]] regardless of `--no-tui`.
    */
  def fromFlags(noTui: Boolean, quiet: Boolean): DisplayMode =
    if (quiet) Quiet
    else if (noTui || PreBootstrapOpts.noColorRequested) NoTui
    else Tui
}
