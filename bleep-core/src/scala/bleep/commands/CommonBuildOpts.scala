package bleep
package commands

/** Shared options for commands that compile before doing other work (run, publish-local, dist).
  *
  * These mirror the flags available on `compile`, `test`, and `link` so that every command that triggers a build step offers the same TUI/flamegraph/cancel
  * experience.
  */
case class CommonBuildOpts(
    displayMode: DisplayMode,
    flamegraph: Boolean,
    cancel: Boolean
)
