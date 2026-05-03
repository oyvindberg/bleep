package bleep.scripts

/** Maintainer-only scripts that depend on bleep's local source tree (e.g. `bleep-cli`'s `Main` opts tree) rather than the published `build.bleep:*` artifacts.
  *
  * Live here when a script needs introspection of bleep itself — for example, `GenCliDocs` walks the decline command tree and emits per-command Markdown into
  * `docs/reference/cli/`.
  */
package object dev
