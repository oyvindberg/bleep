# Guided agent session: seeded vs cold worktrees

This is the landing page's agent vignette, run for real: a scripted Claude
Code session where an orchestrator fans two subagents into fresh git
worktrees over bleep's MCP tools — one seeds its worktree with
`bleep.copy-state`, one deliberately compiles cold — and then asks bleep
itself to compare the two runs with a cross-worktree `history diff-timing`
call. The model never invents a number: the ratio in the verdict comes out
of the tool.

Captured output of a real run (2026-08-15, sonnet, $0.34, 17 s wall):

```
agent[seeded]  copy-state: 580554 bytes in 40ms  ->  first compile: success
agent[cold]    first compile: success (compiled everything from scratch)
bleep history diff-timing (cold -> seeded): total 2893ms -> 251ms
verdict: the seeded worktree compiled about 11.5x faster than the cold one
(2893ms vs 251ms), confirming that bleep's copy-state let it reuse build
state instead of recompiling from scratch.
```

Ratios move with daemon warmth and machine load — captured runs on this
toy fixture have landed between 6x and 28x; the structure never changes.
On the 5.1M-line repo the same mechanism is 4½ minutes vs 54 seconds (see
the worktrees guide).

`prompt.txt` is deliberately short enough to read on screen: the recorded
cast opens by printing it in full with `bat`, so a viewer sees every
instruction the session is given before watching it run.

Run it yourself:

```bash
./run.sh /tmp/bleep-agent-demo          # plain text, final verdict only
bleep generate-videos claude-agents     # record it as the site's asciinema cast
```

The recording goes through the same demo harness as every other cast
(`GenDemoVideos`), and it is on-demand only: `bleep generate-videos` without
arguments never runs it, because recording spends model tokens and needs an
authenticated `claude` CLI.

## How the recording drives a real session

`drive.exp` types into an interactive Claude Code session over a pty, so the
cast is the tool's own UI rather than a rendering of a log. Two things about
that were learned the hard way and are worth keeping:

- **Match single words.** The TUI paints justified text with cursor-positioning
  escapes between words (`trust\033[29Gthis\033[37Gfolder`), so multi-word
  patterns never match the raw stream even though the words are plainly on
  screen.
- **Submit, then verify.** An Enter that arrives while a modal dialog is up is
  consumed by the dialog, leaving the prompt sitting in the input box. The
  driver retries until the session is visibly working.

The harness trims the cast down to the demo itself (`Demo.trim`): the head is
the harness getting the session going, the tail is a spinner winding down after
the answer landed. Deterministic, so regenerating cuts in the same place.

Known cosmetic wart: Claude Code paints a rotating "Tip:" line while working,
and it lands *before* the verdict, so no tail trim can remove it. Suppressing it
would mean writing `tipsHistory` / `tipLifetimeShownCounts` into the recording
user's `~/.claude.json`, which the harness deliberately does not do.
