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
(`GenDemoVideos`), with the session's stream-json rendered live by
`render-stream.py`. It is on-demand only: `bleep generate-videos` without
arguments never runs it, because recording spends model tokens and needs an
authenticated `claude` CLI.

- `fixture.sh` builds a small-but-real build (30 circe-derived case classes)
  and compiles it, so the daemon and caches are warm and the demo measures
  worktree seeding, not dependency downloads.
- `prompt.txt` is the exact orchestrator script the session follows.
- `run.sh` wires it together with a tool whitelist (`Agent`,
  `git worktree`, and the bleep MCP tools - nothing else).
