#!/usr/bin/env bash
# Two subagents, two fresh worktrees: one seeded with bleep copy-state, one
# cold. Bleep itself measures the difference. Run from a compiled bleep repo.
set -euo pipefail

export PROMPT="$(cat <<'PROMPT'
Scripted demo - follow EXACTLY, edit no files, do not improvise.
The cwd is a compiled bleep repo (MAIN); its absolute path is `pwd`.

1. git worktree add -b seeded ../wt-seeded
   git worktree add -b cold   ../wt-cold

2. Spawn two subagents IN PARALLEL: one message, two Agent calls, both in the
   FOREGROUND. Do not background them and do not schedule wakeups - just wait
   for both to report.
   seeded: "With the bleep MCP tools: call copy-state with
     directory=<MAIN>/../wt-seeded and from=<MAIN>, then compile the same
     directory. Report both results verbatim."
   cold:   "With the bleep MCP tools: call compile with
     directory=<MAIN>/../wt-cold. Do NOT call copy-state - this worktree
     starts cold on purpose. Report the result verbatim."

3. Both report historyId=1 in their own worktree. Ask bleep to compare the
   runs: history diff-timing with directory=<MAIN>/../wt-seeded, base=1,
   target=1, baseDirectory=<MAIN>/../wt-cold.

4. Print, with real numbers from the tool results:
   agent[seeded]  copy-state: <bytes> bytes in <ms>  ->  first compile: success
   agent[cold]    first compile: success (compiled everything from scratch)
   bleep history diff-timing (cold -> seeded): total <baseMs>ms -> <targetMs>ms
   verdict: <one sentence stating both durations - no multiplier, no ratio>
Then stop.
PROMPT
)"

# drive.exp types that prompt into a real interactive session, so what you see
# below is Claude Code's own UI - not a rendering of a log
exec expect -f "$(dirname "$0")/drive.exp"
