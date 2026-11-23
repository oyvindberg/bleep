#!/usr/bin/env bash
# Two subagents, two fresh worktrees: one seeded with bleep copy-state, one
# cold. Bleep itself measures the difference. Run from a compiled bleep repo.
set -euo pipefail

# Isolate the session to exactly one MCP server, bleep's own. Without this, any
# OTHER MCP server already registered in the runner's own environment (an
# unauthenticated one is enough) puts a persistent "N MCP server(s) need
# authentication" line in the UI that never goes away — and drive.exp's "MCP"
# pattern, written for the one-time "approve these servers?" dialog, matches it
# on every repaint and loops forever sending Enter at nothing. BLEEP_COMMAND lets
# this point at a specific bleep binary (a local snapshot build, say) instead of
# whatever "bleep" resolves to on PATH; it defaults to that for a normal run.
bleep_command="${BLEEP_COMMAND:-bleep}"
bleep_bin="$(command -v "$bleep_command")"
mcp_config="$(mktemp -t bleep-demo-mcp-config).json"
trap 'rm -f "$mcp_config"' EXIT
cat > "$mcp_config" <<JSON
{"mcpServers": {"bleep": {"type": "stdio", "command": "$bleep_command", "args": ["mcp-server"]}}}
JSON
export MCP_CONFIG_PATH="$mcp_config"

# The two subagents' own compile/copy-state calls go through plain CLI, not
# the bleep MCP tool: both subagents share the ONE bleep-mcp-server process
# Claude Code spawned for this whole session, over its single stdio
# connection, so two "parallel" MCP tool calls queue behind each other there
# no matter how parallel the subagent orchestration itself is - the fast
# seeded check ends up stuck behind the slow cold compile, and the measured
# gap this demo exists to show collapses. A `Bash` call is a real, separate
# OS process per subagent, so it is genuinely concurrent. Only the final,
# sequential history diff-timing comparison - not time-sensitive - stays on
# the MCP tool, so the demo still shows that working.
export PROMPT="$(cat <<PROMPT
Scripted demo - follow EXACTLY, edit no files, do not improvise.
The cwd is a compiled bleep repo (MAIN); its absolute path is \`pwd\`.
BLEEP is the absolute path $bleep_bin - use it for every shell command below,
never the bare word "bleep".

1. git worktree add -b seeded ../wt-seeded
   git worktree add -b cold   ../wt-cold

2. Spawn two subagents IN PARALLEL: one message, two Agent calls, both in the
   FOREGROUND. Do not background them and do not schedule wakeups - just wait
   for both to report.
   seeded: "In a shell, cd to <MAIN>/../wt-seeded, then run
     'BLEEP copy-state <MAIN>' followed by 'BLEEP compile --no-tui
     --no-color'. Report both results verbatim, including the History: #<n>
     line."
   cold:   "In a shell, cd to <MAIN>/../wt-cold, then run 'BLEEP compile
     --no-tui --no-color'. Do NOT run copy-state - this worktree starts cold
     on purpose. Report the result verbatim, including the History: #<n>
     line."

3. Both report historyId=1 in their own worktree. With the bleep MCP tools,
   ask bleep to compare the runs: history diff-timing with
   directory=<MAIN>/../wt-seeded, base=1, target=1,
   baseDirectory=<MAIN>/../wt-cold.

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
