#!/usr/bin/env bash
# Builds the fixture, then runs demo.sh in it: a real Claude Code session
# driving the seeded-vs-cold worktree demo.
#
# the orchestrator spawns two parallel subagents over bleep's MCP tools - one
# seeds its fresh worktree with copy-state, one compiles cold - then asks bleep
# itself for the cross-worktree timing diff and states the ratio.
#
# Requirements: `bleep` on PATH with the MCP server registered user-scope
# (`claude mcp add --scope user bleep -- bleep mcp-server`), `claude` on PATH,
# and a warm compile daemon. Costs a few cents of tokens; ~20s wall.
#
# Usage: run.sh <scratch-dir>   (created; must not exist)
set -euo pipefail

[ $# -eq 1 ] || { echo "usage: run.sh <scratch-dir>" >&2; exit 1; }
scratch="$1"
here="$(cd "$(dirname "$0")" && pwd)"

"$here/fixture.sh" "$scratch/repo"

cd "$scratch/repo"
"$here/demo.sh"
