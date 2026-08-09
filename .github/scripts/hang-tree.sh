#!/usr/bin/env bash
# A process tree that hangs, shaped like the hang `run-bounded.sh` exists to bound. Used only by the `bound-check` job.
#
# The shape is the whole point. A tree of bash `sleep`s would be reaped by the POSIX process-group kill on every
# platform, so such a test would pass while the real thing kept failing — which is exactly the trap this is here to
# avoid. What actually hung was `bleep` spawning a BSP daemon which forked test JVMs: those are GRANDCHILDREN, they are
# native Windows processes outside any POSIX process group, and they had inherited the step's stdout. Both properties
# have to be present or the test proves nothing.
#
#   - grandchild, not child: killing the direct child is what the first version of the bound did, and the hang came back.
#   - a real Windows process: `start /B` detaches one that MSYS knows nothing about.
#
# It also writes to stdout before blocking, so a run that produces no output tells you the harness itself failed to
# start rather than that the bound worked.
set -uo pipefail

echo "hang-tree: starting"

if [ -n "${WINDIR:-}" ]; then
  # Detached native grandchild. `ping -n` is the portable Windows sleep: no PowerShell startup cost, present on every
  # runner image, and it shows up in `tasklist` under a name the check can look for.
  cmd //c "start /B ping -n 3000 127.0.0.1 > NUL" || true
  echo "hang-tree: spawned detached grandchild, now blocking"
  ping -n 3000 127.0.0.1 >/dev/null
else
  # POSIX equivalent, so the job can run on Linux too and prove the bound is not Windows-only behaviour.
  sleep 3000 &
  echo "hang-tree: spawned background grandchild, now blocking"
  sleep 3000
fi
