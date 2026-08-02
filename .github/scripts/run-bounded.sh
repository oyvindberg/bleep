#!/usr/bin/env bash
# Run a command under a wall-clock bound, and fail if it exceeds it.
#
# Usage: run-bounded.sh <seconds> <command> [args...]
#
# Why this exists rather than `timeout-minutes:` on the step. When a step exceeds its GitHub-side cap, the runner has to
# reap the step's process tree — and on windows-latest it could not. Observed on run 30697246865: the test step declared
# `timeout-minutes: 20` and ran 45m07s, the job then blew through its own 45-minute ceiling at 55m00s, and the runner was
# destroyed with both telemetry steps still pending. The one hang worth diagnosing produced no diagnostics at all, which
# is the exact failure #627 added the step cap to prevent. The cap did not hold.
#
# Why not `timeout(1)`. It is GNU coreutils, absent from macOS, which is two of the five arches in this matrix. Doing it
# in bash keeps one code path for every OS, which is the same reason the surrounding steps unified on `shell: bash`.
#
# On expiry we SIGKILL rather than SIGTERM: the process being bounded is a build that has already proven it is not
# responding, and a graceful signal it might ignore would put us back to waiting on a tree that will not die.

set -uo pipefail

if [ "$#" -lt 2 ]; then
  echo "usage: run-bounded.sh <seconds> <command> [args...]" >&2
  exit 2
fi

limit_seconds="$1"
shift

"$@" &
child_pid=$!

waited=0
poll_interval=5
while kill -0 "$child_pid" 2>/dev/null; do
  if [ "$waited" -ge "$limit_seconds" ]; then
    # A GitHub workflow command, so this lands as an annotation on the job rather than only in the log.
    echo "::error title=Timed out::'$*' exceeded ${limit_seconds}s and was killed"
    kill -9 "$child_pid" 2>/dev/null || true
    wait "$child_pid" 2>/dev/null || true
    # 124 is what timeout(1) reports for this, so the number means the same thing here as everywhere else.
    exit 124
  fi
  sleep "$poll_interval"
  waited=$(( waited + poll_interval ))
done

# The loop only ends when the child is gone, so this reports its real exit status rather than blocking.
wait "$child_pid"
exit $?
