#!/usr/bin/env bash
# Run a command under a wall-clock bound, and fail if it exceeds it.
#
# Usage: run-bounded.sh <seconds> <command> [args...]
#
# Why this exists rather than `timeout-minutes:` on the step. When a step exceeds its GitHub-side cap the runner has to
# reap the step's process tree, and on windows-latest it cannot: the test step declared 20 minutes and ran 45m07s, the
# job blew its own 45-minute ceiling at 55m00s, and the runner was destroyed with both telemetry steps still pending. So
# the one hang worth diagnosing produced no diagnostics — twice.
#
# TWO things have to be true for the step to actually end, and the first version of this script only did the first.
# That is why the hang came back on run 30752489661 with the bound already in place.
#
#   1. The bounded command must die. Killing the direct child is not enough: `bleep` spawns a BSP daemon, which spawns
#      forked test JVMs. Those are grandchildren and outlive their parent. So the whole process GROUP is killed, and on
#      Windows `taskkill /T` as well, because Windows processes are not in POSIX process groups.
#
#   2. Nothing may still hold the step's stdout. GitHub waits for the step's output pipes to close, not merely for the
#      shell to exit — so a surviving daemon that inherited stdout keeps the step alive no matter what happened to the
#      process we were watching. The child therefore writes to a FILE, and its descendants inherit that file instead of
#      the pipe. Only `tail` holds the pipe, and `tail` is ours to kill.
#
# `timeout(1)` is not used: it is GNU coreutils, absent on macOS, which is two of the five arches in this matrix. Doing
# it in bash keeps one code path per OS, the same reason the surrounding steps unified on `shell: bash`.
#
# SIGKILL rather than SIGTERM on expiry: the thing being bounded has already proven it is not responding, and a signal
# it may ignore puts us back to waiting on a tree that will not die.

set -uo pipefail

if [ "$#" -lt 2 ]; then
  echo "usage: run-bounded.sh <seconds> <command> [args...]" >&2
  exit 2
fi

limit_seconds="$1"
shift

log_file="$(mktemp -t run-bounded.XXXXXX)"
is_windows=false
case "$(uname -s)" in
  MINGW* | MSYS* | CYGWIN*) is_windows=true ;;
esac

# Job control, so the background child leads its own process group and `kill -- -PID` reaches everything it spawned.
# Without it the child shares our group and a group kill would take this script down with it.
set -m
"$@" >"$log_file" 2>&1 &
child_pid=$!
set +m

# Stream the log, so the step is not silent for the ~13 minutes this normally takes. `tail` holding the step's stdout is
# fine and is the point: it is ours, it has no children, and it is killed below.
tail -n +1 -f "$log_file" &
tail_pid=$!

stop_tail() {
  kill "$tail_pid" 2>/dev/null || true
  wait "$tail_pid" 2>/dev/null || true
}

kill_tree() {
  if [ "$is_windows" = true ]; then
    # MSYS pids are not Windows pids; /proc/<pid>/winpid is the translation. Doubled slashes stop MSYS mangling the
    # arguments into paths.
    local winpid
    winpid="$(cat "/proc/$child_pid/winpid" 2>/dev/null || true)"
    if [ -n "$winpid" ]; then
      taskkill //F //T //PID "$winpid" >/dev/null 2>&1 || true
    fi
  fi
  kill -9 -- "-$child_pid" 2>/dev/null || true
  kill -9 "$child_pid" 2>/dev/null || true
}

waited=0
poll_interval=5
while kill -0 "$child_pid" 2>/dev/null; do
  if [ "$waited" -ge "$limit_seconds" ]; then
    # A workflow command, so this lands as an annotation on the job rather than only in the log.
    echo "::error title=Timed out::'$*' exceeded ${limit_seconds}s and was killed"
    kill_tree
    sleep 2 # let tail drain what the child wrote before it died
    stop_tail
    echo "::group::last 200 lines before the kill"
    tail -n 200 "$log_file" 2>/dev/null || true
    echo "::endgroup::"
    rm -f "$log_file"
    # 124 is what timeout(1) reports for this, so the number means the same thing here as everywhere else.
    exit 124
  fi
  sleep "$poll_interval"
  waited=$((waited + poll_interval))
done

wait "$child_pid"
exit_code=$?

sleep 1
stop_tail
rm -f "$log_file"
exit "$exit_code"
