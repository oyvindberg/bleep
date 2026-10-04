#!/usr/bin/env bash
# List (default) or kill (--kill) bleep processes that are not running the installed binary's code.
#
#   - `bleep mcp-server` processes whose executable is not the current ~/.local/bin/bleep. Compared by inode, because
#     a deploy renames the old binary (bleep.prev-*) and a long-lived server keeps running that renamed file.
#     Hosts respawn the server on the next tool use, from the current binary.
#   - BspServerDaemon processes whose bleep-bsp jar version differs from the version baked into that binary.
#
# Before any daemon is killed, every metrics.jsonl is copied out, since a stopped daemon deletes its socket dir.
#
# Usage: stale-processes.sh [--kill] [--binary PATH] [--version VERSION]
#   --binary   the binary that counts as current (default ~/.local/bin/bleep)
#   --version  the daemon version that counts as current (default: the snapshot version read from the binary;
#              a release binary has none, so pass it)
#
# macOS only (lsof txt inodes, stat -f).
set -euo pipefail

kill_them=false
binary="$HOME/.local/bin/bleep"
version=""
while [ $# -gt 0 ]; do
  case "$1" in
    --kill) kill_them=true; shift ;;
    --binary) binary="$2"; shift 2 ;;
    --version) version="$2"; shift 2 ;;
    *) echo "unknown argument: $1" >&2; exit 2 ;;
  esac
done

[ -x "$binary" ] || { echo "no executable at $binary" >&2; exit 1; }
current_inode=$(stat -f %i "$binary")

if [ -z "$version" ]; then
  versions=$(strings "$binary" | grep -oE '1\.0\.0-M[0-9]+\+[0-9]+-[a-f0-9]+(-SNAPSHOT)?' | sort -u)
  if [ "$(printf '%s\n' "$versions" | grep -c .)" != 1 ]; then
    echo "cannot tell the version of $binary, it names: $(echo $versions). Pass --version." >&2
    exit 1
  fi
  version="$versions"
fi

echo "current binary: $binary (inode $current_inode), version $version"

stale_mcp=""
for pid in $(pgrep -f 'mcp-server' || true); do
  ps -p "$pid" -o command= | grep -qE '(^|/)bleep(-cli)? mcp-server' || continue
  exe_line=$(lsof -a -p "$pid" -d txt -Fin 2>/dev/null | sed -n '3,4p' | tr '\n' ' ') || true
  inode=$(echo "$exe_line" | grep -oE 'i[0-9]+' | head -1 | cut -c2-)
  path=$(echo "$exe_line" | grep -oE 'n/.*' | head -1 | cut -c2- | sed 's/ *$//')
  [ -n "$inode" ] || { echo "  mcp-server $pid: cannot read its executable (exited?), skipping"; continue; }
  if [ "$inode" != "$current_inode" ]; then
    echo "  stale mcp-server $pid  started $(ps -p "$pid" -o lstart=)  runs $path"
    stale_mcp="$stale_mcp $pid"
  fi
done

stale_daemons=""
for pid in $(pgrep -f BspServerDaemon || true); do
  v=$(ps -p "$pid" -o command= | grep -oE 'bleep-bsp_3/[^/]+' | head -1 | cut -d/ -f2) || true
  if [ "$v" != "$version" ]; then
    echo "  stale daemon $pid  started $(ps -p "$pid" -o lstart=)  version ${v:-unknown}"
    stale_daemons="$stale_daemons $pid"
  fi
done

n_mcp=$(echo $stale_mcp | wc -w | tr -d ' ')
n_daemons=$(echo $stale_daemons | wc -w | tr -d ' ')
echo "stale: $n_mcp mcp-server, $n_daemons daemon"

$kill_them || exit 0
[ "$n_mcp" = 0 ] && [ "$n_daemons" = 0 ] && exit 0

if [ "$n_daemons" != 0 ]; then
  backup="$HOME/.bleep-metrics-backups/$(date +%Y%m%d-%H%M%S)"
  mkdir -p "$backup"
  for f in "$HOME"/Library/Caches/build.bleep/socket/*/metrics.jsonl; do
    [ -f "$f" ] || continue
    cp "$f" "$backup/$(basename "$(dirname "$f")").metrics.jsonl"
  done
  echo "metrics backed up to $backup"
fi

all="$stale_mcp $stale_daemons"
kill -TERM $all 2>/dev/null || true
for _ in 1 2 3 4 5 6 7 8 9 10; do
  alive=""
  for pid in $all; do kill -0 "$pid" 2>/dev/null && alive="$alive $pid"; done
  [ -z "$alive" ] && break
  sleep 1
done
if [ -n "$alive" ]; then
  echo "still alive after SIGTERM, sending SIGKILL:$alive"
  kill -KILL $alive 2>/dev/null || true
  sleep 1
fi
for pid in $all; do
  if kill -0 "$pid" 2>/dev/null; then echo "FAILED to kill $pid" >&2; exit 1; fi
done
echo "killed $n_mcp mcp-server, $n_daemons daemon"
