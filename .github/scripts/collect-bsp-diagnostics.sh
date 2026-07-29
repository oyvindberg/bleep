#!/usr/bin/env bash
# Collect the compile server's telemetry and logs into ./bsp-diagnostics for upload.
#
# The socket directory sits under bleep's per-user cache dir, which `UserPaths.fromAppDirs` derives from
# `ProjectDirectories.from("build", null, "bleep")` — a different path on every OS:
#
#   linux    ~/.cache/bleep/socket
#   macos    ~/Library/Caches/build.bleep/socket
#   windows  %LOCALAPPDATA%\bleep\cache\socket
#
# Globbing only the Linux one collected nothing anywhere else. That mattered less than it looks, because the
# steps that called it existed only in the `build` job (ubuntu) — so the native-image matrix, i.e. every
# Windows and macOS run, produced no telemetry at all. Windows is where the platform-specific failures live
# and was the one arch we could not see into.
#
# Windows paths are reached via `$HOME` rather than `$LOCALAPPDATA` on purpose: under Git bash the latter is
# a backslash path (`C:\Users\runneradmin\AppData\Local`), and backslashes are escapes to the globber.
#
# Never fails the caller — a diagnostic that breaks the run it is diagnosing is worse than no diagnostic.
set -uo pipefail
shopt -s nullglob

out=${1:-bsp-diagnostics}

dirs=(
  ~/.cache/bleep/socket/*/               # linux (XDG)
  ~/Library/Caches/build.bleep/socket/*/ # macos
  ~/AppData/Local/bleep/cache/socket/*/  # windows
)

if [ ${#dirs[@]} -eq 0 ]; then
  echo "no compile-server socket dirs — nothing to summarise"
  exit 0
fi

mkdir -p "$out"
for d in "${dirs[@]}"; do
  name=$(basename "$d")
  mkdir -p "$out/$name"
  for f in "$d"metrics.jsonl "$d"output "$d"output.1 "$d"output.2; do
    [ -f "$f" ] && cp "$f" "$out/$name/"
  done
done

# The summary is a convenience so the common questions do not need an artifact download. The raw files are
# already copied by this point, so a missing interpreter costs the summary, not the diagnostics.
if command -v python3 >/dev/null 2>&1; then
  python3 "$(dirname "$0")/summarise-bsp-metrics.py" "$out"
else
  echo "python3 not on PATH — skipping the inline summary; the uploaded artifact still has the raw metrics"
fi
