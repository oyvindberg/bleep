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
# The Windows root comes from `$LOCALAPPDATA` run through `cygpath`, not from `~/AppData/Local`. Two reasons,
# both of which make the difference between collecting everything and collecting nothing:
#
#   - `$LOCALAPPDATA` is a native path (`C:\Users\runneradmin\AppData\Local`) and backslashes are escapes to
#     the globber, so it has to be converted rather than interpolated. `build.yml`'s "Kill leftover JVMs"
#     step already documents this trap; this is the same `cygpath` call.
#   - `~` is not a safe substitute. GitHub runs every `shell: bash` block as `bash --noprofile --norc`, so
#     none of Git Bash's startup files run and `$HOME` is whatever the runner exported — which is not
#     guaranteed to be a POSIX path, and is not guaranteed to be the account whose LOCALAPPDATA bleep used.
#
# Every candidate root is echoed with what was found under it, so a run that collects nothing says which
# paths it looked at instead of printing one unfalsifiable "nothing to summarise".
#
# Never fails the caller — a diagnostic that breaks the run it is diagnosing is worse than no diagnostic.
set -uo pipefail
shopt -s nullglob

out=${1:-bsp-diagnostics}

roots=(
  "$HOME/.cache/bleep"               # linux (XDG)
  "$HOME/Library/Caches/build.bleep" # macos
)

if [ -n "${LOCALAPPDATA:-}" ]; then
  if command -v cygpath >/dev/null 2>&1; then
    roots+=("$(cygpath "$LOCALAPPDATA")/bleep/cache")
  else
    # Only reachable if something other than Git Bash is running this on Windows. Say so rather than
    # appending a backslash path that would silently match nothing.
    echo "::warning::LOCALAPPDATA is set but cygpath is not on PATH — cannot resolve the Windows cache dir"
  fi
fi

echo "compile-server socket dirs:"
dirs=()
for root in "${roots[@]}"; do
  socket="$root/socket"
  if [ -d "$socket" ]; then
    here=("$socket"/*/)
    echo "  $socket — ${#here[@]} server dir(s)"
    dirs+=("${here[@]}")
  else
    echo "  $socket — absent"
  fi
done

if [ ${#dirs[@]} -eq 0 ]; then
  echo "no compile-server socket dirs under any of the above — nothing to summarise"
  exit 0
fi

mkdir -p "$out"
for d in "${dirs[@]}"; do
  name=$(basename "$d")
  mkdir -p "$out/$name"
  for f in "$d"metrics.jsonl "$d"output "$d"output.1 "$d"output.2; do
    [ -f "$f" ] || continue
    cp "$f" "$out/$name/"
    # Sizes in the log because an empty metrics.jsonl and a missing one are different failures, and the
    # artifact listing alone does not distinguish them.
    echo "  collected $name/$(basename "$f") ($(wc -c <"$f" | tr -d ' ') bytes)"
  done
done

# The summary is a convenience so the common questions do not need an artifact download. The raw files are
# already copied by this point, so a missing interpreter costs the summary, not the diagnostics.
if command -v python3 >/dev/null 2>&1; then
  python3 "$(dirname "$0")/summarise-bsp-metrics.py" "$out"
else
  echo "python3 not on PATH — skipping the inline summary; the uploaded artifact still has the raw metrics"
fi
