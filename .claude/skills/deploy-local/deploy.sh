#!/usr/bin/env bash
# Build this checkout into ~/.local/bin/bleep, publish the matching daemon jars, and kill every process still running
# older code: stale `bleep mcp-server`s and daemons. See SKILL.md for why each step exists.
#
# Usage: deploy.sh   (from anywhere inside the checkout; the working tree must be clean)
#
# macOS only.
set -euo pipefail

here="$(cd "$(dirname "$0")" && pwd)"
root="$(git rev-parse --show-toplevel)"
cd "$root"

step() { echo; echo "==> $*"; }
die() { echo "deploy: $*" >&2; exit 1; }

# A dirty tree makes dynver add a minute-resolution timestamp, and every build re-stamps, so the binary and the
# published jars would end up with different versions.
[ -z "$(git status --porcelain)" ] || die "working tree is dirty; commit first (see the version invariant in SKILL.md)"

step "sourcegen"
bleep sourcegen --no-color

step "compile bleep-cli (native-image only images the classes already on disk)"
bleep compile bleep-cli --no-color --no-tui

stamp=.bleep/projects/bleep-model/generated-resources/bleep-stamps/bleep-stamp/bleep-model.properties
version=$(sed -n 's/^dynver=//p' "$stamp")
[ -n "$version" ] || die "no dynver in $stamp"
echo "version: $version"

step "publish local-ivy"
bleep publish local-ivy --no-color --no-tui
for artifact in bleep-bsp_3 bleep-test-runner; do
  [ -d "$HOME/.ivy2/local/build.bleep/$artifact/$version" ] || die "$artifact $version was not published"
done

step "native-image"
bleep native-image --no-color
image=.bleep/projects/bleep-cli/builds/normal/target/native-image/bleep-cli
[ -x "$image" ] || die "no native image at $image"

# The exit code says nothing about whether the image holds the code just compiled. The version stamp does.
baked=$(strings "$image" | grep -oE '1\.0\.0-M[0-9]+\+[0-9]+-[a-f0-9]+(-SNAPSHOT)?' | sort -u)
[ "$baked" = "$version" ] || die "image names version(s) [$(echo $baked)], expected $version"

step "install (mv, never cp: cp breaks the macOS signature and the binary dies with exit 137)"
bin="$HOME/.local/bin/bleep"
if [ -e "$bin" ]; then
  prev="$bin.prev-$version"
  [ -e "$prev" ] && prev="$prev-$(date +%Y%m%d-%H%M%S)"
  mv "$bin" "$prev"
  echo "previous binary kept as $prev"
fi
mv "$image" "$bin"
"$bin" --help >/dev/null || die "installed binary does not run (exit $?)"

step "stop registered daemons"
# stale-processes.sh backs up metrics before it kills daemons; stop-all deletes socket dirs too, so back up first here.
backup="$HOME/.bleep-metrics-backups/$(date +%Y%m%d-%H%M%S)-pre-stop-all"
mkdir -p "$backup"
for f in "$HOME"/Library/Caches/build.bleep/socket/*/metrics.jsonl; do
  [ -f "$f" ] || continue
  cp "$f" "$backup/$(basename "$(dirname "$f")").metrics.jsonl"
done
echo "metrics backed up to $backup"
"$bin" config compile-server stop-all --no-color

step "kill stale mcp-servers and daemons stop-all missed"
"$here/stale-processes.sh" --kill --binary "$bin" --version "$version"

step "smoke test: a fresh daemon comes up on $version"
"$bin" compile bleep-model --no-color --no-tui
running=$(for pid in $(pgrep -f BspServerDaemon); do ps -p "$pid" -o command= | grep -oE 'bleep-bsp_3/[^/]+' | cut -d/ -f2; done | sort -u)
[ "$running" = "$version" ] || die "daemons run [$(echo $running)], expected only $version"

echo
echo "deployed $version. MCP hosts respawn bleep mcp-server on the next tool use (or /mcp to reconnect now)."
