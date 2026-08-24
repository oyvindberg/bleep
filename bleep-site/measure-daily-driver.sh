#!/usr/bin/env bash
# Re-measure the code sizes quoted across the site.
#
# Every number the site states about "the repo we work in daily" comes from
# bleep-site/src/data/build-stats.json, and this script is how that file's
# `repo` and `compiled` sections are produced. Run it, paste the numbers in,
# and update `measuredAt` and `repo.commit`.
#
# Usage: ./bleep-site/measure-daily-driver.sh [path-to-repo]
#
# Timings (build-stats.json `timings`) are NOT produced here — they are
# measured by hand against a warm daemon, see docs/guides/worktrees.mdx.

set -euo pipefail

REPO="${1:-$HOME/pr/dlab}"

if [ ! -d "$REPO/.git" ]; then
  echo "not a git repository: $REPO" >&2
  exit 1
fi

cd "$REPO"

# Directories that are not part of the build: VCS internals, agent scratch
# space, sibling worktrees, and build output.
prune=(
  -path ./.git -o
  -path ./.claude -o
  -path ./worktrees -o
  -name .bleep -o
  -name target -o
  -name node_modules
)

count_lines() {
  local ext="$1"
  shift
  find . \( "${prune[@]}" ${1:+-o -path "$1"} \) -prune -o -name "*.$ext" -print0 |
    xargs -0 cat |
    wc -l |
    tr -d ' '
}

count_files() {
  local ext="$1"
  find . \( "${prune[@]}" \) -prune -o -name "*.$ext" -print |
    wc -l |
    tr -d ' '
}

java_lines=$(count_lines java)
java_files=$(count_files java)
scala_lines=$(count_lines scala)
scala_files=$(count_files scala)

# The generated Java lives in its own top-level project. Split it out: the
# point of the number is that most of what bleep compiles is machine-written,
# which is exactly the case that makes other build tools slow.
if [ -d ./dquery-generated ]; then
  generated_lines=$(find ./dquery-generated -name '*.java' -print0 | xargs -0 cat | wc -l | tr -d ' ')
else
  echo "warning: ./dquery-generated not found, generated/hand-written split unavailable" >&2
  generated_lines=0
fi
hand_written_lines=$((java_lines - generated_lines))

# `git ls-files` also lists submodule entries and symlinks, which are not
# readable files. Filter to regular files explicitly rather than letting `cat`
# fail and take the whole run down with it.
only_regular_files() {
  while IFS= read -r -d '' f; do
    [ -f "$f" ] && printf '%s\0' "$f"
  done
}

tracked_files=$(git ls-files | wc -l | tr -d ' ')
tracked_lines=$(git ls-files -z | only_regular_files | xargs -0 cat | wc -l | tr -d ' ')
projects=$(bleep --no-color projects 2>/dev/null | grep -c .)

cat <<EOF
repo.commit                        $(git rev-parse --short HEAD)
repo.trackedFiles                  ${tracked_files}
repo.trackedLines                  ${tracked_lines}

compiled.projects                  ${projects}
compiled.files                     $((java_files + scala_files))
compiled.lines                     $((java_lines + scala_lines))

compiled.java.files                ${java_files}
compiled.java.lines                ${java_lines}
compiled.java.generatedLines       ${generated_lines}
compiled.java.handWrittenLines     ${hand_written_lines}

compiled.scala.files               ${scala_files}
compiled.scala.lines               ${scala_lines}
EOF
