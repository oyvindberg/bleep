#!/usr/bin/env bash
# Print the version this build will bake into the client, i.e. `model.BleepVersion.current`.
#
# Every publish in build.yml has to name this exact version. The client asks Coursier for `build.bleep:bleep-bsp` at the
# version compiled into it, so publishing under any other coordinate produces a binary that resolves nothing — and
# nothing fails until someone runs it.
#
# It is read out of the sourcegen output rather than recomputed with dynver, because recomputing is how the two drift:
# dynver appends a timestamp on a dirty tree, so a second derivation of "the same" version need not agree with the one
# already compiled in.
#
# This lives in one place because it previously did not. Four steps each carried their own copy of the grep, all naming
# the pre-1.0.0-M11 layout (`.bleep/generated-sources/<project>/...`), and when the bootstrap moved to a bleep that
# writes the current layout (`.bleep/projects/<project>/generated-sources/...`) every one of them silently matched
# nothing at once.

set -uo pipefail

version_file=".bleep/projects/bleep-model/generated-sources/bleep.scripts.GenerateResources/bleep/model/BleepVersion.scala"

if [ ! -f "$version_file" ]; then
  echo "::error::$version_file does not exist — run 'bleep sourcegen' before this step. (If the layout moved again, this script is what needs updating.)" >&2
  exit 1
fi

version=$(grep 'val current' "$version_file" | sed 's/.*BleepVersion("\(.*\)").*/\1/')

# An empty version is the dangerous case, not a loud one: passed to `--version ""` it publishes under whatever the
# fallback picks, and the mismatch only surfaces when a binary later fails to resolve its own server.
if [ -z "$version" ]; then
  echo "::error::could not parse a version out of $version_file" >&2
  exit 1
fi

echo "$version"
