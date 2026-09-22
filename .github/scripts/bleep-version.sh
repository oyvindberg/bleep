#!/usr/bin/env bash
# Print the version this build baked into the client, i.e. `model.BleepVersion.current`.
#
# Every publish in build.yml has to name this exact version. The client asks Coursier for `build.bleep:bleep-bsp` at its
# own version, so publishing under any other coordinate produces a binary that resolves nothing — and nothing fails
# until someone runs it.
#
# `BleepVersion.current` reads bleep-model's `dynver` stamp, so that stamp is what this reads too. The compile writes
# it (the BSP server stamps every project it compiles), so run this after a compile. Reading it rather than recomputing
# dynver is still the point: dynver appends a timestamp on a dirty tree, so a second derivation of "the same" version
# need not agree with the one the binary will report.
#
# This lives in one place because it previously did not. Four steps each carried their own copy of the grep, and when a
# layout moved every one of them silently matched nothing at once.

set -uo pipefail

stamp_file=".bleep/projects/bleep-model/generated-resources/bleep-stamps/bleep-stamp/bleep-model.properties"

if [ ! -f "$stamp_file" ]; then
  echo "::error::$stamp_file does not exist — compile bleep-model before this step, with a bleep that writes stamps. (If the layout moved again, this script is what needs updating.)" >&2
  exit 1
fi

version=$(sed -n 's/^dynver=//p' "$stamp_file")

# An empty version is the dangerous case, not a loud one: passed to `--version ""` it publishes under whatever the
# fallback picks, and the mismatch only surfaces when a binary later fails to resolve its own server.
if [ -z "$version" ]; then
  echo "::error::could not parse a dynver out of $stamp_file" >&2
  exit 1
fi

echo "$version"
