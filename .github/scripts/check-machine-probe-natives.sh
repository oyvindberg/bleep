#!/bin/bash
# Fails unless the bleep-machine-probes jar(s) under the given directory carry the machine-probe JNI library for every platform
# that needs one. A jar published without them works on Linux and throws at compile-server start on macOS and Windows,
# so this is checked where jars are published rather than discovered by the first user on another OS.
#
# Usage: check-machine-probe-natives.sh <directory to search for bleep-machine-probes_3 jars>
set -euo pipefail

root=$1
jars=$(find "$root" -name 'bleep-machine-probes_3*.jar' ! -name '*-sources.jar' ! -name '*-javadoc.jar')
if [ -z "$jars" ]; then
  echo "::error::no bleep-machine-probes_3 jar under $root"
  exit 1
fi
for jar in $jars; do
  listing=$(unzip -l "$jar")
  for lib in bleep/machine/native/darwin-universal/libbleep-machine.dylib bleep/machine/native/windows-x86_64/bleep-machine.dll bleep/machine/native/windows-arm64/bleep-machine.dll; do
    if ! grep -q "$lib" <<<"$listing"; then
      echo "::error::$jar has no $lib"
      exit 1
    fi
  done
  echo "$jar carries the machine-probe libraries"
done
