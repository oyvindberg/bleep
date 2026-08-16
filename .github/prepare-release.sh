#!/bin/bash

set -e

mkdir -p work/release

CWD=$(pwd -P)

chmod +x artifacts/bleep-x86_64-pc-linux/bleep
(cd artifacts/bleep-x86_64-pc-linux && tar cvfz "$CWD/work/release/bleep-x86_64-pc-linux.tar.gz" bleep)

chmod +x artifacts/bleep-arm64-pc-linux/bleep
(cd artifacts/bleep-arm64-pc-linux && tar cvfz "$CWD/work/release/bleep-arm64-pc-linux.tar.gz" bleep)

chmod +x artifacts/bleep-x86_64-apple-darwin/bleep
(cd artifacts/bleep-x86_64-apple-darwin && tar cvfz "$CWD/work/release/bleep-x86_64-apple-darwin.tar.gz" bleep)

chmod +x artifacts/bleep-arm64-apple-darwin/bleep
(cd artifacts/bleep-arm64-apple-darwin && tar cvfz "$CWD/work/release/bleep-arm64-apple-darwin.tar.gz" bleep)

(cd artifacts/bleep-x86_64-pc-win32 && zip -r "$CWD/work/release/bleep-x86_64-pc-win32.zip" bleep.exe)

# SHA-256 for every archive above, in the format `sha256sum -c` / `shasum -a 256 -c` expect:
# `<hex>  <filename>`, filenames relative to the directory the file sits in. Generated inside
# work/release so the names have no `work/release/` prefix — a user who downloads the archive and
# SHA256SUMS into the same directory can run the check with no arguments and no editing.
#
# `SHA256SUMS` itself is uploaded by the `files: work/release/*` glob in build.yml, so it lands as
# a release asset alongside the archives with no workflow change beyond this script.
#
# The glob is expanded before the redirect, and `set -e` plus the explicit count check below means
# a missing or renamed archive fails the release rather than publishing a short list of hashes
# that silently omits a platform.
(
  cd work/release
  sha256sum bleep-*.tar.gz bleep-*.zip > SHA256SUMS
  count=$(wc -l < SHA256SUMS)
  if [ "$count" -ne 5 ]; then
    echo "::error::expected 5 release archives to hash, got $count" >&2
    cat SHA256SUMS >&2
    exit 1
  fi
  cat SHA256SUMS
)
