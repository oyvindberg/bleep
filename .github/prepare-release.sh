#!/bin/bash

set -e

mkdir -p work/release

CWD=$(pwd -P)

chmod +x artifacts/bleep-x86_64-pc-linux/bleep
(cd artifacts/bleep-x86_64-pc-linux && tar cvfz "$CWD/work/release/bleep-x86_64-pc-linux.tar.gz" bleep)

chmod +x artifacts/bleep-x86_64-apple-darwin/bleep
(cd artifacts/bleep-x86_64-apple-darwin && tar cvfz "$CWD/work/release/bleep-x86_64-apple-darwin.tar.gz" bleep)

chmod +x artifacts/bleep-arm64-apple-darwin/bleep
(cd artifacts/bleep-arm64-apple-darwin && tar cvfz "$CWD/work/release/bleep-arm64-apple-darwin.tar.gz" bleep)

(cd artifacts/bleep-x86_64-pc-win32 && zip -r "$CWD/work/release/bleep-x86_64-pc-win32.zip" bleep.exe)

