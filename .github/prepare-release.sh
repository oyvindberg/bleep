#!/bin/bash
mkdir -p work/release
(cd artifacts && unzip linux-binary.zip)
(cd artifacts && unzip macos-binary.zip)
(cd artifacts && unzip windows-binary.zip)
chmod 755 artifacts/bleep-linux
chmod 755 artifacts/bleep-macos

mv artifacts/bleep-linux "work/release/bleep-$VERSION-x86-64-pc-linux"
mv artifacts/bleep-macos "work/release/bleep-$VERSION-x86-64-apple-darwin"

gzip "work/release/bleep-$VERSION-x86-64-pc-linux"
gzip "work/release/bleep-$VERSION-x86-64-apple-darwin"
CWD=$(pwd -P)
(cd artifacts && zip -r "$CWD/work/release/bleep-$VERSION-x86-64-pc-win32.zip" *.exe)