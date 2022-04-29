#!/bin/bash
mkdir -p work/release
chmod 755 artifacts/linux-binary/bleep-linux
chmod 755 artifacts/macos-binary/bleep-macos

mv artifacts/linux-binary/bleep-linux "work/release/bleep-$VERSION-x86-64-pc-linux"
mv artifacts/macos-binary/bleep-macos "work/release/bleep-$VERSION-x86-64-apple-darwin"
mv artifacts/windows-binary/bleep-windows.exe artifacts/windows-binary/bleep.exe

gzip "work/release/bleep-$VERSION-x86-64-pc-linux"
gzip "work/release/bleep-$VERSION-x86-64-apple-darwin"
CWD=$(pwd -P)
(cd artifacts/windows-binary && zip -r "$CWD/work/release/bleep-$VERSION-x86-64-pc-win32.zip" *.exe)