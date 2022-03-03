#!/usr/bin/env bash

# This script can be used to run bleep from command line without building native-image

# where you have checked out bleep
bleep_git=$HOME/pr/bleep

bleep -d "${bleep_git}" run bleep -- -d "$(pwd)" "$@"
