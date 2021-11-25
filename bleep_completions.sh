#!/usr/bin/env bash

_bleep_completions() {
  COMPREPLY=($(bleep _complete "${COMP_LINE}" "${COMP_CWORD}" "${COMP_POINT}"))
}

complete -F _bleep_completions bleep
