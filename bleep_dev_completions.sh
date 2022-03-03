#!/usr/bin/env bash

_bleep_dev_completions() {
  COMPREPLY=($(bleep_dev _complete "${COMP_LINE}" "${COMP_CWORD}" "${COMP_POINT}"))
}

complete -F _bleep_dev_completions bleep_dev
