#!/usr/bin/env bash
#
# BSP Server Stress Test
#
# Clones a source repo into N temp workspaces and repeatedly git-clean + compile
# concurrently. Collects per-iteration timing and points at the BSP server's
# metrics.jsonl for post-analysis.
#
# Usage:
#   benchmark-bsp.sh [OPTIONS]
#
# Options:
#   -i NUM        Number of iterations per workspace (default: 10)
#   -s DIR        Source project directory (default: ~/pr/typr-3)
#   -n NUM        Number of concurrent workspaces (default: 3)
#
# Example:
#   benchmark-bsp.sh -i 10 -s ~/pr/typr-3 -n 3
#
set -euo pipefail

ITERATIONS=10
SOURCE="$HOME/pr/typr-3"
NUM_WS=3

while getopts "i:s:n:" opt; do
  case "$opt" in
    i) ITERATIONS="$OPTARG" ;;
    s) SOURCE="$OPTARG" ;;
    n) NUM_WS="$OPTARG" ;;
    *) echo "Usage: $0 [-i iterations] [-s source_dir] [-n num_workspaces]"; exit 1 ;;
  esac
done

BLEEP="${BLEEP:-./bleep}"

# Resolve source to absolute path
SOURCE="$(cd "$SOURCE" && pwd)"

echo "============================================="
echo "  BSP Server Benchmark"
echo "============================================="
echo "Source:      $SOURCE"
echo "Iterations:  $ITERATIONS per workspace"
echo "Workspaces:  $NUM_WS"
echo "Bleep:       $BLEEP"
echo ""
echo "PREREQUISITES:"
echo "  Ensure bleep-bsp changes are published:"
echo "    bleep sourcegen && bleep my-publish-local"
echo "    pkill -f BspServerDaemon"
echo ""

# Stable base dir — reuses existing workspaces across runs
BASE_DIR="/tmp/bsp-benchmark"
mkdir -p "$BASE_DIR"
RESULTS_DIR="$BASE_DIR/results"
mkdir -p "$RESULTS_DIR"

echo "Base dir:    $BASE_DIR"
echo "Results:     $RESULTS_DIR"
echo ""

# Clone source into workspace directories only if they don't exist yet
for i in $(seq 1 "$NUM_WS"); do
  ws_dir="$BASE_DIR/ws-$i"
  if [ -d "$ws_dir/.git" ]; then
    echo "Reusing existing ws-$i"
  else
    echo "Cloning source to ws-$i..."
    git clone --local "$SOURCE" "$ws_dir" --quiet
  fi
done
echo ""
echo "All workspaces ready."
echo ""

# Millisecond timestamp
ts_ms() {
  python3 -c 'import time; print(int(time.time() * 1000))'
}

# Per-workspace benchmark loop
run_workspace() {
  local ws_num="$1"
  local ws_dir="$BASE_DIR/ws-$ws_num"
  local csv="$RESULTS_DIR/ws-${ws_num}.csv"
  local log="$RESULTS_DIR/ws-${ws_num}-output.log"
  echo "iteration,operation,start_ms,end_ms,duration_ms,exit_code" > "$csv"

  local iteration=0
  while [ "$iteration" -lt "$ITERATIONS" ]; do
    iteration=$((iteration + 1))

    # Clean: git clean -xfd removes all untracked/ignored files (build outputs)
    local op_start
    op_start="$(ts_ms)"
    git -C "$ws_dir" clean -xfd >> "$log" 2>&1
    local clean_exit=$?
    local op_end
    op_end="$(ts_ms)"
    local dur=$((op_end - op_start))
    echo "$iteration,clean,$op_start,$op_end,$dur,$clean_exit" >> "$csv"

    # Compile
    op_start="$(ts_ms)"
    "$BLEEP" --no-color -d "$ws_dir" compile >> "$log" 2>&1
    local compile_exit=$?
    op_end="$(ts_ms)"
    dur=$((op_end - op_start))
    echo "$iteration,compile,$op_start,$op_end,$dur,$compile_exit" >> "$csv"

    echo "[ws-$ws_num] iteration $iteration: clean=$clean_exit compile=$compile_exit (${dur}ms)"
  done

  echo "[ws-$ws_num] Done after $iteration iterations."
}

echo "Starting benchmark ($ITERATIONS iterations x $NUM_WS workspaces)..."
echo "Start time: $(date -u '+%Y-%m-%dT%H:%M:%SZ')"
echo ""

START_TS="$(ts_ms)"

# Launch all workspaces concurrently
pids=()
for i in $(seq 1 "$NUM_WS"); do
  run_workspace "$i" &
  pids+=($!)
done

# Wait for all to finish
all_ok=true
for pid in "${pids[@]}"; do
  if ! wait "$pid"; then
    all_ok=false
  fi
done

END_TS="$(ts_ms)"
TOTAL_DURATION=$((END_TS - START_TS))

echo ""
echo "============================================="
echo "  Benchmark Complete"
echo "============================================="
echo "Total wall time: ${TOTAL_DURATION}ms"
echo ""

# Find the BSP server's metrics.jsonl
# It lives under ~/.bleep/bsp/<hash>/metrics.jsonl or ~/Library/Caches/build.bleep/socket/<hash>/metrics.jsonl
METRICS_FILE=""
for search_dir in "$HOME/.bleep/bsp" "$HOME/Library/Caches/build.bleep/socket"; do
  if [ -d "$search_dir" ]; then
    found="$(find "$search_dir" -name metrics.jsonl 2>/dev/null | head -1)"
    if [ -n "$found" ]; then
      METRICS_FILE="$found"
      break
    fi
  fi
done

# Summary
SUMMARY="$RESULTS_DIR/benchmark-summary.txt"
{
  echo "BSP Benchmark Summary"
  echo "====================="
  echo "Source: $SOURCE"
  echo "Iterations: $ITERATIONS per workspace"
  echo "Actual duration: ${TOTAL_DURATION}ms"
  echo "Workspaces: $NUM_WS"
  echo "Start: $(date -u -r $((START_TS / 1000)) '+%Y-%m-%dT%H:%M:%SZ' 2>/dev/null || echo 'N/A')"
  echo ""

  for i in $(seq 1 "$NUM_WS"); do
    csv="$RESULTS_DIR/ws-${i}.csv"
    if [ -f "$csv" ]; then
      total_iters=$(tail -n +2 "$csv" | grep ',compile,' | wc -l | tr -d ' ')
      failures=$(tail -n +2 "$csv" | awk -F, '$NF != 0' | wc -l | tr -d ' ')
      avg_compile=$(tail -n +2 "$csv" | grep ',compile,' | awk -F, '{sum+=$5; n++} END {if(n>0) printf "%.0f", sum/n; else print "N/A"}')
      echo "Workspace $i: $total_iters iterations, $failures failures, avg compile ${avg_compile}ms"
    fi
  done

  echo ""
  if [ -n "$METRICS_FILE" ]; then
    echo "BSP Metrics: $METRICS_FILE"
  else
    echo "BSP Metrics: not found (server may not have started yet)"
  fi
} | tee "$SUMMARY"

# Copy metrics into results if found
if [ -n "$METRICS_FILE" ]; then
  cp "$METRICS_FILE" "$RESULTS_DIR/metrics.jsonl"
  echo ""
  echo "Metrics copied to: $RESULTS_DIR/metrics.jsonl"
fi

echo ""
echo "Results directory: $RESULTS_DIR"
echo ""

if [ "$all_ok" = false ]; then
  echo "WARNING: Some workspace processes exited with errors."
  exit 1
fi
