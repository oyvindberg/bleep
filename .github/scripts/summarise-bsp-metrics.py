#!/usr/bin/env python3
"""Print what the compile server did during a CI run, from its metrics.jsonl.

The point is that the common questions should not require downloading an artifact. Two of them
recur often enough to be worth answering on every run:

  "did each compile get slower, or did fewer of them run at once?"
      Those have different causes and different fixes, and the distinction is invisible in a wall
      clock. Per-project compile durations answer the first; machine occupancy answers the second.

  "is bleep leaving the machine idle while work is queued?"
      Idle cores with a non-empty queue means admission is refusing work it could take — a
      scheduling bug rather than a slow compiler.

Reads every metrics.jsonl under the directory given, tolerates truncated or malformed lines (a
server killed mid-write leaves one), and never fails the build: a diagnostic that breaks the run it
is diagnosing is worse than no diagnostic.
"""

from __future__ import annotations

import json
import pathlib
import sys
from collections import Counter, defaultdict


def load(path: pathlib.Path) -> list[dict]:
    rows = []
    with path.open(encoding="utf-8", errors="replace") as fh:
        for line in fh:
            line = line.strip()
            if not line:
                continue
            try:
                rows.append(json.loads(line))
            except ValueError:
                continue  # truncated final line from a killed server
    return rows


def fmt_ms(ms: float) -> str:
    return f"{ms / 1000:.1f}s" if ms < 60_000 else f"{ms / 60_000:.1f}m"


def summarise(path: pathlib.Path, rows: list[dict]) -> None:
    print(f"\n=== {path.parent.name} — {len(rows)} events ===")
    by_type = Counter(r.get("type") for r in rows)
    print("  events: " + ", ".join(f"{k}={v}" for k, v in by_type.most_common()))

    compiles = [r for r in rows if r.get("type") == "compile_end"]
    if compiles:
        total = sum(r.get("duration_ms", 0) for r in compiles)
        failed = sum(1 for r in compiles if not r.get("success", True))
        print(f"\n  compiles: {len(compiles)} ({failed} failed), total task time {fmt_ms(total)}")
        worst = sorted(compiles, key=lambda r: -r.get("duration_ms", 0))[:8]
        print("  slowest:")
        for r in worst:
            print(f"    {fmt_ms(r.get('duration_ms', 0)):>8}  {r.get('project', '?')}")

    allocs = [r for r in rows if r.get("type") == "compile_allocation"]
    if allocs:
        per = defaultdict(int)
        for r in allocs:
            per[r.get("project", "?")] += r.get("allocated_mb", 0)
        print(f"\n  allocation attributed for {len(allocs)} compiles; heaviest:")
        for proj, mb in sorted(per.items(), key=lambda kv: -kv[1])[:5]:
            print(f"    {mb:>8} MB  {proj}")

    machine = [r for r in rows if r.get("type") == "machine"]
    if machine:
        samples = len(machine)
        starved = sum(1 for r in machine if r.get("waiting", 0) > 0 and r.get("used_cpu", 0) < r.get("total_cpu", 0))
        saturated = sum(1 for r in machine if r.get("used_cpu", 0) >= r.get("total_cpu", 0))
        idle = sum(1 for r in machine if r.get("used_cpu", 0) == 0 and r.get("waiting", 0) == 0)
        peak_wait = max((r.get("waiting", 0) for r in machine), default=0)
        print(f"\n  machine: {samples} samples at 15s")
        print(f"    saturated (all cpu in use):            {saturated:>4}  ({100 * saturated / samples:.0f}%)")
        print(f"    idle (nothing running, nothing queued): {idle:>4}  ({100 * idle / samples:.0f}%)")
        print(f"    STARVED (queue non-empty, cpu free):    {starved:>4}  ({100 * starved / samples:.0f}%)   <- admission refusing work it could take")
        print(f"    deepest queue: {peak_wait}")

    cache = [r for r in rows if r.get("type") == "analysis_cache"]
    if cache:
        last = cache[-1]
        hits, misses = last.get("intern_hits", 0), last.get("intern_misses", 0)
        looked_up = hits + misses
        rate = (100 * hits / looked_up) if looked_up else 0.0
        print(f"\n  analysis cache: {last.get('entries', 0)} analyses over {last.get('workspaces', 0)} workspace(s), {last.get('file_bytes', 0) // (1024 * 1024)}MB of files")
        print(f"    interning: {hits} hits / {misses} misses ({rate:.1f}% hit rate), sharing factor {last.get('sharing_factor', 0)}")
        if looked_up and rate < 5:
            print("    NOTE: interning is paying its full cost and returning almost nothing here.")

    oom = [r for r in rows if r.get("type") in ("oom_pressure", "oom_crash")]
    if oom:
        print(f"\n  heap events: {len(oom)}")
        for r in oom[-3:]:
            print(f"    {r.get('type')}: used={r.get('heap_used_mb')}MB live={r.get('heap_live_mb', '?')}MB max={r.get('heap_max_mb')}MB compiles={r.get('concurrent_compiles')}")


def main() -> int:
    root = pathlib.Path(sys.argv[1] if len(sys.argv) > 1 else ".")
    found = sorted(root.rglob("metrics.jsonl"))
    if not found:
        print(f"no metrics.jsonl under {root}")
        return 0
    for path in found:
        try:
            summarise(path, load(path))
        except Exception as e:  # noqa: BLE001 - a broken summary must not break the build
            print(f"could not summarise {path}: {e!r}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
