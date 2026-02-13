#!/usr/bin/env python3
# /// script
# requires-python = ">=3.9"
# dependencies = [
#     "plotly>=5.18",
#     "pandas>=2.0",
# ]
# ///
"""
BSP Benchmark Dashboard Generator

Reads metrics.jsonl (from BspMetrics) and workspace CSV files (from benchmark-bsp.sh)
and generates an interactive HTML dashboard using Plotly.

Usage:
    uv run benchmark/visualize.py <metrics.jsonl> <ws-1.csv> [ws-2.csv ...] [-o output.html]
"""

import argparse
import json
import sys
from pathlib import Path

import pandas as pd
import plotly.graph_objects as go
from plotly.subplots import make_subplots


def load_metrics(path):
    """Load JSONL metrics file into categorized lists."""
    events = {
        "jvm": [],
        "compile_start": [],
        "compile_end": [],
        "build_start": [],
        "build_end": [],
        "cache_evict": [],
        "clean_cache": [],
        "connection_open": [],
        "connection_close": [],
        "sourcegen_start": [],
        "sourcegen_end": [],
        "summary": [],
    }
    with open(path) as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            try:
                obj = json.loads(line)
                etype = obj.get("type", "")
                if etype in events:
                    events[etype].append(obj)
            except json.JSONDecodeError:
                continue
    return events


def load_workspace_csv(path):
    """Load a workspace CSV file."""
    return pd.read_csv(path)


def ts_to_datetime(ts_ms):
    """Convert millisecond timestamp to datetime."""
    return pd.to_datetime(ts_ms, unit="ms")


def build_dashboard(metrics_path, csv_paths, output_path):
    events = load_metrics(metrics_path)

    # Load workspace CSVs
    ws_data = {}
    for p in csv_paths:
        name = Path(p).stem  # e.g. "ws-1"
        ws_data[name] = load_workspace_csv(p)

    # Determine time range
    all_ts = []
    for etype in events:
        for e in events[etype]:
            if "ts" in e:
                all_ts.append(e["ts"])
    if not all_ts:
        print("No timestamps found in metrics file", file=sys.stderr)
        sys.exit(1)

    t0 = min(all_ts)

    # Helper: relative seconds from start
    def rel_s(ts_ms):
        return (ts_ms - t0) / 1000.0

    # Create figure with subplots
    fig = make_subplots(
        rows=5,
        cols=2,
        subplot_titles=[
            "Heap Memory (MB)",
            "GC Activity",
            "Thread Count",
            "CPU Load",
            "Concurrent Compilations",
            "Per-Project Compile Duration",
            "Build Duration Over Time",
            "Compilation Timeline",
            "Cache & Connection Events",
            "Summary Statistics",
        ],
        vertical_spacing=0.06,
        horizontal_spacing=0.08,
        specs=[
            [{"type": "scatter"}, {"type": "scatter"}],
            [{"type": "scatter"}, {"type": "scatter"}],
            [{"type": "scatter"}, {"type": "scatter"}],
            [{"type": "scatter"}, {"type": "bar"}],
            [{"type": "scatter"}, {"type": "table"}],
        ],
    )

    # ---- 1. Heap Memory ----
    jvm = events["jvm"]
    if jvm:
        jvm_t = [rel_s(e["ts"]) for e in jvm]
        fig.add_trace(
            go.Scatter(x=jvm_t, y=[e["heap_used_mb"] for e in jvm], name="Heap Used", line=dict(color="blue")),
            row=1, col=1,
        )
        fig.add_trace(
            go.Scatter(x=jvm_t, y=[e["heap_committed_mb"] for e in jvm], name="Heap Committed", line=dict(color="orange", dash="dash")),
            row=1, col=1,
        )
        fig.add_trace(
            go.Scatter(x=jvm_t, y=[e["heap_max_mb"] for e in jvm], name="Heap Max", line=dict(color="red", dash="dot")),
            row=1, col=1,
        )

    # ---- 2. GC Activity ----
    if jvm:
        # Aggregate GC across all collectors
        gc_times = []
        gc_counts = []
        for e in jvm:
            total_count = sum(g["count"] for g in e.get("gc", []))
            total_time = sum(g["time_ms"] for g in e.get("gc", []))
            gc_counts.append(total_count)
            gc_times.append(total_time)
        fig.add_trace(
            go.Scatter(x=jvm_t, y=gc_counts, name="GC Count (cumulative)", line=dict(color="purple")),
            row=1, col=2,
        )
        fig.add_trace(
            go.Scatter(x=jvm_t, y=gc_times, name="GC Time (ms, cumulative)", line=dict(color="green")),
            row=1, col=2,
        )

    # ---- 3. Thread Count ----
    if jvm:
        fig.add_trace(
            go.Scatter(x=jvm_t, y=[e["threads"] for e in jvm], name="Threads", line=dict(color="teal")),
            row=2, col=1,
        )
        fig.add_trace(
            go.Scatter(x=jvm_t, y=[e["peak_threads"] for e in jvm], name="Peak Threads", line=dict(color="red", dash="dot")),
            row=2, col=1,
        )
        fig.add_trace(
            go.Scatter(x=jvm_t, y=[e["daemon_threads"] for e in jvm], name="Daemon Threads", line=dict(color="gray", dash="dash")),
            row=2, col=1,
        )

    # ---- 4. CPU Load ----
    if jvm:
        fig.add_trace(
            go.Scatter(x=jvm_t, y=[e["cpu_process"] for e in jvm], name="Process CPU", line=dict(color="blue")),
            row=2, col=2,
        )
        fig.add_trace(
            go.Scatter(x=jvm_t, y=[e["cpu_system"] for e in jvm], name="System CPU", line=dict(color="orange")),
            row=2, col=2,
        )

    # ---- 5. Concurrent Compilations ----
    if jvm:
        fig.add_trace(
            go.Scatter(
                x=jvm_t,
                y=[e["concurrent_compiles"] for e in jvm],
                name="Concurrent Compiles",
                fill="tozeroy",
                line=dict(color="darkblue"),
            ),
            row=3, col=1,
        )

    # ---- 6. Per-Project Compile Duration (scatter) ----
    compile_ends = events["compile_end"]
    if compile_ends:
        # Color by workspace
        workspaces = sorted(set(e.get("workspace", "") for e in compile_ends))
        colors = ["blue", "red", "green", "orange", "purple", "brown"]
        for i, ws in enumerate(workspaces):
            ws_compiles = [e for e in compile_ends if e.get("workspace", "") == ws]
            ws_label = Path(ws).name if ws else "unknown"
            fig.add_trace(
                go.Scatter(
                    x=[rel_s(e["ts"]) for e in ws_compiles],
                    y=[e["duration_ms"] for e in ws_compiles],
                    mode="markers",
                    name=f"Compile {ws_label}",
                    marker=dict(color=colors[i % len(colors)], size=4, opacity=0.6),
                    text=[e["project"] for e in ws_compiles],
                    hovertemplate="%{text}<br>%{y}ms<extra></extra>",
                ),
                row=3, col=2,
            )

    # ---- 7. Build Duration Over Time ----
    build_ends = events["build_end"]
    if build_ends:
        workspaces = sorted(set(e.get("workspace", "") for e in build_ends))
        colors = ["blue", "red", "green", "orange", "purple", "brown"]
        for i, ws in enumerate(workspaces):
            ws_builds = sorted([e for e in build_ends if e.get("workspace", "") == ws], key=lambda e: e["ts"])
            ws_label = Path(ws).name if ws else "unknown"
            fig.add_trace(
                go.Scatter(
                    x=[rel_s(e["ts"]) for e in ws_builds],
                    y=[e["duration_ms"] for e in ws_builds],
                    name=f"Build {ws_label}",
                    line=dict(color=colors[i % len(colors)]),
                    mode="lines+markers",
                ),
                row=4, col=1,
            )

    # Also overlay workspace CSV data if available
    for ws_name, df in ws_data.items():
        compiles = df[df["operation"] == "compile"]
        if not compiles.empty:
            fig.add_trace(
                go.Scatter(
                    x=[rel_s(t) for t in compiles["end_ms"]],
                    y=compiles["duration_ms"],
                    name=f"CLI {ws_name}",
                    line=dict(dash="dot"),
                    mode="lines+markers",
                    marker=dict(size=5),
                ),
                row=4, col=1,
            )

    # ---- 8. Compilation Timeline (horizontal bars) ----
    compile_starts = events["compile_start"]
    if compile_starts and compile_ends:
        # Match starts to ends by project+workspace
        start_map = {}
        for e in compile_starts:
            key = (e.get("project", ""), e.get("workspace", ""))
            start_map.setdefault(key, []).append(e)

        bar_data = []
        for e in compile_ends:
            key = (e.get("project", ""), e.get("workspace", ""))
            starts = start_map.get(key, [])
            if starts:
                s = starts.pop(0)
                bar_data.append({
                    "project": e.get("project", ""),
                    "workspace": Path(e.get("workspace", "")).name,
                    "start": rel_s(s["ts"]),
                    "end": rel_s(e["ts"]),
                    "duration_ms": e["duration_ms"],
                    "success": e.get("success", True),
                })

        if bar_data:
            # Show up to 200 bars (most recent)
            bar_data = bar_data[-200:]
            fig.add_trace(
                go.Bar(
                    x=[b["duration_ms"] for b in bar_data],
                    y=[f"{b['project']} ({b['workspace']})" for b in bar_data],
                    orientation="h",
                    name="Compile Tasks",
                    marker=dict(
                        color=["green" if b["success"] else "red" for b in bar_data],
                        opacity=0.7,
                    ),
                    hovertemplate="%{y}<br>%{x}ms<extra></extra>",
                ),
                row=4, col=2,
            )

    # ---- 9. Cache & Connection Events ----
    cache_evicts = events["cache_evict"]
    clean_caches = events["clean_cache"]
    conn_opens = events["connection_open"]
    conn_closes = events["connection_close"]

    event_markers = []
    event_labels = []
    event_colors = []

    for e in cache_evicts:
        event_markers.append(rel_s(e["ts"]))
        event_labels.append(f"evict: {e.get('cache', '')}")
        event_colors.append("red")
    for e in clean_caches:
        event_markers.append(rel_s(e["ts"]))
        event_labels.append(f"clean: {e.get('project', '')}")
        event_colors.append("orange")
    for e in conn_opens:
        event_markers.append(rel_s(e["ts"]))
        event_labels.append(f"conn open #{e.get('conn_id', '')}")
        event_colors.append("green")
    for e in conn_closes:
        event_markers.append(rel_s(e["ts"]))
        event_labels.append(f"conn close #{e.get('conn_id', '')}")
        event_colors.append("gray")

    if event_markers:
        fig.add_trace(
            go.Scatter(
                x=event_markers,
                y=[1] * len(event_markers),
                mode="markers+text",
                text=event_labels,
                textposition="top center",
                marker=dict(color=event_colors, size=10, symbol="diamond"),
                name="Events",
                hovertemplate="%{text}<br>t=%{x:.1f}s<extra></extra>",
            ),
            row=5, col=1,
        )

    # ---- 10. Summary Statistics ----
    total_compiles = len(compile_ends)
    successful_compiles = sum(1 for e in compile_ends if e.get("success", True))
    failed_compiles = total_compiles - successful_compiles
    avg_compile_ms = sum(e["duration_ms"] for e in compile_ends) / total_compiles if total_compiles else 0
    max_concurrent = max((e.get("concurrent_compiles", 0) for e in jvm), default=0) if jvm else 0
    max_heap = max((e.get("heap_used_mb", 0) for e in jvm), default=0) if jvm else 0
    total_gc_ms = max((sum(g["time_ms"] for g in e.get("gc", [])) for e in jvm), default=0) if jvm else 0
    total_builds = len(build_ends)
    avg_build_ms = sum(e["duration_ms"] for e in build_ends) / total_builds if total_builds else 0

    # Summary from metrics
    summary = events["summary"]
    summary_max_concurrent = summary[0]["max_concurrent_compiles"] if summary else max_concurrent
    summary_max_heap = summary[0]["max_heap_used_mb"] if summary else max_heap

    headers = ["Metric", "Value"]
    cells = [
        ["Total Builds", str(total_builds)],
        ["Total Compiles", str(total_compiles)],
        ["Successful Compiles", str(successful_compiles)],
        ["Failed Compiles", str(failed_compiles)],
        ["Avg Compile Duration", f"{avg_compile_ms:.0f} ms"],
        ["Avg Build Duration", f"{avg_build_ms:.0f} ms"],
        ["Max Concurrent Compiles", str(summary_max_concurrent)],
        ["Max Heap Used", f"{summary_max_heap} MB"],
        ["Total GC Time", f"{total_gc_ms} ms"],
        ["Cache Evictions", str(len(cache_evicts))],
        ["Cache Cleans", str(len(clean_caches))],
        ["Connections", str(len(conn_opens))],
    ]

    fig.add_trace(
        go.Table(
            header=dict(values=headers, fill_color="paleturquoise", align="left"),
            cells=dict(
                values=[[c[0] for c in cells], [c[1] for c in cells]],
                fill_color="lavender",
                align="left",
            ),
        ),
        row=5, col=2,
    )

    # Layout
    fig.update_layout(
        title="BSP Server Benchmark Dashboard",
        height=2000,
        showlegend=True,
        legend=dict(orientation="h", yanchor="bottom", y=1.01, xanchor="right", x=1),
    )

    # Axis labels
    for row in range(1, 6):
        for col in range(1, 3):
            fig.update_xaxes(title_text="Time (s)", row=row, col=col)

    fig.update_yaxes(title_text="MB", row=1, col=1)
    fig.update_yaxes(title_text="Count / ms", row=1, col=2)
    fig.update_yaxes(title_text="Count", row=2, col=1)
    fig.update_yaxes(title_text="Load (0-1)", row=2, col=2)
    fig.update_yaxes(title_text="Count", row=3, col=1)
    fig.update_yaxes(title_text="Duration (ms)", row=3, col=2)
    fig.update_yaxes(title_text="Duration (ms)", row=4, col=1)
    fig.update_xaxes(title_text="Duration (ms)", row=4, col=2)

    # Write HTML
    fig.write_html(output_path, include_plotlyjs="cdn")
    print(f"Dashboard written to: {output_path}")


def main():
    parser = argparse.ArgumentParser(description="BSP Benchmark Dashboard Generator")
    parser.add_argument("metrics", help="Path to metrics.jsonl")
    parser.add_argument("csvs", nargs="+", help="Workspace CSV files (ws-1.csv, ws-2.csv, ...)")
    parser.add_argument("-o", "--output", help="Output HTML file (default: dashboard.html)")

    args = parser.parse_args()
    output = args.output
    if output is None:
        output = str(Path(args.metrics).parent / "dashboard.html")

    build_dashboard(args.metrics, args.csvs, output)


if __name__ == "__main__":
    main()
