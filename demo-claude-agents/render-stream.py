#!/usr/bin/env python3
"""Live renderer for `claude -p --output-format stream-json`: turns the event
stream of the guided session into compact, vignette-style terminal lines -
who called which tool with what, and what came back. Used by record.sh to
turn the session into an asciinema recording.

Reads stream-json on stdin, writes rendered lines to stdout (line-buffered).
$DEMO_SCRATCH (if set) and $HOME are shortened to `~` in all output.
"""
import json
import os
import re
import sys

BOLD = "\033[1m"
DIM = "\033[2m"
CYAN = "\033[36m"
GREEN = "\033[32m"
RED = "\033[31m"
YELLOW = "\033[33m"
RESET = "\033[0m"

scratch = os.environ.get("DEMO_SCRATCH", "")
home = os.path.expanduser("~")


def shorten(s: str) -> str:
    if scratch:
        s = s.replace(scratch + "/repo", "~/main").replace(scratch, "~")
    s = s.replace(home, "~")
    s = s.replace("~/main/../", "~/")  # display only: collapse the prompt's <ABS>/../wt-x spelling
    return s


# tool_use id -> agent label, for tagging subagent activity
agents: dict[str, str] = {}


def label_for(parent_id):
    if parent_id is None:
        return f"{BOLD}orchestrator{RESET}"
    name = agents.get(parent_id, "agent")
    return f"{BOLD}agent[{name}]{RESET}"


def agent_name(tool_input) -> str:
    blob = json.dumps(tool_input)
    if "seeded" in blob:
        return "seeded"
    if "cold" in blob:
        return "cold"
    desc = str(tool_input.get("description", "agent"))
    return desc.split()[0] if desc else "agent"


def compact(value, limit: int) -> str:
    text = re.sub(r"\s+", " ", str(value)).strip()
    text = shorten(text)
    return text if len(text) <= limit else text[: limit - 1] + "…"


def render_tool_use(prefix: str, c) -> None:
    name = c.get("name", "")
    tool_input = c.get("input", {})
    if name in ("Agent", "Task"):
        agents[c.get("id", "")] = agent_name(tool_input)
        desc = tool_input.get("description", "")
        print(f"{prefix} {CYAN}spawns subagent{RESET} {YELLOW}{compact(desc, 60)}{RESET}")
    elif name == "Bash":
        print(f"{prefix} {CYAN}${RESET} {compact(tool_input.get('command', ''), 90)}")
    elif name.startswith("mcp__bleep__"):
        short = "bleep." + name.removeprefix("mcp__bleep__").removeprefix("bleep_")
        args = {k: shorten(str(v)) for k, v in tool_input.items()}
        arg_str = ", ".join(f"{k}: {v}" for k, v in args.items())
        print(f"{prefix} {CYAN}{short}{RESET} {{ {compact(arg_str, 100)} }}")
    else:
        pass  # harness plumbing (ToolSearch, ScheduleWakeup, ...) is not part of the story


def render_tool_result(prefix: str, c) -> None:
    content = c.get("content")
    if isinstance(content, list):
        texts = [b.get("text", "") for b in content if isinstance(b, dict) and b.get("type") == "text"]
        text = " ".join(texts)
    else:
        text = str(content or "")
    text = compact(text, 150)
    if not text:
        return
    # harness plumbing results are not part of the story
    if re.search(r"Async agent launched|internal metadata|Loop stopped|`prompt` is required|agentId|wakeup|task-notification", text):
        return
    color = RED if ('"success": false' in text or "failed" in text[:60]) else DIM
    print(f"{prefix} {DIM}→{RESET} {color}{text}{RESET}")


def main() -> None:
    for line in sys.stdin:
        try:
            event = json.loads(line)
        except json.JSONDecodeError:
            continue
        etype = event.get("type")
        prefix = label_for(event.get("parent_tool_use_id"))
        message = event.get("message") or {}
        content = message.get("content")
        if not isinstance(content, list):
            continue
        if etype == "assistant":
            for c in content:
                if c.get("type") == "tool_use":
                    render_tool_use(prefix, c)
                elif c.get("type") == "text" and c.get("text", "").strip():
                    for out_line in c["text"].strip().splitlines():
                        stripped = out_line.strip()
                        if stripped in ("", "```"):
                            continue
                        print(f"{prefix} {shorten(out_line)}")
        elif etype == "user":
            for c in content:
                if isinstance(c, dict) and c.get("type") == "tool_result":
                    render_tool_result(prefix, c)
        sys.stdout.flush()


if __name__ == "__main__":
    main()
