---
name: deploy-local
description: Build the current branch into a native-image bleep binary, publish matching snapshot jars, install to ~/.local/bin/bleep, and cycle the daemons. Use when the user says "deploy", "install this branch", or wants their local bleep to run the code they just changed.
---

# Deploy a local snapshot of bleep

Turns the current checkout into the `bleep` binary on PATH, with the daemon jars it
will resolve at runtime published to match. The whole flow takes ~3 minutes, most of
it native-image.

## The version invariant (read this first)

The client binary, the `bleep-bsp` server jars, and `bleep-test-runner` must all be
the **same** version. The client asks coursier for the server at the version baked
into it by `bleep sourcegen` (`model.BleepVersion.current`). Publishing with an
explicit `--version` changes only the coordinate the artifacts land under — not what
the client looks for. Let everything default to the git-derived version and they line
up; override one and the client silently fetches a stale server.

A **clean working tree** makes the version stable (e.g. `1.0.0-M12+15-b322f6f4-SNAPSHOT`).
A dirty tree makes dynver append a minute-resolution timestamp, so `sourcegen` and
`publish` minutes apart produce DIFFERENT versions. Prefer committing/stashing first.
If you must deploy dirty: build the binary, read the version out of it with `strings`,
and pin the publish to exactly that string with `--version` — the one case where
`--version` is correct.

## Steps

All from the workspace root. Always `--no-color` (and `--no-tui` where accepted).

1. **Bake the version**
   ```
   bleep sourcegen --no-color
   ```
   Confirm what got baked:
   ```
   grep -ho 'M[0-9][0-9]*+[^"]*' .bleep/projects/bleep-model/generated-sources/bleep.scripts.GenerateResources/bleep/model/BleepVersion.scala
   ```

2. **Compile the CLI** — mandatory. `GenNativeImage` never compiles anything; it
   images whatever classes are on disk. Skipping this produces a *successful* build
   of stale code with no warning.
   ```
   bleep compile bleep-cli --no-color --no-tui
   ```

3. **Publish the snapshot jars** (all publishable projects, incl. `bleep-bsp` and
   `bleep-test-runner`):
   ```
   bleep publish local-ivy --no-color --no-tui
   ```
   Every line must show the same version as step 1. Spot-check:
   `ls ~/.ivy2/local/build.bleep/bleep-bsp_3/ | tail -3`

4. **Build the native image** (~1-2 min):
   ```
   bleep native-image --no-color
   ```
   Artifact: `.bleep/projects/bleep-cli/builds/normal/target/native-image/bleep-cli`

5. **Verify the artifact — never trust the exit code.** Both checks, on the artifact:
   ```
   strings <artifact> | grep -oE '1\.0\.0-M[0-9]+\+[0-9]+-[a-f0-9]+(-SNAPSHOT)?' | sort -u
   ```
   Must print exactly ONE version, identical to steps 1 and 3. If the change adds a
   distinctive symbol/string, grep for that too. Stale-classes builds have shipped
   twice; this check is what catches them.

6. **Install with `mv`, never `cp`.** `cp` over the old binary breaks the macOS
   signature — the result dies with SIGKILL (exit 137, zero output), indistinguishable
   from a broken build. `mv` keeps the signature valid.
   ```
   mv ~/.local/bin/bleep ~/.local/bin/bleep.prev-<version-shorthand>
   mv <artifact> ~/.local/bin/bleep
   ~/.local/bin/bleep --help >/dev/null; echo $?   # 0, not 137
   ```
   (If a copy is ever unavoidable: `codesign --force --sign - ~/.local/bin/bleep`.)

7. **Cycle the daemons.** Old daemons keep serving the jars they booted with, so the
   just-published code silently isn't what runs. First preserve metrics (stopping a
   daemon deletes its socket dir and the `metrics.jsonl` history with it):
   ```
   mkdir -p <somewhere>/metrics-backup-$(date +%Y%m%d-%H%M%S)
   cp ~/Library/Caches/build.bleep/socket/*/metrics.jsonl <that dir>/  # rename per socket hash
   bleep config compile-server stop-all --no-color
   ```
   Then hunt orphans — `stop-all` only knows registered socket dirs and MISSES
   daemons from older versions:
   ```
   for pid in $(pgrep -f BspServerDaemon); do echo "$pid $(ps -p $pid -o command= | grep -oE 'bleep-bsp_3/[^/]+')"; done
   ```
   `kill` any survivors. Target state: zero `BspServerDaemon` processes.

8. **Smoke-test end-to-end** — proves the client finds its server jars at the baked
   version and a fresh daemon comes up on them:
   ```
   ~/.local/bin/bleep compile bleep-model --no-color --no-tui
   for pid in $(pgrep -f BspServerDaemon); do ps -p $pid -o command= | grep -oE 'bleep-bsp_3/[^/]+'; done
   ```
   The daemon's classpath version must equal step 1's version.

9. **Restart long-lived processes that pin jars at startup**: any `bleep mcp-server`
   (use the `bleep.restart` MCP tool from a live session, or kill the process — the
   host respawns it). IDE BSP sessions reconnect on their own next action.

   **Expected side effect:** `bleep.restart` drops the CALLING session's MCP
   connection — the bleep tools are delisted the moment the process exits, and the
   respawn is lazy: it can take several use attempts or a turn boundary before the
   tools come back on their own (`/mcp` reconnects immediately; a brand-new session
   always gets the new binary via the user-scope registration). So sequence the
   deploy: finish any MCP-dependent work first, call `bleep.restart` last, and be
   ready to fall back to the CLI for the rest of the turn.

   While the tools are delisted, "No such tool available" says NOTHING about any
   project — never conclude from it that a checkout isn't a bleep build, and never
   switch build tools because of it. `bleep.yaml` on disk decides what builds a
   project; the CLI works throughout.

## Failure modes seen in the wild

- Binary runs old code, build was green → step 2 skipped, or verified exit code
  instead of `strings` (step 5).
- Client downloads/looks for a server version that doesn't exist locally → version
  skew from a dirty tree or an explicit `--version` (see invariant above).
- New binary exits 137 instantly with no output → installed with `cp` (step 6).
- Code change "doesn't take effect" after a green deploy → a stale daemon or
  `mcp-server` is still serving old jars (steps 7 and 9).
