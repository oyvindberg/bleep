import { execFileSync } from "node:child_process";
import path from "node:path";
import { fileURLToPath } from "node:url";

const repoRoot = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..", "..");

/**
 * The newest released bleep version, derived from the repository's own git tags.
 *
 * Releases are cut by pushing a `v<version>` tag, so the tags ARE the release history and nothing else has to be kept in
 * sync by hand. `--sort=-v:refname` is git's version sort, which orders `M10` above `M9` numerically (a plain lexical
 * sort does not) and orders a final release above its own prereleases, so a future `v1.0.0` wins over `v1.0.0-M11`.
 *
 * Throws when no tag is found rather than guessing. The usual cause is a shallow clone: `actions/checkout` fetches no
 * tags at depth 1, so any workflow calling this needs `fetch-depth: 0`. A guessed or stale version here would be
 * published as documentation telling people to depend on a version that may not exist, which is worse than a failed
 * build.
 */
export function latestBleepVersion() {
  const stdout = execFileSync("git", ["tag", "-l", "v*", "--sort=-v:refname"], {
    cwd: repoRoot,
    encoding: "utf8",
  });

  const newest = stdout
    .split("\n")
    .map((line) => line.trim())
    .filter((line) => line.length > 0)[0];

  if (!newest) {
    throw new Error(
      `No 'v*' git tags found in ${repoRoot}, so the latest bleep version cannot be determined. ` +
        `If this is CI, the checkout needs 'fetch-depth: 0' — tags are not fetched at depth 1.`,
    );
  }

  return newest.replace(/^v/, "");
}
