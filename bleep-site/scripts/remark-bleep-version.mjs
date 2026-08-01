/**
 * The token documentation writes instead of a literal bleep version.
 *
 * Deliberately not a general "replace any version-shaped string" rewrite. Several pages refer to specific past releases
 * on purpose ("the machine-wide accounting arrived after 1.0.0-M10"), and rewriting those to the current version would
 * silently turn true statements into false ones. Only this explicit token is substituted.
 */
export const VERSION_PLACEHOLDER = "BLEEP_LATEST_VERSION";

/** Node types whose `value` is rendered text a reader can see. `code` covers fenced blocks, which is where `$version:` lives. */
const TEXTUAL_NODES = new Set(["code", "inlineCode", "text"]);

/**
 * Replaces {@link VERSION_PLACEHOLDER} with the newest released version, everywhere it appears in the docs.
 *
 * This runs as a remark plugin rather than as a build-time find-and-replace over the source files because the docs are
 * checked in: rewriting them in place would mean a commit on every release, and a dirty working tree for anyone who
 * built the site locally.
 *
 * MDX has no interpolation inside fenced code blocks, which is exactly where the version is needed, so operating on the
 * parsed tree is the only option that reaches it.
 */
export default function remarkBleepVersion({ version }) {
  if (!version) {
    throw new Error("remark-bleep-version requires a 'version' option");
  }

  return (tree) => {
    visit(tree);
  };

  function visit(node) {
    if (TEXTUAL_NODES.has(node.type) && typeof node.value === "string") {
      node.value = node.value.split(VERSION_PLACEHOLDER).join(version);
    }
    if (Array.isArray(node.children)) {
      for (const child of node.children) {
        visit(child);
      }
    }
  }
}
