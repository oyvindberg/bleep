#!/usr/bin/env node

/**
 * Extracts code snippets from the docs-snippets-{java,kotlin,scala} projects.
 *
 * Each file may contain a snippet bracketed by:
 *
 *   //start
 *   ... code ...
 *   //stop
 *
 * Files without markers expose their full content (minus package/imports
 * header) as the snippet. Files with markers also keep the full file
 * available for the website's "show entire file" toggle.
 *
 * Output: bleep-site/static/snippets/snippets.json — keyed by relative
 * path, consumed by the <Snippet/> React component.
 */

const fs = require("fs");
const path = require("path");
const { glob } = require("glob");

const ROOT_DIR = path.resolve(__dirname, "../..");
const OUTPUT_DIR = path.resolve(__dirname, "../static/snippets");

function dedent(code) {
  const lines = code.split("\n");
  let minIndent = Infinity;
  for (const line of lines) {
    if (line.trim().length === 0) continue;
    const indent = line.match(/^(\s*)/)[1].length;
    minIndent = Math.min(minIndent, indent);
  }
  if (minIndent === Infinity) minIndent = 0;
  return lines
    .map((line) => line.slice(minIndent))
    .join("\n")
    .trim();
}

function stripMarkerLines(content) {
  return content
    .split("\n")
    .filter(
      (line) => !line.match(/\/\/\s*start\b/) && !line.match(/\/\/\s*stop\b/)
    )
    .join("\n");
}

function extractContentWithoutHeader(content) {
  const lines = content.split("\n");
  let contentStartIndex = 0;
  for (let i = 0; i < lines.length; i++) {
    const trimmed = lines[i].trim();
    if (
      trimmed.startsWith("package ") ||
      trimmed.startsWith("import ") ||
      trimmed === ""
    ) {
      contentStartIndex = i + 1;
    } else {
      break;
    }
  }
  return dedent(lines.slice(contentStartIndex).join("\n"));
}

function extractSnippet(content, filePath) {
  const startRegex = /\/\/\s*start\b/;
  const stopRegex = /\/\/\s*stop\b/;
  const startMatch = content.match(startRegex);
  const stopMatch = content.match(stopRegex);

  if (startMatch && !stopMatch)
    throw new Error(`File "${filePath}" has //start but no matching //stop`);
  if (stopMatch && !startMatch)
    throw new Error(`File "${filePath}" has //stop but no matching //start`);

  const allStarts = content.match(/\/\/\s*start\b/g);
  if (allStarts && allStarts.length > 1)
    throw new Error(
      `File "${filePath}" has ${allStarts.length} //start markers. Only one snippet per file is allowed — split into separate files.`
    );

  if (!startMatch) {
    return {
      snippet: extractContentWithoutHeader(content),
      fullContent: content.trim(),
      hasMarkers: false,
    };
  }

  const startIndex = startMatch.index + startMatch[0].length;
  const stopIndex = content.indexOf(stopMatch[0]);
  const snippetContent = content.slice(startIndex, stopIndex);

  return {
    snippet: dedent(snippetContent),
    fullContent: stripMarkerLines(content).trim(),
    hasMarkers: true,
  };
}

function getSnippetNameFromPath(filePath) {
  // e.g. docs-snippets-java/src/java/bleep/docs/tutorials/Hello.java
  // becomes: java/tutorials/Hello
  const relativePath = path.relative(ROOT_DIR, filePath);
  let language;
  if (relativePath.startsWith("docs-snippets-java")) language = "java";
  else if (relativePath.startsWith("docs-snippets-kotlin")) language = "kotlin";
  else if (relativePath.startsWith("docs-snippets-scala")) language = "scala";
  else throw new Error(`Unknown module: ${relativePath}`);

  const match = relativePath.match(/bleep\/docs\/([^/]+)\/([^.]+)/);
  if (!match) throw new Error(`Could not parse path: ${relativePath}`);
  return `${language}/${match[1]}/${match[2]}`;
}

async function processFile(filePath) {
  const content = fs.readFileSync(filePath, "utf8");
  const relativePath = path.relative(ROOT_DIR, filePath);
  const { snippet, fullContent, hasMarkers } = extractSnippet(
    content,
    relativePath
  );
  const name = getSnippetNameFromPath(filePath);
  return { name, path: relativePath, snippet, fullContent, hasMarkers };
}

async function main() {
  console.log("Extracting code snippets...");
  const snippetPatterns = [
    "docs-snippets-java/src/java/bleep/docs/**/*.java",
    "docs-snippets-kotlin/src/kotlin/bleep/docs/**/*.kt",
    "docs-snippets-scala/src/scala/bleep/docs/**/*.scala",
  ];
  // docs-snippets-from-tests/* directories are written verbatim by integration
  // tests (see bleep-tests/.../*IT.scala) and git-tracked. Pull every text file
  // under each example so docs can include any of them via
  // <Snippet path="docs-snippets-from-tests/.../bleep.yaml" />.
  const buildPatterns = [
    "docs-snippets-from-tests/**/*.yaml",
    "docs-snippets-from-tests/**/*.yml",
    "docs-snippets-from-tests/**/*.java",
    "docs-snippets-from-tests/**/*.kt",
    "docs-snippets-from-tests/**/*.scala",
    "docs-snippets-from-tests/**/*.md",
  ];

  const snippetFiles = [];
  for (const pattern of snippetPatterns) {
    const files = await glob(pattern, { cwd: ROOT_DIR });
    snippetFiles.push(...files.map((f) => path.join(ROOT_DIR, f)));
  }

  const buildFiles = [];
  for (const pattern of buildPatterns) {
    const files = await glob(pattern, { cwd: ROOT_DIR });
    buildFiles.push(...files.map((f) => path.join(ROOT_DIR, f)));
  }

  console.log(`Found ${snippetFiles.length} snippet files, ${buildFiles.length} doc-build files`);

  const allSnippets = {};
  const errors = [];

  // Snippet files: split into snippet (between markers) + fullContent.
  for (const file of snippetFiles) {
    try {
      const result = await processFile(file);
      allSnippets[result.path] = {
        name: result.name,
        snippet: result.snippet,
        fullContent: result.fullContent,
      };
      console.log(
        `  snippet ${result.name}: ${result.hasMarkers ? "has markers" : "no markers"}`
      );
    } catch (err) {
      errors.push(err.message);
    }
  }

  // docs-snippets-from-tests files: full content only. Same json key (relative
  // path) so <Snippet path="..." /> can fetch them by raw path.
  for (const file of buildFiles) {
    try {
      const content = fs.readFileSync(file, "utf8");
      const relativePath = path.relative(ROOT_DIR, file);
      allSnippets[relativePath] = {
        name: relativePath,
        snippet: content.trim(),
        fullContent: content.trim(),
      };
      console.log(`  from-test ${relativePath}`);
    } catch (err) {
      errors.push(`Failed to read ${file}: ${err.message}`);
    }
  }

  if (errors.length > 0) {
    console.error("\nErrors found:");
    for (const error of errors) console.error(`  - ${error}`);
    process.exit(1);
  }

  fs.mkdirSync(OUTPUT_DIR, { recursive: true });
  const outputPath = path.join(OUTPUT_DIR, "snippets.json");
  fs.writeFileSync(outputPath, JSON.stringify(allSnippets, null, 2));
  console.log(
    `\nExtracted ${Object.keys(allSnippets).length} files to ${path.relative(
      ROOT_DIR,
      outputPath
    )}`
  );
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
