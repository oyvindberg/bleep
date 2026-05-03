import React, { useState } from "react";
import Tabs from "@theme/Tabs";
import TabItem from "@theme/TabItem";
import CodeBlock from "@theme/CodeBlock";
import styles from "./styles.module.css";

import snippetsData from "@site/static/snippets/snippets.json";

interface FileData {
  name: string;
  snippet: string;
  fullContent: string;
}

const allSnippets: Record<string, FileData> = snippetsData as Record<
  string,
  FileData
>;

type Lang = "java" | "kotlin" | "scala";

type CodeLanguage = Lang | "yaml" | "yml" | "bash" | "shell" | "text";

interface FileMode {
  /** Path under each language's docs root, without extension. */
  /** e.g. "tutorials/Hello" → docs-snippets-java/src/java/bleep/docs/tutorials/Hello.java (and the kotlin/scala equivalents). */
  file: string;
  /** Show only one language. Omit for tabs (Java/Kotlin/Scala). */
  lang?: Lang;
  /** Hide the "Show entire file" toggle. */
  hideFullFile?: boolean;
  path?: never;
}

interface PathMode {
  /** Raw path relative to repo root, e.g. "docs-snippets-from-tests/first-script-java/bleep.yaml". */
  path: string;
  /** Highlighting language. Required (we can't infer from extension reliably for yaml/yml/etc.). */
  lang: CodeLanguage;
  file?: never;
  hideFullFile?: never;
}

type SnippetProps = FileMode | PathMode;

function buildPath(file: string, lang: Lang): string {
  const ext = lang === "java" ? ".java" : lang === "kotlin" ? ".kt" : ".scala";
  return `docs-snippets-${lang}/src/${lang}/bleep/docs/${file}${ext}`;
}

function getSnippetCode(path: string, showFull: boolean): string {
  const fileData = allSnippets[path];
  if (!fileData) {
    throw new Error(
      `Snippet file not found: "${path}".\n\n` +
        `Make sure the file exists in docs-snippets-{java,kotlin,scala} or docs-snippets-from-tests/.\n` +
        `Available files: ${Object.keys(allSnippets).slice(0, 5).join(", ")}...`
    );
  }
  return showFull ? fileData.fullContent : fileData.snippet;
}

export default function Snippet(props: SnippetProps): JSX.Element {
  const [showFullFile, setShowFullFile] = useState(false);

  // path= mode: render a single arbitrary file. No tabs, no toggle.
  if ("path" in props && props.path !== undefined) {
    return (
      <div className={styles.snippetContainer}>
        <div className={styles.tabsWrapper}>
          <CodeBlock language={props.lang}>
            {getSnippetCode(props.path, false)}
          </CodeBlock>
        </div>
      </div>
    );
  }

  // file= mode: language tabs (or single-language) over docs-snippets-*.
  const { file, lang, hideFullFile } = props as FileMode;
  const langs: Lang[] = lang ? [lang] : ["java", "kotlin", "scala"];
  const codeBlocks = langs.map((l) => ({
    lang: l,
    path: buildPath(file, l),
  }));

  const content = (
    <>
      {langs.length === 1 ? (
        <CodeBlock language={codeBlocks[0].lang}>
          {getSnippetCode(codeBlocks[0].path, showFullFile)}
        </CodeBlock>
      ) : (
        <Tabs groupId="language">
          {codeBlocks.map(({ lang: l, path }) => (
            <TabItem
              key={l}
              value={l}
              label={l.charAt(0).toUpperCase() + l.slice(1)}
            >
              <CodeBlock language={l}>
                {getSnippetCode(path, showFullFile)}
              </CodeBlock>
            </TabItem>
          ))}
        </Tabs>
      )}
    </>
  );

  return (
    <div className={styles.snippetContainer}>
      <div className={styles.tabsWrapper}>
        {content}
        {!hideFullFile && (
          <label className={styles.checkbox}>
            <input
              type="checkbox"
              checked={showFullFile}
              onChange={(e) => setShowFullFile(e.target.checked)}
            />
            <span>Show entire file</span>
          </label>
        )}
      </div>
    </div>
  );
}
