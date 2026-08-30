import React from "react";
import styles from "./styles.module.css";

/**
 * A plain, native `<details>`.
 *
 * Docusaurus swaps every `<details>` written in MDX for its own React component, which calls
 * `e.stopPropagation()` on every click inside it. React listens on the Docusaurus root element,
 * which is below `document` — so that call stops the event before it reaches `document`, where
 * libraries that use event delegation (solid-js, and therefore asciinema-player) listen. The
 * result is that every control inside a Docusaurus `<details>` is dead: play button, keyboard
 * shortcuts, progress bar.
 *
 * This component renders the native element instead, with no click handler of its own, so events
 * inside it propagate the way the platform intends.
 */
export default function PlainDetails({
  summary,
  children,
}: {
  summary: React.ReactNode;
  children: React.ReactNode;
}): JSX.Element {
  return (
    <details className={styles.details}>
      <summary className={styles.summary}>{summary}</summary>
      <div className={styles.content}>{children}</div>
    </details>
  );
}
