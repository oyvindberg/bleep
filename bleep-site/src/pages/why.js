import React, { useEffect, useRef, useState } from "react";
import Link from "@docusaurus/Link";
import Layout from "@theme/Layout";
import Snippet from "@site/src/components/Snippet";
import styles from "./index.module.css";

/* ------------------------------------------------------------------
   /why — the pitch, at full volume.
   A parallel front page: same design system as index.js, different
   register. The regular front page explains; this one argues.
   ------------------------------------------------------------------ */

function Vignette({ rows }) {
  return (
    <div className={styles.vignette}>
      {rows.map((r, i) =>
        r.gap ? (
          <div key={i} className={styles.vgGap} />
        ) : (
          <div key={i} className={styles.vgRow}>
            <span className={r.hot ? styles.vgActorHot : styles.vgActor}>
              {r.actor}
            </span>
            <span className={styles.vgLines}>
              <span className={r.deed ? styles.vgDeed : styles.vgCall}>
                {r.deed || r.call}
              </span>
              {r.result && (
                <span className={styles.vgResult}>
                  {"→ "}
                  <span
                    className={
                      r.bad ? styles.vgBad : r.good ? styles.vgGood : undefined
                    }
                  >
                    {r.result}
                  </span>
                  {r.note && (
                    <span className={styles.vgNote}>{"  ← " + r.note}</span>
                  )}
                </span>
              )}
            </span>
          </div>
        )
      )}
    </div>
  );
}

function Reveal({ children, delay, as: Tag, className, ...rest }) {
  const Component = Tag || "div";
  const cls = className || "";
  const ref = useRef(null);
  const [visible, setVisible] = useState(false);

  useEffect(() => {
    if (typeof IntersectionObserver === "undefined") {
      setVisible(true);
      return;
    }
    const el = ref.current;
    if (!el) return;
    const io = new IntersectionObserver(
      ([entry]) => {
        if (entry.isIntersecting) {
          setVisible(true);
          io.unobserve(el);
        }
      },
      { threshold: 0.12, rootMargin: "0px 0px -8% 0px" }
    );
    io.observe(el);
    return () => io.disconnect();
  }, []);

  return (
    <Component
      ref={ref}
      className={`${styles.reveal} ${visible ? styles.isVisible : ""} ${cls}`}
      style={delay ? { transitionDelay: `${delay}ms` } : undefined}
      {...rest}
    >
      {children}
    </Component>
  );
}

function SectionHeader({ eyebrow, title, children }) {
  return (
    <header className={styles.sectionHead}>
      {eyebrow && (
        <Reveal>
          <span className={styles.eyebrow}>{eyebrow}</span>
        </Reveal>
      )}
      <Reveal delay={60}>
        <h2 className={styles.sectionTitle}>{title}</h2>
      </Reveal>
      {children && (
        <Reveal delay={120}>
          <p className={styles.sectionLede}>{children}</p>
        </Reveal>
      )}
    </header>
  );
}

/* ------------------------------------------------------------------
   Hero
   ------------------------------------------------------------------ */
function Hero() {
  const ref = useRef(null);

  useEffect(() => {
    const el = ref.current;
    if (!el) return;
    let raf = 0;
    const onMove = (e) => {
      cancelAnimationFrame(raf);
      raf = requestAnimationFrame(() => {
        const r = el.getBoundingClientRect();
        const x = ((e.clientX - r.left) / r.width) * 100;
        const y = ((e.clientY - r.top) / r.height) * 100;
        el.style.setProperty("--x", `${x}%`);
        el.style.setProperty("--y", `${y}%`);
      });
    };
    el.addEventListener("mousemove", onMove);
    return () => {
      el.removeEventListener("mousemove", onMove);
      cancelAnimationFrame(raf);
    };
  }, []);

  return (
    <header ref={ref} className={styles.hero}>
      <div className={styles.heroAurora} aria-hidden="true" />
      <div className={styles.heroGlow} aria-hidden="true" />

      <div className={`${styles.heroInner} ${styles.heroEnter}`}>
        <div className={styles.heroMeta}>
          <span>Bleep — a build tool for Java, Kotlin &amp; Scala</span>
        </div>

        <h1 className={styles.heroTitle}>
          Everything got faster.<br />
          Except the <em>build</em>.
        </h1>

        <p className={styles.heroTagline}>
          Coding agents write at a thousand words a minute, fan out into
          six git worktrees, and then sit there, watching a build tool
          configure itself. We build five million lines of JVM code for
          a living and we got tired of waiting too. So we built a build
          tool with a native binary that bootstraps in ten milliseconds,
          answers an agent in two hundred tokens, and forks a warm copy
          of your entire compiled world in under a minute. This page is
          the sales pitch. Every number on it is measured.
        </p>

        <div className={styles.heroButtons}>
          <Link className={styles.btnPrimary} to="/docs/installing/">
            Install
          </Link>
          <Link className={styles.btnSecondary} to="#receipts">
            The receipts
          </Link>
          <Link className={styles.btnSecondary} to="/">
            The sober version
          </Link>
        </div>

        <div className={styles.heroFacts}>
          <div className={styles.heroFact}>
            <span className={styles.heroFactLabel}>CLI startup</span>
            <span className={styles.heroFactValue}>
              10 <em>ms</em>
            </span>
            <span className={styles.heroFactSub}>native binary, no JVM</span>
          </div>
          <div className={styles.heroFact}>
            <span className={styles.heroFactLabel}>
              new worktree, compiled
            </span>
            <span className={styles.heroFactValue}>
              268 <em>→</em> 54 s
            </span>
            <span className={styles.heroFactSub}>
              cold rebuild vs copy-state, 5.1M lines
            </span>
          </div>
          <div className={styles.heroFact}>
            <span className={styles.heroFactLabel}>tokens per answer</span>
            <span className={styles.heroFactValue}>
              ~<em>200</em>
            </span>
            <span className={styles.heroFactSub}>not a 30k-token log</span>
          </div>
          <div className={styles.heroFact}>
            <span className={styles.heroFactLabel}>build files</span>
            <span className={styles.heroFactValue}>
              <em>1</em>
            </span>
            <span className={styles.heroFactSub}>plain YAML, whole repo</span>
          </div>
        </div>
      </div>
    </header>
  );
}

/* ------------------------------------------------------------------
   The attention argument
   ------------------------------------------------------------------ */
function AttentionSection() {
  return (
    <section className={styles.section}>
      <div className={styles.container}>
        <SectionHeader
          eyebrow="Why speed is the whole game"
          title={
            <>
              A build minute costs more than a <em>minute</em>.
            </>
          }
        >
          You know this one from the inside: you hit compile, it takes
          forty seconds, you glance at Slack, and twenty minutes later
          you remember what you were doing. The research agrees with
          your gut.{" "}
          <Link to="https://chrisparnin.me/pdf/parnin-sqj11.pdf">
            Parnin studied ten thousand programming sessions
          </Link>
          : after an interruption, the median programmer needs 10&ndash;15
          minutes to make the next edit.{" "}
          <Link to="https://github.blog/engineering/infrastructure/experiment-the-hidden-costs-of-waiting-on-slow-build-times/">
            GitHub priced the waiting
          </Link>{" "}
          at roughly $400 of developer time per slow build. And the agent era
          sharpened it: code generation exploded while merged throughput
          didn&rsquo;t — validation is the bottleneck now, and the build
          sits in the middle of every validation.{" "}
          <Link to="https://cloud.google.com/blog/products/ai-machine-learning/announcing-the-2025-dora-report">
            DORA&rsquo;s verdict
          </Link>
          : AI multiplies the conditions you already have. Teams with
          fast feedback loops turned agents into 20&ndash;30% gains.
          Teams without got a faster way to wait.
        </SectionHeader>

        <Reveal>
          <p className={styles.pullQuote}>
            The build is the heartbeat of the loop.
            <br />
            It should be too fast to <em>notice</em>.
          </p>
        </Reveal>
      </div>
    </section>
  );
}

/* ------------------------------------------------------------------
   Field notes — the ecosystem's own words
   ------------------------------------------------------------------ */
function FieldNotesSection() {
  return (
    <section className={`${styles.section} ${styles.sectionPaper}`}>
      <div className={styles.container}>
        <SectionHeader
          eyebrow="Field notes"
          title={
            <>
              Everyone can <em>see</em> the problem.
            </>
          }
        >
          We didn&rsquo;t invent the pain. Here it is in the
          ecosystem&rsquo;s own words, links included — and what falls
          out when the build tool is designed for it instead of patched
          around.
        </SectionHeader>

        <Reveal>
          <div className={styles.mcpGrid}>
            <article className={styles.mcpCard}>
              <h3 className={styles.mcpCardTitle}>
                The daemon that logs itself to <em>death</em>
              </h3>
              <p className={styles.mcpCardBody}>
                An open Gradle issue, verbatim title:{" "}
                <Link to="https://github.com/gradle/gradle/issues/15621">
                  &ldquo;daemon hangs unrecoverably if it receives
                  console output too fast&rdquo;
                </Link>
                . A build daemon taken down by its own log volume.
                Bleep&rsquo;s daemon emits typed events; the transcript
                lands on disk as data, and the terminal gets a live
                summary, not a firehose.
              </p>
            </article>
            <article className={styles.mcpCard}>
              <h3 className={styles.mcpCardTitle}>
                Teams ship software to <em>hide</em> their build&rsquo;s
                output
              </h3>
              <p className={styles.mcpCardBody}>
                Agent-infra companies{" "}
                <Link to="https://www.humanlayer.dev/blog/context-efficient-backpressure">
                  build custom backpressure wrappers
                </Link>{" "}
                so Maven and Gradle logs (&ldquo;notoriously
                verbose&rdquo;, their words) stop shredding agent
                context. An entire product category exists to apologize
                for build output. Bleep answers in ~200 tokens because
                that&rsquo;s all it says.
              </p>
            </article>
            <article className={styles.mcpCard}>
              <h3 className={styles.mcpCardTitle}>
                MCP servers that <em>read about</em> your build
              </h3>
              <p className={styles.mcpCardBody}>
                Gradle&rsquo;s enterprise{" "}
                <Link to="https://develocity.ai/product/mcp-servers/">
                  Develocity MCP server
                </Link>{" "}
                is analytics over build scans — it tells agents about
                builds. Maven and Bazel get third-party wrappers that
                re-parse CLI text. Bleep <em>is</em> the MCP server:
                compile, test, run, diff. Built in, free, and it does
                the work.
              </p>
            </article>
          </div>
        </Reveal>
      </div>
    </section>
  );
}

/* ------------------------------------------------------------------
   Receipts — the numbers grid
   ------------------------------------------------------------------ */
function ReceiptsSection() {
  return (
    <section id="receipts" className={styles.section}>
      <div className={styles.container}>
        <SectionHeader
          eyebrow="Receipts"
          title={
            <>
              Numbers from a real repo, not a <em>benchmark</em> repo.
            </>
          }
        >
          Measured on the repo bleep&rsquo;s authors work in daily — 5.1
          million lines of JVM code across 130 projects — on the laptop
          it was typed on, against today&rsquo;s master. We can measure this any day,
          because the build{" "}
          <Link to="/docs/usage/run-history">records its own runs</Link>{" "}
          and diffs its own timing. A build tool that couldn&rsquo;t
          would have to ask you to trust it.
        </SectionHeader>

        <Reveal>
          <div className={styles.numbersGrid}>
            <div className={styles.numberCell}>
              <span className={styles.numberKicker}>CLI startup</span>
              <span className={styles.numberValue}>
                10<span className={styles.numberValueUnit}>ms</span>
              </span>
              <p className={styles.numberCaption}>
                Native binary. Build model loaded, dependencies resolved
                from cache — done before a JVM would have said hello.
              </p>
            </div>
            <div className={styles.numberCell}>
              <span className={styles.numberKicker}>
                &ldquo;is everything green?&rdquo;
              </span>
              <span className={styles.numberValue}>
                9<span className={styles.numberValueUnit}>s</span>
              </span>
              <p className={styles.numberCaption}>
                A no-op compile across all 130 projects, 5.1 million
                lines. That&rsquo;s the whole tax for asking.
              </p>
            </div>
            <div className={styles.numberCell}>
              <span className={styles.numberKicker}>fork → verified green</span>
              <span className={styles.numberValue}>
                54<span className={styles.numberValueUnit}>s</span>
              </span>
              <p className={styles.numberCaption}>
                The whole fork: <code>git worktree add</code>, then{" "}
                <code>bleep copy-state</code>, then a full compile
                verifying all 130 projects green. The same fork without
                copy-state compiles cold for 4&nbsp;min&nbsp;22&nbsp;s —
                five times longer. No fork pays cold twice.
              </p>
            </div>
            <div className={styles.numberCell}>
              <span className={styles.numberKicker}>tokens per agent answer</span>
              <span className={styles.numberValue}>
                ~200<span className={styles.numberValueUnit}>tok</span>
              </span>
              <p className={styles.numberCaption}>
                Counts, first errors, a <code>historyId</code>. The full
                transcript is one regex-searchable call away.
              </p>
            </div>
          </div>
        </Reveal>

        <Reveal delay={120}>
          <p className={styles.compareCta}>
            <Link className={styles.compareCtaLink} to="/docs/guides/worktrees">
              The worktree benchmark, step by step &nbsp;&rarr;
            </Link>
          </p>
        </Reveal>
      </div>
    </section>
  );
}

/* ------------------------------------------------------------------
   Per-language maturity — "is MY stack first-class?"
   ------------------------------------------------------------------ */
function MaturitySection() {
  return (
    <section className={`${styles.section} ${styles.sectionPaper}`}>
      <div className={styles.container}>
        <SectionHeader
          eyebrow="Is your stack in?"
          title={
            <>
              Java, Kotlin, Scala — <em>first-class</em>.
            </>
          }
        >
          The question right after the numbers: is <em>my</em> thing
          supported? Compile, test, run, publish, IDE import, cross-build,
          scripts and sourcegen are first-class for all three languages.
          The details, honestly labeled:
        </SectionHeader>

        <Reveal>
          <div className={styles.mcpGrid}>
            <article className={styles.mcpCard}>
              <h3 className={styles.mcpCardTitle}>Java</h3>
              <p className={styles.mcpCardBody}>
                <strong>First-class</strong>: javac and ECJ, annotation
                processors (Lombok, MapStruct, Dagger, Immutables, …),
                Spring Boot via{" "}
                <Link to="/docs/spring-boot-proves-the-model/">
                  bleep-plugin-spring-boot
                </Link>
                , Maven import.
                <br />
                <strong>Partial</strong>: BOM /{" "}
                <code>dependencyManagement</code> not yet, every
                dependency declares its own version explicitly.
                <br />
                <strong>Not in scope</strong>: Android.
              </p>
            </article>

            <article className={styles.mcpCard}>
              <h3 className={styles.mcpCardTitle}>Kotlin</h3>
              <p className={styles.mcpCardBody}>
                <strong>First-class</strong>: kotlinc 2.x, compiler
                plugins (<code>allopen</code>, <code>jpa</code>,{" "}
                <code>spring</code>, <code>noarg</code>,{" "}
                <code>serialization</code>),{" "}
                <Link to="/docs/usage/annotation-processing#kotlin-ksp">
                  KSP processors
                </Link>{" "}
                (Room, Hilt, Moshi codegen, Koin KSP, kotlin-inject, …),
                Kotlin/JS via <code>cross:</code>.
                <br />
                <strong>Partial</strong>: KSP runs from scratch each
                compile; Kotlin/Native targets exist but the ecosystem
                expects Gradle.
                <br />
                <strong>Not in scope</strong>: KAPT (migrate to KSP),
                Android, Gradle import.
              </p>
            </article>

            <article className={styles.mcpCard}>
              <h3 className={styles.mcpCardTitle}>Scala</h3>
              <p className={styles.mcpCardBody}>
                <strong>First-class</strong>: Scala 2.13 + Scala 3
                cross-builds, Scala.js, Scala Native, scalafmt,
                scalafix, Zinc incremental, sbt import, ports of
                sbt-ci-release / sbt-sonatype / sbt-pgp / sbt-dynver /
                sbt-native-image / mdoc.
                <br />
                <strong>Partial</strong>:{" "}
                <code>projectMatrix</code>-style third axes beyond
                JVM/JS/Native × 2.13/3.
                <br />
                <strong>Not in scope</strong>: publishing as an sbt
                plugin artifact (consuming sbt plugins works).
              </p>
            </article>
          </div>
        </Reveal>

        <Reveal delay={120}>
          <p className={styles.compareCta}>
            <Link className={styles.compareCtaLink} to="/docs/appendix/status/">
              Full project status &amp; what&rsquo;s not yet covered &nbsp;&rarr;
            </Link>
          </p>
        </Reveal>
      </div>
    </section>
  );
}

/* ------------------------------------------------------------------
   Simplicity
   ------------------------------------------------------------------ */
function SimplicitySection() {
  return (
    <section className={styles.section}>
      <div className={styles.container}>
        <SectionHeader
          eyebrow="How it can be this fast"
          title={
            <>
              Simplicity is the <em>weapon</em>.
            </>
          }
        >
          Your whole build is one YAML file. Not a program that
          eventually produces a build — the build itself, as data.
          Everything a build plugin ever did is either source generation
          or a program you run. That single decision is where all the
          speed comes from: nothing to configure, nothing to interpret,
          nothing to warm up, nothing that can hide.
        </SectionHeader>

        <Reveal>
          <ul
            className={styles.heroNoList}
            style={{ justifyContent: "center", marginTop: "0.5rem" }}
          >
            <li>code in the build</li>
            <li>build plugins</li>
            <li>project scopes</li>
            <li>task graph</li>
            <li>DSL</li>
            <li>configuration phase</li>
          </ul>
        </Reveal>

        <Reveal delay={80}>
          <div className={styles.specimenFrame} style={{ marginTop: "2.5rem" }}>
            <div className={styles.specimenHead}>
              <span className={styles.specimenHeadAside}>
                a complete, working build — from the docs&rsquo; own
                integration tests
              </span>
            </div>
            <div className={styles.specimenSnippet}>
              <Snippet
                path="docs-snippets-from-tests/your-first-kotlin-project/bleep.yaml"
                lang="yaml"
              />
            </div>
          </div>
        </Reveal>

        <Reveal delay={100}>
          <div className={styles.dossierGrid} style={{ marginTop: "2.5rem" }}>
            <article className={`${styles.dossierCard} ${styles.dossierCardTenet}`}>
              <div className={styles.dossierHead}>
                <span className={styles.dossierKicker}>
                  <span className={styles.dossierDot} />
                  <span>Tenet</span>
                </span>
                <span className={styles.dossierNum}>1</span>
              </div>
              <h3 className={styles.dossierTitle}>
                The build is <em>data</em>.
              </h3>
              <p className={styles.dossierBody}>
                Readable top to bottom by anyone on the team, and by any
                agent in one pass. Grep it, diff it, rewrite it with
                tooling. A build you can read is a build you can trust.
              </p>
              <div className={styles.dossierAccent} aria-hidden="true" />
            </article>

            <article className={`${styles.dossierCard} ${styles.dossierCardTenet}`}>
              <div className={styles.dossierHead}>
                <span className={styles.dossierKicker}>
                  <span className={styles.dossierDot} />
                  <span>Tenet</span>
                </span>
                <span className={styles.dossierNum}>2</span>
              </div>
              <h3 className={styles.dossierTitle}>
                Everything else is <em>code</em>.
              </h3>
              <p className={styles.dossierBody}>
                Docker images, doc generation, release orchestration:
                programs in your repo, in your language, debuggable in
                your IDE. Not plugins in someone else&rsquo;s lifecycle,
                debugged with <code>println</code>.
              </p>
              <div className={styles.dossierAccent} aria-hidden="true" />
            </article>

            <article className={`${styles.dossierCard} ${styles.dossierCardTenet}`}>
              <div className={styles.dossierHead}>
                <span className={styles.dossierKicker}>
                  <span className={styles.dossierDot} />
                  <span>Tenet</span>
                </span>
                <span className={styles.dossierNum}>3</span>
              </div>
              <h3 className={styles.dossierTitle}>
                Fail <em>loudly</em>.
              </h3>
              <p className={styles.dossierBody}>
                No graceful degradation, no silently stale outputs, no
                &ldquo;it probably worked&rdquo;. If something is wrong,
                bleep throws. Humans appreciate this. Agents{" "}
                <em>depend</em> on it — a tool that degrades quietly
                sends them chasing ghosts.
              </p>
              <div className={styles.dossierAccent} aria-hidden="true" />
            </article>
          </div>
        </Reveal>

        <Reveal delay={140}>
          <p
            className={styles.sectionLede}
            style={{ marginTop: "2.25rem", textAlign: "center" }}
          >
            &ldquo;But my build plugin!&rdquo; We checked. We analyzed
            each of the{" "}
            <Link to="/docs/compared-to-other-build-tools/maven-plugin-coverage/">
              top 50 Maven plugins
            </Link>
            , implemented the hardest case (
            <Link to="/docs/spring-boot-proves-the-model/">
              Spring Boot
            </Link>
            ), and ship{" "}
            <Link to="/docs/appendix/status/">
              codebases of millions of lines
            </Link>{" "}
            on this model.
          </p>
        </Reveal>
      </div>
    </section>
  );
}

/* ------------------------------------------------------------------
   The human loop — IDE + test runner DX
   ------------------------------------------------------------------ */
function HumansSection() {
  return (
    <section className={`${styles.section} ${styles.sectionPaper}`}>
      <div className={styles.container}>
        <SectionHeader
          eyebrow="Also: you"
          title={
            <>
              The same speed in your <em>IDE</em> and terminal.
            </>
          }
        >
          Agents run headless. You don&rsquo;t. Open the project and the
          IDE import takes a second or two, because bleep reads{" "}
          <code>bleep.yaml</code> and hands the model straight over BSP —
          no configuration phase, no plugin resolution, no progress bar
          with your afternoon on it. Switch to a branch with a different
          Kotlin version and the reload is milliseconds. And when you
          run tests, you watch them run.
        </SectionHeader>

        <Reveal>
          <div className={styles.testRunnerVideo}>
            <video
              src="https://github.com/user-attachments/assets/06ba4fa0-2ab0-4199-ac24-3806d6c80206"
              controls
              loop
              muted
              playsInline
              preload="metadata"
              aria-label="bleep test runner showing parallel execution and live progress"
            />
          </div>
        </Reveal>

        <Reveal delay={100}>
          <div className={styles.mcpGrid}>
            <article className={styles.mcpCard}>
              <h3 className={styles.mcpCardTitle}>
                IDE imports in <em>seconds</em>
              </h3>
              <p className={styles.mcpCardBody}>
                First import into IntelliJ or any BSP editor: a second
                or two. Branch reload: milliseconds. In Gradle or sbt the same actions cost a
                configuration phase, plugin loading, and an IDE model
                rebuild — minutes, on real projects, several times a
                day.
              </p>
            </article>
            <article className={styles.mcpCard}>
              <h3 className={styles.mcpCardTitle}>
                Tests run <em>everywhere</em> at once
              </h3>
              <p className={styles.mcpCardBody}>
                Suites compile and run in parallel across every CPU, in
                forked JVMs with their own classpaths. The terminal
                stays live: which suites are compiling, which are
                running, which failed — the moment it happens, not at
                the end.
              </p>
            </article>
            <article className={styles.mcpCard}>
              <h3 className={styles.mcpCardTitle}>
                A summary you can <em>act on</em>
              </h3>
              <p className={styles.mcpCardBody}>
                Exact suite and test names, pass/fail counts per
                project, and <code>bleep test --diff</code> prints only
                what changed since the previous run. JUnit XML is one
                flag away for CI.
              </p>
            </article>
          </div>
        </Reveal>
      </div>
    </section>
  );
}

/* ------------------------------------------------------------------
   Huge projects
   ------------------------------------------------------------------ */
function ScaleSection() {
  return (
    <section className={styles.section}>
      <div className={styles.container}>
        <SectionHeader
          eyebrow="Where it matters most"
          title={
            <>
              Written for the <em>biggest</em> repos out there.
            </>
          }
        >
          Big repos are where the important software lives — systems
          with twenty years of history and no appetite for adventure —
          and where build pain compounds hardest: every wart multiplied
          by a thousand modules and a hundred engineers. The
          industrial-strength alternative has a price tag on the record:{" "}
          <Link to="https://news.ycombinator.com/item?id=41975870">
            &ldquo;Rolling out Bazel at my prior employer took about
            one person decade of engineering time&rdquo;
          </Link>
          . Bleep is designed at scale and lives at scale, daily — with
          no dedicated build team required.
        </SectionHeader>

        <Reveal>
          <div className={styles.mcpGrid}>
            <article className={styles.mcpCard}>
              <h3 className={styles.mcpCardTitle}>
                One <em>hot daemon</em> per machine
              </h3>
              <p className={styles.mcpCardBody}>
                Every checkout, every worktree, every IDE window and
                agent shares one compile server that keeps incremental
                state hot and stores identical dependency analyses
                exactly once. Ten worktrees don&rsquo;t cost ten JVMs.
              </p>
            </article>
            <article className={styles.mcpCard}>
              <h3 className={styles.mcpCardTitle}>
                CI that <em>skips</em>
              </h3>
              <p className={styles.mcpCardBody}>
                <code>bleep build invalidated</code> names exactly the
                projects a diff touched — both build loads are instant,
                because the build is data. The{" "}
                <Link to="/docs/usage/remote-cache">remote cache</Link>{" "}
                pulls everything a previous run already compiled. You
                pay to build the change, not the repo.
              </p>
            </article>
            <article className={styles.mcpCard}>
              <h3 className={styles.mcpCardTitle}>
                Migration is <em>one command</em>
              </h3>
              <p className={styles.mcpCardBody}>
                <code>bleep import</code> reads an sbt build,{" "}
                <code>bleep import-maven</code> a Maven one, and writes
                the equivalent <code>bleep.yaml</code> — project graph
                derived, templates inferred. Compiling and testing on
                day one, even at hundreds of modules.
              </p>
            </article>
          </div>
        </Reveal>
      </div>
    </section>
  );
}

/* ------------------------------------------------------------------
   The agent era
   ------------------------------------------------------------------ */
function AgentEraSection() {
  return (
    <section className={`${styles.section} ${styles.sectionPaper}`}>
      <div className={styles.container}>
        <SectionHeader
          eyebrow="The agent era"
          title={
            <>
              Your agents&rsquo; favorite <em>build tool</em>.
            </>
          }
        >
          Agentic development has a shape: an orchestrator fans
          subagents into git worktrees, each hammering compile-test
          loops in parallel. Bleep ships the whole apparatus for that
          shape, built in. One MCP registration covers every checkout
          you&rsquo;ll ever create. Every call names its workspace, so a
          subagent can&rsquo;t silently build the wrong checkout. New
          worktrees clone the parent&rsquo;s compiled state instead of
          rebuilding the world. And every run becomes a transcript the
          agent can query — or <em>diff</em>.
        </SectionHeader>

        <Reveal>
          <p className={styles.mcpStat}>
            <span className={styles.mcpStatFigure}>build runs you can diff</span>
            <span className={styles.mcpStatRest}>
              &ldquo;what changed since the last green run?&rdquo; is one
              call, answered as data. No other build tool has this shape —
              the nearest attempt, Develocity&rsquo;s build-scan comparison,
              is paid analytics in someone else&rsquo;s cloud.
            </span>
          </p>
        </Reveal>

        <Reveal delay={60}>
          <Vignette
            rows={[
              {
                actor: "agent",
                deed: "breaks a test, reruns — diffBase pins the last green run, so the answer rides along",
              },
              {
                actor: "",
                call: "bleep.test { directory: ~/repo/wt/api, diffBase: 18 }",
                result:
                  '{ historyId: 19, failed: 1, diff: { summary: "1 newlyFailing", newlyFailing: [{ test: "PricingTest.10 percent off at 100 and above", from: "passed", to: "failed", message: "expected 216, obtained 204" }] } }',
                bad: true,
                note: "run + what-changed in one call: the break, name and assertion, nothing else",
              },
              { gap: true },
              { actor: "agent", deed: "reverts, reruns" },
              {
                actor: "",
                call: "bleep.test { directory: ~/repo/wt/api, diffBase: 18 }",
                result:
                  '{ historyId: 20, passed: 14, diff: { identical: true, summary: "No logical differences." } }',
                good: true,
                note: "~800ms of timing noise between the runs, zero false diffs",
              },
            ]}
          />
        </Reveal>

        <Reveal delay={80}>
          <div className={styles.mcpGrid}>
            <article className={styles.mcpCard}>
              <h3 className={styles.mcpCardTitle}>
                Answers, not <em>logs</em>
              </h3>
              <p className={styles.mcpCardBody}>
                Compile and test return counts, the first errors, and a{" "}
                <code>historyId</code> — a couple hundred tokens.
                Failures stream the moment they happen. The full
                transcript is regex-searchable server-side, so no agent
                ever pipes a build log to a file and greps past the
                answer again.
              </p>
            </article>
            <article className={styles.mcpCard}>
              <h3 className={styles.mcpCardTitle}>
                Forks start <em>warm</em>
              </h3>
              <p className={styles.mcpCardBody}>
                <code>bleep.copy-state</code> clones the parent
                worktree&rsquo;s compiled state under the compile
                server&rsquo;s own locks — clonefile-fast, safe
                mid-compile. Fork to verified green in 54 seconds on
                5.1 million lines. Subagents earn their keep from the
                first minute.
              </p>
            </article>
            <article className={styles.mcpCard}>
              <h3 className={styles.mcpCardTitle}>
                The loop is <em>one call</em>
              </h3>
              <p className={styles.mcpCardBody}>
                Pass <code>diffBase</code> to compile or test and the
                response carries what your edit changed: newly failing,
                fixed, new diagnostics — nothing else. Timing jitter is
                filtered by construction, so the diff never cries wolf.
                Humans get the same loop as{" "}
                <code>bleep test --diff</code>.
              </p>
            </article>
          </div>
        </Reveal>

        <Reveal delay={100}>
          <p className={styles.toolRoster}>
            <span className={styles.toolRosterLabel}>
              the whole surface, 18 tools —{" "}
            </span>
            <Link to="/docs/usage/mcp-server/">
              bleep.compile · test · run · projects · test.suites ·
              build.effective/resolved · history.list/show/diff/diff-timing ·
              copy-state · fmt · clean · sourcegen · scripts · programs ·
              restart
            </Link>
          </p>
        </Reveal>

        <Reveal delay={120}>
          <p
            className={styles.sectionLede}
            style={{ marginTop: "2.25rem", textAlign: "center" }}
          >
            Bleep is developed by sessions of parallel agents in git
            worktrees, building bleep with bleep. Everything above is
            how this tool gets maintained, daily.
          </p>
        </Reveal>
      </div>
    </section>
  );
}

/* ------------------------------------------------------------------
   The part where we admit things
   ------------------------------------------------------------------ */
function HonestySection() {
  return (
    <section className={styles.section}>
      <div className={styles.containerNarrow}>
        <SectionHeader
          eyebrow="The part where we admit things"
          title={
            <>
              What bleep is <em>not</em>.
            </>
          }
        >
          Marketing pages that admit nothing are asking you to do the
          discounting yourself. Save the trip: bleep is for Java,
          Kotlin, and Scala on the JVM (with JS and Native
          cross-builds). No Android. No Gradle import yet — Maven and
          sbt import exist, Gradle is a hand-port today. BOM support
          is on the list, not in the tool. The{" "}
          <Link to="/docs/appendix/status/">status page</Link> keeps the
          full account. If your dealbreaker is on it, we&rsquo;d rather
          you find out here than three days in — and if it isn&rsquo;t,
          everything above is waiting. One more, in reverse: if bleep
          ever stops being right for you, your whole build is portable
          YAML and Maven coordinates — leaving is a mechanical
          translation, and we wrote the{" "}
          <Link to="/docs/guides/exit-strategy">exit strategy</Link>{" "}
          down. Betting on a young tool should never mean betting the
          repo.
        </SectionHeader>
      </div>
    </section>
  );
}

/* ------------------------------------------------------------------
   CTA
   ------------------------------------------------------------------ */
function InstallCTA() {
  const [copied, setCopied] = useState(false);
  const [copiedMcp, setCopiedMcp] = useState(false);
  const installCmd = "curl -fsSL https://bleep.build/install | sh";
  const mcpCmd = "claude mcp add --scope user bleep -- bleep mcp-server";

  const onCopy = () => {
    navigator.clipboard?.writeText(installCmd);
    setCopied(true);
    setTimeout(() => setCopied(false), 1600);
  };
  const onCopyMcp = () => {
    navigator.clipboard?.writeText(mcpCmd);
    setCopiedMcp(true);
    setTimeout(() => setCopiedMcp(false), 1600);
  };

  return (
    <section className={styles.cta}>
      <div className={styles.container}>
        <SectionHeader
          eyebrow="The close"
          title={
            <>
              Two lines. Then go <em>build</em> something huge.
            </>
          }
        >
          Open source, Apache 2.0. The first line installs the binary.
          The second gives every agent on your machine a build tool that
          answers in two hundred tokens. There is no third line.
        </SectionHeader>

        <Reveal>
          <div className={styles.installStack}>
            <div className={styles.installFrame}>
              <span className={styles.installPrompt}>$</span>
              <span className={styles.installCmd}>{installCmd}</span>
              <button
                type="button"
                onClick={onCopy}
                className={`${styles.installCopy} ${copied ? styles.copied : ""}`}
              >
                {copied ? "Copied" : "Copy"}
              </button>
            </div>
            <div className={styles.installFrame}>
              <span className={styles.installPrompt}>$</span>
              <span className={styles.installCmd}>{mcpCmd}</span>
              <button
                type="button"
                onClick={onCopyMcp}
                className={`${styles.installCopy} ${copiedMcp ? styles.copied : ""}`}
              >
                {copiedMcp ? "Copied" : "Copy"}
              </button>
            </div>
          </div>
        </Reveal>

        <Reveal delay={120}>
          <div className={styles.ctaButtons}>
            <Link className={styles.btnPrimary} to="/docs/installing/">
              Install guide
            </Link>
            <Link className={styles.btnSecondary} to="/docs/tutorials/your-first-project/">
              First project
            </Link>
            <Link className={styles.btnSecondary} to="https://github.com/oyvindberg/bleep">
              GitHub
            </Link>
          </div>
        </Reveal>
      </div>
    </section>
  );
}

/* ------------------------------------------------------------------
   Page
   ------------------------------------------------------------------ */
export default function Why() {
  return (
    <Layout
      title="Why bleep"
      description="The case for bleep at full volume: ten-millisecond startup, one-file builds, 200-token agent answers, and warm worktree forks on five-million-line repos. Every number measured."
    >
      <div className={styles.page}>
        <Hero />
        <main>
          <AttentionSection />
          <FieldNotesSection />
          <ReceiptsSection />
          <MaturitySection />
          <SimplicitySection />
          <HumansSection />
          <ScaleSection />
          <AgentEraSection />
          <HonestySection />
          <InstallCTA />
        </main>
      </div>
    </Layout>
  );
}
