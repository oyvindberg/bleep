import React, { useEffect, useRef, useState } from "react";
import Link from "@docusaurus/Link";
import Layout from "@theme/Layout";
import Snippet from "@site/src/components/Snippet";
import { AsciinemaPlayer } from "@site/src/components/AsciinemaPlayer";
import claudeAgentsCast from "!!file-loader!@site/static/demos/claude-agents.cast";
import ownTestsCast from "!!file-loader!@site/static/demos/own-tests.cast";
import stats from "@site/src/data/build-stats.json";
import styles from "./index.module.css";

/* ------------------------------------------------------------------
   Vignette — an annotated agent-session transcript
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
        ),
      )}
    </div>
  );
}

/* ------------------------------------------------------------------
   Reveal, scroll-triggered fade + rise
   ------------------------------------------------------------------ */
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
      { threshold: 0.12, rootMargin: "0px 0px -8% 0px" },
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
   Hero, cursor-tracked glow, drift aurora, staggered entrance
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
          <span>A build tool for Java, Kotlin &amp; Scala</span>
        </div>

        <h1 className={styles.heroTitle}>
          Compile. Test. Sourcegen.
          <br />
          That&rsquo;s the <em>build</em>. Everything else is code.
        </h1>

        <p className={styles.heroTagline}>
          Two decades of Maven, Gradle, and sbt is a long time to watch build
          tools grow incredibly complex. We built one that won&rsquo;t. Bleep
          does precisely what a build is for: compile, test, sourcegen, then
          package, link, publish what comes out. It refuses the rest. Your
          container build is code you write. So is your doc generation, your
          sidecar boot, your CI orchestration. All of it. And it&rsquo;s the
          build tool your agents have been asking for.
        </p>

        <div className={styles.heroButtons}>
          <Link className={styles.btnPrimary} to="/docs/installing/">
            Install
          </Link>
          <Link
            className={styles.btnSecondary}
            to="/docs/tutorials/your-first-project/"
          >
            Your first project
          </Link>
          <Link className={styles.btnSecondary} to="/#agents">
            Bleep for agents
          </Link>
          <Link
            className={styles.btnSecondary}
            to="https://github.com/oyvindberg/bleep"
          >
            GitHub
          </Link>
        </div>

        <div className={styles.heroFacts}>
          <div className={styles.heroFact}>
            <span className={styles.heroFactLabel}>CLI startup</span>
            <span className={styles.heroFactValue}>
              {stats.timings.cliStartupMs} <em>ms</em>
            </span>
            <span className={styles.heroFactSub}>native binary, no JVM</span>
          </div>
          <div className={styles.heroFact}>
            <span className={styles.heroFactLabel}>new worktree, compiled</span>
            <span className={styles.heroFactValue}>
              {stats.timings.forkToVerifiedGreenColdSeconds} <em>&rarr;</em>{" "}
              {stats.timings.forkToVerifiedGreenSeconds} s
            </span>
            <span className={styles.heroFactSub}>
              cold rebuild vs copy-state, {stats.compiled.linesShort} lines
            </span>
          </div>
          <div className={styles.heroFact}>
            <span className={styles.heroFactLabel}>tokens per answer</span>
            <span className={styles.heroFactValue}>
              {stats.tokens.greenCompile}
              <em>&ndash;</em>
              {stats.tokens.failingTestWithDiff}
            </span>
            <span className={styles.heroFactSub}>
              green &rarr; failure with full diff
            </span>
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
   Refusals, the things we will not have
   ------------------------------------------------------------------ */
const refusals = [
  {
    title: (
      <>
        No <em>code</em> in the build file.
      </>
    ),
    body: (
      <>
        A build file describes a project. It doesn&rsquo;t run one.{" "}
        <code>bleep.yaml</code> is data: readable top to bottom by anyone on the
        team. Logic lives in your code, in your repo, where you can{" "}
        <code>git blame</code> it.
      </>
    ),
  },
  {
    title: (
      <>
        No <em>build plugins</em>.
      </>
    ),
    body: (
      <>
        No autoplugins, no <code>requires</code> graphs, no{" "}
        <code>Plugin&lt;Project&gt;</code> registration. Bleep doesn&rsquo;t
        have one. Code goes in your repo, where you can read it.
      </>
    ),
  },
  {
    title: (
      <>
        No <em>project scopes</em>.
      </>
    ),
    body: (
      <>
        A test project is a project. A scripts project (your build code) is a
        project. Your production app is a project. Same fields, same dependency
        model, same <code>bleep compile</code> and <code>bleep test</code>, no
        second category. No <code>Test/test/itTest/Compile</code> scope dance
        grafted onto the project graph. A project is a project is a project.
      </>
    ),
  },
  {
    title: (
      <>
        No <em>task graph</em>.
      </>
    ),
    body: (
      <>
        There&rsquo;s no user-definable task DAG. The build does compile, test,
        sourcegen. Everything else is a script: a <code>main</code> class you
        call when you want to. Composable like programs, debuggable like
        programs, no special layer between you and the JVM.
      </>
    ),
  },
];

function RefusalsSection() {
  return (
    <section className={styles.section}>
      <div className={styles.container}>
        <SectionHeader
          eyebrow="The simplification"
          title={
            <>
              Four things we <em>cut</em>.
            </>
          }
        >
          A project is a project is a project. Code is code is code. Everything
          explicit, everything simple. Here&rsquo;s how we got there.
        </SectionHeader>

        <div className={styles.dossierGrid}>
          {refusals.map((r, i) => (
            <Reveal key={i} delay={(i % 4) * 60}>
              <article
                className={`${styles.dossierCard} ${styles.dossierCardRefuse}`}
              >
                <div className={styles.dossierHead}>
                  <span className={styles.dossierKicker}>
                    <span className={styles.dossierDot} />
                    <span>Simplification</span>
                  </span>
                </div>
                <h3 className={styles.dossierTitle}>{r.title}</h3>
                <p className={styles.dossierBody}>{r.body}</p>
                <div className={styles.dossierAccent} aria-hidden="true" />
              </article>
            </Reveal>
          ))}
        </div>
      </div>
    </section>
  );
}

/* ------------------------------------------------------------------
   Specimen, real bleep.yaml, loaded from the integration-test snippets
   we ship to the docs site. Source of truth lives in:
   docs-snippets-from-tests/your-first-kotlin-project/bleep.yaml
   ------------------------------------------------------------------ */
const specimenLangs = [
  {
    id: "kotlin",
    label: "Kotlin",
    fixture: "your-first-kotlin-project",
    competitor: <code>build.gradle.kts</code>,
    bumpLabel: "Bump Kotlin in one line.",
    bumpFromTo: (
      <>
        Move <code>2.3.0</code> to <code>2.4.0</code> in{" "}
        <code>template-common</code>
      </>
    ),
  },
  {
    id: "java",
    label: "Java",
    fixture: "your-first-project",
    competitor: <code>pom.xml</code>,
    bumpLabel: "Bump JUnit in one line.",
    bumpFromTo: (
      <>
        Move <code>5.10.1</code> to <code>5.11.0</code> on{" "}
        <code>myapp-test</code>
      </>
    ),
  },
  {
    id: "scala",
    label: "Scala",
    fixture: "your-first-scala-project",
    competitor: <code>build.sbt</code>,
    bumpLabel: "Bump Scala in one line.",
    bumpFromTo: (
      <>
        Move <code>3.8.3</code> to <code>3.9.0</code> in{" "}
        <code>template-common</code>
      </>
    ),
  },
];

function SpecimenSection() {
  const [active, setActive] = useState("kotlin");
  const lang = specimenLangs.find((l) => l.id === active) || specimenLangs[0];
  const fixturePath = `docs-snippets-from-tests/${lang.fixture}/bleep.yaml`;

  return (
    <section className={`${styles.section} ${styles.sectionPaper}`}>
      <div className={styles.container}>
        <SectionHeader
          eyebrow="Build-as-data"
          title={
            <>
              The build is <em>data</em>, not a program.
            </>
          }
        >
          A real <code>bleep.yaml</code>. Not pseudocode. Not a marketing
          render. Plain YAML you can read, grep, diff, and rewrite. The same
          model bleep itself uses. And because it&rsquo;s data, your agent can
          read it in one pass, edit it safely, and verify the result — try that
          with a build script. The YAML is just the first layer: the same
          discipline runs through the whole tool, all the way to{" "}
          <Link to="/#agents">build runs you can diff</Link>.
        </SectionHeader>

        <Reveal>
          <div className={styles.specimenFrame}>
            <div className={styles.specimenHead}>
              <div
                className={styles.specimenTabs}
                role="tablist"
                aria-label="Choose specimen language"
              >
                {specimenLangs.map((l) => (
                  <button
                    key={l.id}
                    type="button"
                    role="tab"
                    aria-selected={l.id === active}
                    className={`${styles.specimenTab} ${l.id === active ? styles.specimenTabActive : ""}`}
                    onClick={() => setActive(l.id)}
                  >
                    {l.label}
                  </button>
                ))}
              </div>
              <span className={styles.specimenHeadAside}>{lang.fixture}</span>
            </div>
            <div className={styles.specimenSnippet}>
              <Snippet path={fixturePath} lang="yaml" />
            </div>
          </div>
        </Reveal>
      </div>
    </section>
  );
}

/* ------------------------------------------------------------------
   Per-language maturity. Three short columns under the specimen so a
   reader can answer "is the thing I need first-class?" without
   drilling into the docs.
   ------------------------------------------------------------------ */
function MaturitySection() {
  return (
    <section className={styles.section}>
      <div className={styles.container}>
        <SectionHeader
          eyebrow="What's first-class today"
          title={
            <>
              Per-language <em>maturity</em>.
            </>
          }
        >
          What works, what's partial, what's not in scope yet. Compile, test,
          run, publish, BSP, cross-build, scripts and sourcegen are first-class
          for every language.
        </SectionHeader>

        <Reveal>
          <div className={styles.mcpGrid}>
            <article className={styles.mcpCard}>
              <h3 className={styles.mcpCardTitle}>Java</h3>
              <p className={styles.mcpCardBody}>
                <strong>First-class</strong>: javac and ECJ, annotation
                processors (Lombok, MapStruct, Dagger, Immutables, …), Spring
                Boot via{" "}
                <Link to="/docs/spring-boot-proves-the-model/">
                  bleep-plugin-spring-boot
                </Link>
                , Maven import.
                <br />
                <strong>Partial</strong>: BOM /{" "}
                <code>dependencyManagement</code> not yet, every dependency
                declares its own version explicitly.
                <br />
                <strong>Not in scope</strong>: Android.
              </p>
            </article>

            <article className={styles.mcpCard}>
              <h3 className={styles.mcpCardTitle}>Kotlin</h3>
              <p className={styles.mcpCardBody}>
                <strong>First-class</strong>: kotlinc 2.x, compiler plugins (
                <code>allopen</code>, <code>jpa</code>, <code>spring</code>,{" "}
                <code>noarg</code>, <code>serialization</code>),{" "}
                <Link to="/docs/usage/annotation-processing#kotlin-ksp">
                  KSP processors
                </Link>{" "}
                (Room, Hilt, Moshi codegen, Koin KSP, kotlinx.serialization KSP
                variant, kotlin-inject, …), Kotlin/JS via <code>cross:</code>.
                <br />
                <strong>Partial</strong>: KSP runs from scratch each compile,
                per-file change tracking is a planned follow-up. Kotlin/Native
                targets exist but the ecosystem expects Gradle. No Gradle import
                yet — hand-port today.
                <br />
                <strong>Not in scope</strong>: KAPT (migrate to KSP), Android.
              </p>
            </article>

            <article className={styles.mcpCard}>
              <h3 className={styles.mcpCardTitle}>Scala</h3>
              <p className={styles.mcpCardBody}>
                <strong>First-class</strong>: Scala 2.13 + Scala 3 cross-builds,
                Scala.js, Scala Native, scalafmt, scalafix, Zinc incremental,
                sbt import, ports of sbt-ci-release / sbt-sonatype / sbt-pgp /
                sbt-dynver / sbt-native-image / mdoc.
                <br />
                <strong>Partial</strong>: <code>projectMatrix</code>-style third
                axes, per-minor-Scala-version overrides; the cross-build model
                covers JVM × {"{"}2.13, 3{"}"} × {"{"}JVM, JS, Native{"}"} but
                not arbitrary fourth dimensions.
                <br />
                <strong>Not in scope</strong>: publishing as an sbt plugin
                artifact (consuming sbt plugins works).
              </p>
            </article>
          </div>
        </Reveal>

        <Reveal delay={120}>
          <p className={styles.compareCta}>
            <Link className={styles.compareCtaLink} to="/docs/appendix/status/">
              Full project status &amp; what's not yet covered &nbsp;&rarr;
            </Link>
          </p>
        </Reveal>
      </div>
    </section>
  );
}

/* ------------------------------------------------------------------
   Performance, inner loop. Branch switch, incremental compile, read-only.
   ------------------------------------------------------------------ */
function PerformanceSection() {
  return (
    <section className={styles.section}>
      <div className={styles.container}>
        <SectionHeader
          eyebrow="The payoff"
          title={
            <>
              Built for the <em>inner loop</em>.
            </>
          }
        >
          Cut the code, the build plugins, the scopes, the task graph, and the
          inner loop stops being something you wait for. The numbers below are
          measured on the repo bleep&rsquo;s authors work in daily &mdash;{" "}
          {stats.compiled.java.linesLabel} lines of Java and{" "}
          {stats.compiled.scala.linesLabel} of Scala across{" "}
          {stats.compiled.projects} projects &mdash; and we can re-measure them
          any day, because the build{" "}
          <Link to="/docs/usage/run-history">records its own runs</Link>. It
          matters more than ever, too:{" "}
          <Link to="https://cloud.google.com/blog/products/ai-machine-learning/announcing-the-2025-dora-report">
            DORA&rsquo;s 2025 report
          </Link>{" "}
          found AI multiplies whatever loop you already have. Fast feedback
          turned agents into real gains. Slow feedback got a faster way to wait.
        </SectionHeader>

        <Reveal>
          <div className={styles.mcpGrid}>
            <article className={styles.mcpCard}>
              <h3 className={styles.mcpCardTitle}>
                Load <em>everything</em> in 10 ms
              </h3>
              <p className={styles.mcpCardBody}>
                Native CLI binary. Reads <code>bleep.yaml</code>, resolves
                dependencies through Coursier&rsquo;s local cache, builds the
                full project model &mdash; all {stats.compiled.projects}{" "}
                projects, done in ten milliseconds, before a JVM would have
                finished saying hello. No configuration phase, no &ldquo;loading
                projects&hellip;&rdquo; progress bar. The compile daemon (
                <code>bleep-bsp</code>) is the JVM-heavy bit, and it stays hot
                between invocations.
              </p>
            </article>
            <article className={styles.mcpCard}>
              <h3 className={styles.mcpCardTitle}>
                IDE imports &amp; <em>reloads</em>
              </h3>
              <p className={styles.mcpCardBody}>
                Open a project the first time. Switch a branch with a different
                Kotlin version and reload. In Gradle or sbt that&rsquo;s a
                configuration phase, plugin loading, dep resolution, and IDE
                model rebuild: minutes on real projects. Bleep reads{" "}
                <code>bleep.yaml</code>, builds the BSP model, syncs to the IDE.
                Initial import: a second or two. Branch reload: milliseconds.
              </p>
            </article>
            <article className={styles.mcpCard}>
              <h3 className={styles.mcpCardTitle}>
                <em>Incremental</em> compile
              </h3>
              <p className={styles.mcpCardBody}>
                One file changed in a 200-class module. Maven recompiles all
                200, slowly. Bleep does file-level incremental compilation: one
                file changed, one (or two) recompiled, in milliseconds. And
                asking the big question &mdash;{" "}
                <em>is everything still green?</em> &mdash; costs{" "}
                {stats.timings.noopCompileSeconds} seconds across all{" "}
                {stats.compiled.projects} projects and{" "}
                {stats.compiled.linesLabel} lines. That is the whole tax for
                asking.
              </p>
            </article>
          </div>
        </Reveal>
      </div>
    </section>
  );
}

/* ------------------------------------------------------------------
   CI, bleep invalidated + remote cache, the outer-loop money win.
   ------------------------------------------------------------------ */
function CISection() {
  return (
    <section className={`${styles.section} ${styles.sectionPaper}`}>
      <div className={styles.container}>
        <SectionHeader
          eyebrow="CI"
          title={
            <>
              Stupidly <em>fast</em> CI.
            </>
          }
        >
          The same simplification pays off again at CI scale. Build only what
          changed, pull the rest from cache: two commands, and your CI bill
          stops being a thing you complain about.
        </SectionHeader>

        <Reveal>
          <div className={styles.mcpGrid}>
            <article className={styles.mcpCard}>
              <h3 className={styles.mcpCardTitle}>
                Skip what <em>hasn&rsquo;t</em> changed
              </h3>
              <p className={styles.mcpCardBody}>
                <code>bleep build invalidated</code> loads the build at two git
                refs, digests each project from config plus sources plus
                transitive deps, and prints the ones that differ. Both loads are
                instant because the build is data and dependency resolution is
                cached. Scope the rest of your CI run to those projects.
                Everything else is already green from the last build.
              </p>
            </article>
            <article className={styles.mcpCard}>
              <h3 className={styles.mcpCardTitle}>
                Pull what <em>someone else</em> built
              </h3>
              <p className={styles.mcpCardBody}>
                <code>bleep remote-cache push</code> uploads compile outputs to
                S3, keyed by a SHA-256 over config plus sources plus transitive
                deps. <code>bleep remote-cache pull</code> fetches them on the
                next run. Skip the compile entirely for projects that
                haven&rsquo;t changed.
              </p>
            </article>
            <article className={styles.mcpCard}>
              <h3 className={styles.mcpCardTitle}>
                <em>Explicit</em>, on purpose
              </h3>
              <p className={styles.mcpCardBody}>
                No transparent freshness checks across the network. You push
                when you want a cache populated, you pull when you want to use
                it. The fail-hard error model stays clean, your CI logs stay
                grep-able — and agents benefit most of all: nothing degrades
                silently, so there&rsquo;s nothing to chase.
              </p>
            </article>
          </div>
        </Reveal>
      </div>
    </section>
  );
}

/* ------------------------------------------------------------------
   Self-editing build, bleep rewrites its own bleep.yaml.
   ------------------------------------------------------------------ */
function RoundtripSection() {
  return (
    <section className={styles.section}>
      <div className={styles.container}>
        <SectionHeader
          eyebrow="Self-editing"
          title={
            <>
              Read it. Change it. Write it <em>back</em>.
            </>
          }
        >
          Build-as-data has one more payoff: bleep can rewrite its own input.{" "}
          <code>update-deps</code>, <code>project-rename</code>,
          <code>templates-reapply</code>: each reads the file, transforms the
          model, writes it back. No DSL to interpret, no build plugin lifecycle
          to mutate, just a small library of commands operating on the same
          model bleep itself uses.
        </SectionHeader>

        <Reveal>
          <div className={styles.roundtripPills}>
            <div className={styles.roundtripPillRow}>
              <span
                className={`${styles.roundtripPillKind} ${styles.roundtripPillKindMutate}`}
              >
                <span
                  className={styles.roundtripPillKindDot}
                  aria-hidden="true"
                />
                Mutate
              </span>
              <span className={styles.roundtripPillVerbs}>
                <Link to="/docs/reference/cli/build/update-deps/">bump</Link>{" "}
                <i>·</i>{" "}
                <Link to="/docs/reference/cli/build/project-rename/">
                  rename
                </Link>{" "}
                <i>·</i>{" "}
                <Link to="/docs/reference/cli/build/project-merge-into/">
                  merge
                </Link>{" "}
                <i>·</i>{" "}
                <Link to="/docs/reference/cli/build/projects-move/">move</Link>{" "}
                <i>·</i>{" "}
                <Link to="/docs/reference/cli/build/normalize/">normalize</Link>{" "}
                <i>·</i>{" "}
                <Link to="/docs/reference/cli/build/templates-reapply/">
                  templatize
                </Link>{" "}
                <i>·</i>{" "}
                <Link to="/docs/reference/cli/build/templates-generate-new/">
                  re-infer
                </Link>
              </span>
            </div>
            <div className={styles.roundtripPillRow}>
              <span
                className={`${styles.roundtripPillKind} ${styles.roundtripPillKindInspect}`}
              >
                <span
                  className={styles.roundtripPillKindDot}
                  aria-hidden="true"
                />
                Inspect
              </span>
              <span className={styles.roundtripPillVerbs}>
                <Link to="/docs/reference/cli/build/show/">show</Link> <i>·</i>{" "}
                <Link to="/docs/reference/cli/build/diff/">diff</Link> <i>·</i>{" "}
                <Link to="/docs/reference/cli/build/invalidated/">
                  invalidated
                </Link>{" "}
                <i>·</i>{" "}
                <Link to="/docs/reference/cli/build/evicted/">evicted</Link>
              </span>
            </div>
          </div>
        </Reveal>

        <Reveal delay={140}>
          <aside className={styles.templatesCallout}>
            <strong>About templates.</strong>
            Templates keep <code>bleep.yaml</code> short. They never make it
            opaque.{" "}
            <Link to="/docs/reference/cli/build/show/">
              <code>bleep build show effective</code>
            </Link>{" "}
            and{" "}
            <Link to="/docs/reference/cli/build/diff/">
              <code>bleep build diff effective</code>
            </Link>{" "}
            always give you the fully expanded view, exactly what bleep sees,
            exactly what you can grep through or feed to CI.{" "}
            <em>
              The compactness is for humans. The transparency is for tools.
            </em>
          </aside>
        </Reveal>
      </div>
    </section>
  );
}

/* ------------------------------------------------------------------
   Build extensions, the codegen + scripts argument.
   Reassures readers that everything build plugins do can still be done.
   ------------------------------------------------------------------ */
function BuildExtensionsSection() {
  return (
    <section className={`${styles.section} ${styles.sectionPaper}`}>
      <div className={styles.container}>
        <SectionHeader
          eyebrow="What about build plugins?"
          title={
            <>
              <em>Code</em>, not build plugins.
            </>
          }
        >
          Two integration points cover what build plugins ever did. Most of that
          didn&rsquo;t belong in the build to begin with.
        </SectionHeader>

        <div
          style={{
            maxWidth: "720px",
            margin: "2.25rem auto 0",
            display: "grid",
            gap: "1.5rem",
          }}
        >
          <Reveal>
            <article>
              <h3 className={styles.dossierTitle}>
                A build plugin is a black box.
              </h3>
              <p className={styles.dossierBody}>
                Rules you don&rsquo;t write, settings you can&rsquo;t see, order
                you don&rsquo;t control. Debugged with <code>println</code>.
              </p>
            </article>
          </Reveal>

          <Reveal delay={80}>
            <article>
              <h3 className={styles.dossierTitle}>
                Most plugin work isn&rsquo;t build work.
              </h3>
              <p className={styles.dossierBody}>
                Signing, containers, docs, CI glue: distribution. None of it
                runs when you save a file; none of it needs to be coupled to
                compile and test. Write a script, run it when you want it.
              </p>
            </article>
          </Reveal>

          <Reveal delay={160}>
            <article>
              <h3 className={styles.dossierTitle}>
                Two patterns cover the rest.
              </h3>
              <p className={styles.dossierBody}>
                Bring a build plugin&rsquo;s logic into bleep and it becomes one
                of two things.
              </p>
              <ul className={styles.dossierList}>
                <li>
                  Generates files the compiler reads &rarr; the build runs it as{" "}
                  <code>sourcegen</code> before compile.
                </li>
                <li>
                  Operates on what compile produced &rarr; you run it after.
                </li>
              </ul>
            </article>
          </Reveal>
        </div>

        <Reveal delay={220}>
          <p
            className={styles.sectionLede}
            style={{ marginTop: "2.25rem", textAlign: "center" }}
          >
            We verified this model three ways: by analyzing each of the{" "}
            <Link to="/docs/compared-to-other-build-tools/maven-plugin-coverage/">
              top 50 Maven plugins
            </Link>
            , by implementing the hardest case (
            <Link to="/docs/spring-boot-proves-the-model/">Spring Boot</Link>
            ), and by shipping{" "}
            <Link to="/docs/appendix/status/">
              codebases of millions of lines
            </Link>{" "}
            on it.
          </p>
        </Reveal>

        <Reveal delay={260}>
          <p className={styles.compareCta}>
            <Link
              className={styles.compareCtaLink}
              to="/docs/concepts/scripts/"
            >
              How scripts and source generation work &nbsp;&rarr;
            </Link>
          </p>
        </Reveal>
      </div>
    </section>
  );
}

/* ------------------------------------------------------------------
   Test runner, parallel, live TUI, actionable summary
   ------------------------------------------------------------------ */
function TestRunnerSection() {
  return (
    <section className={styles.section}>
      <div className={styles.container}>
        <SectionHeader
          eyebrow="Tests"
          title={
            <>
              A test runner that <em>shows its work</em>.
            </>
          }
        >
          Nine minutes of test work, done in one. That is bleep testing itself
          below — {stats.ownTests.suites} suites, {stats.ownTests.tests} tests,
          integration builds and all, green in {stats.ownTests.wallClockSeconds}{" "}
          seconds. Suites compile and run in parallel across every CPU, and you
          watch it happen: which suite is running, which just failed, the moment
          it fails. No staring at a dead terminal wondering how far along it is.
        </SectionHeader>

        <Reveal>
          <div className={styles.testRunnerVideo}>
            <AsciinemaPlayer
              src={ownTestsCast}
              cols={100}
              rows={40}
              fit="width"
            />
          </div>
        </Reveal>

        <Reveal delay={120}>
          <div className={styles.mcpGrid}>
            <article className={styles.mcpCard}>
              <h3 className={styles.mcpCardTitle}>
                Massively <em>parallel</em>
              </h3>
              <p className={styles.mcpCardBody}>
                Test suites run in forked JVMs across every available CPU. Each
                test project gets its own classpath, its own JVM, its own
                lifecycle. The bottleneck is your hardware, not the build tool.
              </p>
            </article>
            <article className={styles.mcpCard}>
              <h3 className={styles.mcpCardTitle}>
                <em>Live</em> TUI
              </h3>
              <p className={styles.mcpCardBody}>
                The terminal shows which suites are compiling, which are
                running, which finished, which failed. Failures land the instant
                they happen, not at the end of the run. Pass{" "}
                <code>--no-tui</code> for plain CI logs.
              </p>
            </article>
            <article className={styles.mcpCard}>
              <h3 className={styles.mcpCardTitle}>
                Precise <em>summary</em>
              </h3>
              <p className={styles.mcpCardBody}>
                Exact suite and test names, pass/fail counts per project, and
                the failures written out with their assertions — short enough to
                act on without scrolling. <code>--diff</code> narrows it to what
                changed since your last run, and JUnit XML is one flag away.
              </p>
            </article>
          </div>
        </Reveal>
      </div>
    </section>
  );
}

/* ------------------------------------------------------------------
   Agents — one section, three beats: a subagent in a worktree got
   cheap, the answers are tiny, and diffing runs opens loops that
   weren't practical before.
   ------------------------------------------------------------------ */
function AgentsSection() {
  return (
    <section id="agents" className={`${styles.section} ${styles.sectionPaper}`}>
      <div className={styles.container}>
        <SectionHeader
          eyebrow="Agentic development"
          title={
            <>
              The best build tool for <em>agents</em>.
            </>
          }
        >
          An orchestrator, a handful of subagents, each one off in its own git
          worktree — that is what a working day looks like now. The build is
          what makes it expensive: every fresh worktree starts cold, and on a
          big codebase that is minutes of nothing, per agent, every time. Bleep
          collapses that price. A new worktree clones the parent&rsquo;s
          compiled state and hits verified green in{" "}
          <strong>
            {stats.timings.forkToVerifiedGreenSeconds} seconds on{" "}
            {stats.compiled.linesLabel} lines
          </strong>
          , against {stats.timings.forkToVerifiedGreenColdLabel} cold. Spawn six
          agents and the sixth starts as fast as the first.
        </SectionHeader>

        <Reveal delay={60}>
          <div style={{ marginTop: "2.25rem" }}>
            <p
              className={styles.sectionLede}
              style={{ textAlign: "center", marginBottom: "1.25rem" }}
            >
              Below, two subagents go into fresh worktrees. One seeds itself,
              one doesn&rsquo;t, and bleep times them both.{" "}
              <Link to="https://github.com/oyvindberg/bleep/tree/master/demo-claude-agents">
                The prompt is in the repo
              </Link>
              . Seeding costs <strong>{stats.demo.copyStateMs}&nbsp;ms</strong>;
              the cold worktree&rsquo;s first compile then takes{" "}
              <strong>{stats.demo.coldFirstCompileMs}&nbsp;ms</strong> against
              the seeded one&rsquo;s{" "}
              <strong>{stats.demo.seededFirstCompileMs}&nbsp;ms</strong>. The
              model never does that arithmetic &mdash; it asks bleep, and bleep
              reads both runs back out of its own history. Toy build, so it fits
              on camera; the same mechanism is what turns{" "}
              {stats.timings.forkToVerifiedGreenColdLabel} into{" "}
              {stats.timings.forkToVerifiedGreenSeconds}&nbsp;s on{" "}
              {stats.compiled.linesLabel} lines.
            </p>
            <div className={styles.testRunnerVideo}>
              <AsciinemaPlayer
                src={claudeAgentsCast}
                cols={110}
                rows={40}
                fit="width"
              />
            </div>
          </div>
        </Reveal>

        <Reveal delay={80}>
          <p
            className={styles.sectionLede}
            style={{ textAlign: "center", marginTop: "0.5rem" }}
          >
            Then there is what it costs to <em>ask</em>. Maven and Gradle bury
            an agent in output so reliably that agent companies{" "}
            <Link to="https://www.humanlayer.dev/blog/context-efficient-backpressure">
              ship wrappers
            </Link>{" "}
            whose whole job is stopping build logs from shredding context. Bleep
            just answers in sentences.
          </p>
        </Reveal>

        <Reveal>
          <p className={styles.mcpStat}>
            <span className={styles.mcpStatFigure}>
              {stats.tokens.rangeLabel} tokens
            </span>
            <span className={styles.mcpStatRest}>
              measured over the live MCP server: a green compile answers in{" "}
              {stats.tokens.greenCompile}, a failing test with its full
              what-changed diff in {stats.tokens.failingTestWithDiff}. A raw
              build log runs to tens of thousands, and every one of them costs
              the agent attention it could spend on your code.
            </span>
          </p>
        </Reveal>

        <Reveal delay={60}>
          <p
            className={styles.sectionLede}
            style={{ textAlign: "center", marginTop: "2.25rem" }}
          >
            And because every run is written down as a file, an agent can ask
            what changed instead of reading everything again. That is a loop
            other build tools cannot offer at any token price.
          </p>
        </Reveal>

        <Reveal delay={80}>
          <Vignette
            rows={[
              {
                actor: "agent",
                deed: "breaks a test, reruns — diffBase pinned to the last green run",
              },
              {
                actor: "",
                call: "bleep.test { directory: ~/repo/wt/api, diffBase: 18 }",
                result:
                  '{ failed: 1, diff: { summary: "1 newlyFailing", test: "PricingTest.10 percent off at 100 and above", message: "expected 216, obtained 204" } }',
                bad: true,
                note: "the run and what changed, in one call",
              },
              { gap: true },
              { actor: "agent", deed: "reverts, reruns" },
              {
                actor: "",
                call: "bleep.test { directory: ~/repo/wt/api, diffBase: 18 }",
                result: "{ passed: 14, diff: { identical: true } }",
                good: true,
                note: "800ms of timing noise between the runs, zero false diffs",
              },
            ]}
          />
        </Reveal>

        <Reveal>
          <div className={styles.mcpGrid}>
            <article className={styles.mcpCard}>
              <h3 className={styles.mcpCardTitle}>
                Forks start <em>warm</em>
              </h3>
              <p className={styles.mcpCardBody}>
                One <code>bleep.copy-state</code> call clones the parent&rsquo;s
                compiled state, safely, even mid-compile. One daemon serves
                every checkout, so ten worktrees don&rsquo;t cost ten JVMs.{" "}
                <Link to="/docs/guides/worktrees">Recipe and numbers</Link>.
              </p>
            </article>
            <article className={styles.mcpCard}>
              <h3 className={styles.mcpCardTitle}>
                Answers, not <em>logs</em>
              </h3>
              <p className={styles.mcpCardBody}>
                Counts, the first errors, an id — and failures stream the moment
                they happen. Want the detail? <code>history.show</code> greps
                the stored run server-side and hands back the matching lines.
              </p>
            </article>
            <article className={styles.mcpCard}>
              <h3 className={styles.mcpCardTitle}>
                Ask what <em>changed</em>
              </h3>
              <p className={styles.mcpCardBody}>
                <code>history.diff</code> compares any two runs — newly failing,
                fixed, new and resolved diagnostics — and{" "}
                <code>diff-timing</code> answers what got slower. Durations
                never enter the logical diff, so a busy laptop can&rsquo;t
                invent a difference.
              </p>
            </article>
          </div>
        </Reveal>

        <Reveal delay={60}>
          <p className={styles.compareCta}>
            <Link
              className={styles.compareCtaLink}
              to="/docs/usage/mcp-server/"
            >
              All 18 MCP tools &nbsp;&rarr;
            </Link>
          </p>
        </Reveal>

        <Reveal delay={100}>
          <p
            className={styles.sectionLede}
            style={{ marginTop: "2.25rem", textAlign: "center" }}
          >
            We build bleep this way every day: parallel agents in git worktrees,
            building bleep with bleep.
          </p>
        </Reveal>
      </div>
    </section>
  );
}

/* ------------------------------------------------------------------
   Migration — the same story from both directions: getting in, and
   getting out again. Both work, neither is magic, and saying so is
   the point.
   ------------------------------------------------------------------ */
function MigrationSection() {
  return (
    <section className={`${styles.section} ${styles.sectionPaper}`}>
      <div className={styles.container}>
        <SectionHeader
          eyebrow="Getting in, getting out"
          title={
            <>
              The door swings <em>both ways</em>.
            </>
          }
        >
          Adopting a build tool is a bet, so bleep works to make it a small one.{" "}
          <code>bleep import</code> reads an sbt build,{" "}
          <code>bleep import-maven</code> a Maven one, and writes the equivalent{" "}
          <code>bleep.yaml</code> — project graph derived, templates inferred
          from whatever repeats. You should be compiling and testing after one
          command. And when you want out, <code>bleep export-maven</code> walks
          the same model the other way and writes POMs.
        </SectionHeader>

        <Reveal>
          <div className={styles.mcpGrid}>
            <article className={styles.mcpCard}>
              <h3 className={styles.mcpCardTitle}>
                One command <em>in</em>
              </h3>
              <p className={styles.mcpCardBody}>
                <Link to="/docs/reference/cli/import/">
                  <code>bleep import</code>
                </Link>{" "}
                for sbt,{" "}
                <Link to="/docs/reference/cli/import-maven/">
                  <code>bleep import-maven</code>
                </Link>{" "}
                for Maven. Multi-module reactors included: the project graph is
                derived, repeated configuration lifted into templates, and
                compile and test run immediately.{" "}
                <Link to="/docs/demos/importing-maven-build">Watch one</Link>.
              </p>
            </article>
            <article className={styles.mcpCard}>
              <h3 className={styles.mcpCardTitle}>
                One command <em>out</em>
              </h3>
              <p className={styles.mcpCardBody}>
                <code>bleep export-maven</code> is a script, not a product: it
                walks the build model and writes POMs. Run against bleep&rsquo;s
                own build, every module compiles and the tests pass under stock
                Maven — source generators included.
              </p>
            </article>
            <article className={styles.mcpCard}>
              <h3 className={styles.mcpCardTitle}>
                Worst case, you hold <em>data</em>
              </h3>
              <p className={styles.mcpCardBody}>
                Your build is portable YAML and plain Maven coordinates. No
                plugin state, no tool-internal database, nothing that exists
                only inside bleep. Betting on a young tool should never mean
                betting the repo.
              </p>
            </article>
          </div>
        </Reveal>

        <Reveal delay={100}>
          <p
            className={styles.sectionLede}
            style={{ marginTop: "1.75rem", textAlign: "center" }}
          >
            Neither direction is magic, and we would rather say so than have you
            find out on day three. Import gets you green and then leaves the
            parts that were always going to need a person: source generation
            arrives as a frozen snapshot that works immediately and goes stale
            the day your schemas change, so you replace it with a real{" "}
            <Link to="/docs/concepts/sourcegen/">sourcegen script</Link> —
            usually tens of lines. Coming from Gradle there is no importer at
            all yet; that is a hand-port. Export has its own edges: publish
            configuration does not translate, and the integration suites that
            drive bleep&rsquo;s own compile server cannot run under Maven. What
            both directions do give you is a build that compiles and tests on
            the other side, which is the hard part.
          </p>
        </Reveal>

        <Reveal delay={140}>
          <p className={styles.compareCta}>
            <Link
              className={styles.compareCtaLink}
              to="/docs/guides/exit-strategy"
            >
              The exit strategy, written down &nbsp;&rarr;
            </Link>
          </p>
        </Reveal>
      </div>
    </section>
  );
}

/* ------------------------------------------------------------------
   Install + CTA
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
          eyebrow="Stop fighting your build"
          title={
            <>
              Two lines. One <em>file</em>. Get on with your day.
            </>
          }
        >
          Bleep is open source under the MIT licence. Java, Kotlin, and Scala on
          the JVM. Cross-build to JS and Native if you want. Or don&rsquo;t. The
          second line gives every agent on your machine a build tool that
          answers in {stats.tokens.failingTestWithDiff} tokens or fewer.
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
            <Link
              className={styles.btnSecondary}
              to="/docs/tutorials/your-first-project/"
            >
              First project
            </Link>
            <Link
              className={styles.btnSecondary}
              to="https://github.com/oyvindberg/bleep"
            >
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
export default function Home() {
  return (
    <Layout
      title="A build tool that gives a damn"
      description="Bleep is a JVM build tool for Java, Kotlin, and Scala. One YAML file. Native CLI. One-second IDE imports. MCP-native and built for agentic development: structured tool calls, token-compact output, first-class git worktrees. No code in your build, no project scopes, no build plugin acrobatics."
    >
      <div className={styles.page}>
        <Hero />
        <main>
          <SpecimenSection />
          <MaturitySection />
          <RefusalsSection />
          <BuildExtensionsSection />
          <PerformanceSection />
          <TestRunnerSection />
          <CISection />
          <RoundtripSection />
          <AgentsSection />
          <MigrationSection />
          <InstallCTA />
        </main>
      </div>
    </Layout>
  );
}
