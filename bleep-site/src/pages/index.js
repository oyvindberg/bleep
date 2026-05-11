import React, { useEffect, useRef, useState } from "react";
import Link from "@docusaurus/Link";
import Layout from "@theme/Layout";
import Snippet from "@site/src/components/Snippet";
import styles from "./index.module.css";

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
          Compile. Test. Sourcegen.<br />
          That&rsquo;s the <em>build</em>. Everything else is code.
        </h1>

        <p className={styles.heroTagline}>
          Two decades of Maven, Gradle, and sbt is a long time to watch
          build tools grow incredibly complex. We built one that won&rsquo;t.
          Bleep does precisely what a build is for: compile, test, sourcegen,
          then package, link, publish what comes out. It refuses the rest.
          Your container build is code you write. So is your doc generation,
          your sidecar boot, your CI orchestration. All of it.
        </p>

        <div className={styles.heroButtons}>
          <Link className={styles.btnPrimary} to="/docs/installing/">
            Install
          </Link>
          <Link className={styles.btnSecondary} to="/docs/tutorials/your-first-project/">
            Your first project
          </Link>
          <Link className={styles.btnSecondary} to="https://github.com/oyvindberg/bleep">
            GitHub
          </Link>
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
    title: <>No <em>code</em> in the build file.</>,
    body: (
      <>
        A build file describes a project. It doesn&rsquo;t run one.{" "}
        <code>bleep.yaml</code> is data: readable top to bottom by
        anyone on the team. Logic lives in your code, in your repo,
        where you can <code>git blame</code> it.
      </>
    ),
  },
  {
    title: <>No <em>build plugins</em>.</>,
    body: (
      <>
        No autoplugins, no <code>requires</code> graphs,
        no <code>Plugin&lt;Project&gt;</code> registration. Bleep
        doesn&rsquo;t have one. Code goes in your repo, where you can
        read it.
      </>
    ),
  },
  {
    title: <>No <em>project scopes</em>.</>,
    body: (
      <>
        A test project is a project. A scripts project (your build
        code) is a project. Your production app is a project. Same
        fields, same dependency model, same{" "}
        <code>bleep compile</code> and <code>bleep test</code>, no
        second category. No <code>Test/test/itTest/Compile</code>{" "}
        scope dance grafted onto the project graph. A project is a
        project is a project.
      </>
    ),
  },
  {
    title: <>No <em>task graph</em>.</>,
    body: (
      <>
        There&rsquo;s no user-definable task DAG. The build does
        compile, test, sourcegen. Everything else is a script: a{" "}
        <code>main</code> class you call when you want to. Composable
        like programs, debuggable like programs, no special layer
        between you and the JVM.
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
          A project is a project is a project. Code is code is code.
          Everything explicit, everything simple. Here&rsquo;s how we
          got there.
        </SectionHeader>

        <div className={styles.dossierGrid}>
          {refusals.map((r, i) => (
            <Reveal key={i} delay={(i % 4) * 60}>
              <article className={`${styles.dossierCard} ${styles.dossierCardRefuse}`}>
                <div className={styles.dossierHead}>
                  <span className={styles.dossierKicker}>
                    <span className={styles.dossierDot} />
                    <span>Simplification</span>
                  </span>
                  <span className={styles.dossierNum}>{String(i + 1).padStart(2, "0")}</span>
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
    bumpFromTo: <>Move <code>2.3.0</code> to <code>2.4.0</code> in <code>template-common</code></>,
  },
  {
    id: "java",
    label: "Java",
    fixture: "your-first-project",
    competitor: <code>pom.xml</code>,
    bumpLabel: "Bump JUnit in one line.",
    bumpFromTo: <>Move <code>5.10.1</code> to <code>5.11.0</code> on <code>myapp-test</code></>,
  },
  {
    id: "scala",
    label: "Scala",
    fixture: "your-first-scala-project",
    competitor: <code>build.sbt</code>,
    bumpLabel: "Bump Scala in one line.",
    bumpFromTo: <>Move <code>3.8.3</code> to <code>3.9.0</code> in <code>template-common</code></>,
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
          A real <code>bleep.yaml</code>. Not pseudocode. Not a
          marketing render. Plain YAML you can read, grep, diff, and
          rewrite. The same model bleep itself uses.
        </SectionHeader>

        <Reveal>
          <div className={styles.specimenFrame}>
            <div className={styles.specimenHead}>
              <div className={styles.specimenTabs} role="tablist" aria-label="Choose specimen language">
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
          Cut the code, the build plugins, the scopes, the task graph.
          The inner loop stops being something you wait for.
        </SectionHeader>

        <Reveal>
          <div className={styles.mcpGrid}>
            <article className={styles.mcpCard}>
              <h3 className={styles.mcpCardTitle}>
                Load <em>everything</em> in milliseconds
              </h3>
              <p className={styles.mcpCardBody}>
                Native CLI binary. Reads <code>bleep.yaml</code>,
                resolves dependencies through Coursier&rsquo;s local
                cache, builds the full project model. Done. No JVM
                startup, no configuration phase, no &ldquo;loading
                projects&hellip;&rdquo; progress bar. The compile
                daemon (<code>bleep-bsp</code>) is the JVM-heavy
                bit, and it stays hot between invocations.
              </p>
            </article>
            <article className={styles.mcpCard}>
              <h3 className={styles.mcpCardTitle}>
                IDE imports &amp; <em>reloads</em>
              </h3>
              <p className={styles.mcpCardBody}>
                Open a project the first time. Switch a branch with a
                different Kotlin version and reload. In Gradle or sbt
                that&rsquo;s a configuration phase, plugin loading,
                dep resolution, and IDE model rebuild: minutes on
                real projects. Bleep reads <code>bleep.yaml</code>,
                builds the BSP model, syncs to the IDE. Initial
                import: a second or two. Branch reload: milliseconds.
              </p>
            </article>
            <article className={styles.mcpCard}>
              <h3 className={styles.mcpCardTitle}>
                <em>Incremental</em> compile
              </h3>
              <p className={styles.mcpCardBody}>
                One file changed in a 200-class module. Maven
                recompiles all 200, slowly. Bleep uses Zinc with
                file-level tracking: one file changed, one (or two)
                recompiled, in milliseconds. The save-to-result loop
                stays tight.
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
          The same simplification pays off again at CI scale. Build
          only what changed, pull the rest from cache: two commands,
          and your CI bill stops being a thing you complain about.
        </SectionHeader>

        <Reveal>
          <div className={styles.mcpGrid}>
            <article className={styles.mcpCard}>
              <h3 className={styles.mcpCardTitle}>
                Skip what <em>hasn&rsquo;t</em> changed
              </h3>
              <p className={styles.mcpCardBody}>
                <code>bleep build invalidated</code> loads the build
                at two git refs, digests each project from config
                plus sources plus transitive deps, and prints the
                ones that differ. Both loads are instant because the
                build is data and dependency resolution is cached.
                Scope the rest of your CI run to those projects.
                Everything else is already green from the last build.
              </p>
            </article>
            <article className={styles.mcpCard}>
              <h3 className={styles.mcpCardTitle}>
                Pull what <em>someone else</em> built
              </h3>
              <p className={styles.mcpCardBody}>
                <code>bleep remote-cache push</code> uploads compile
                outputs to S3, keyed by a SHA-256 over config plus
                sources plus transitive deps.{" "}
                <code>bleep remote-cache pull</code> fetches them on
                the next run. Skip the compile entirely for projects
                that haven&rsquo;t changed.
              </p>
            </article>
            <article className={styles.mcpCard}>
              <h3 className={styles.mcpCardTitle}>
                <em>Explicit</em>, on purpose
              </h3>
              <p className={styles.mcpCardBody}>
                No transparent freshness checks across the network.
                You push when you want a cache populated, you pull
                when you want to use it. The fail-hard error model
                stays clean, your CI logs stay grep-able.
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
          Build-as-data has one more payoff: bleep can rewrite its own
          input. <code>update-deps</code>, <code>project-rename</code>,
          <code>templates-reapply</code>: each reads the file,
          transforms the model, writes it back. No DSL to interpret,
          no build plugin lifecycle to mutate, just a small library of
          commands operating on the same model bleep itself uses.
        </SectionHeader>

        <Reveal>
          <div className={styles.roundtripPills}>
            <div className={styles.roundtripPillRow}>
              <span className={`${styles.roundtripPillKind} ${styles.roundtripPillKindMutate}`}>
                <span className={styles.roundtripPillKindDot} aria-hidden="true" />
                Mutate
              </span>
              <span className={styles.roundtripPillVerbs}>
                <Link to="/docs/reference/cli/build/update-deps/">bump</Link> <i>·</i>{" "}
                <Link to="/docs/reference/cli/build/project-rename/">rename</Link> <i>·</i>{" "}
                <Link to="/docs/reference/cli/build/project-merge-into/">merge</Link> <i>·</i>{" "}
                <Link to="/docs/reference/cli/build/projects-move/">move</Link> <i>·</i>{" "}
                <Link to="/docs/reference/cli/build/normalize/">normalize</Link> <i>·</i>{" "}
                <Link to="/docs/reference/cli/build/templates-reapply/">templatize</Link> <i>·</i>{" "}
                <Link to="/docs/reference/cli/build/templates-generate-new/">re-infer</Link>
              </span>
            </div>
            <div className={styles.roundtripPillRow}>
              <span className={`${styles.roundtripPillKind} ${styles.roundtripPillKindInspect}`}>
                <span className={styles.roundtripPillKindDot} aria-hidden="true" />
                Inspect
              </span>
              <span className={styles.roundtripPillVerbs}>
                <Link to="/docs/reference/cli/build/show/">show</Link> <i>·</i>{" "}
                <Link to="/docs/reference/cli/build/diff/">diff</Link> <i>·</i>{" "}
                <Link to="/docs/reference/cli/build/invalidated/">invalidated</Link> <i>·</i>{" "}
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
            <Link to="/docs/reference/cli/build/show/"><code>bleep build show effective</code></Link>{" "}
            and{" "}
            <Link to="/docs/reference/cli/build/diff/"><code>bleep build diff effective</code></Link>{" "}
            always give you the fully
            expanded view, exactly what bleep sees, exactly what you can
            grep through or feed to CI. <em>The compactness is for humans.
            The transparency is for tools.</em>
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
          Two reasons, and a rule.
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
                <span className={styles.dossierNum} style={{ marginRight: "0.5em" }}>01</span>{" "}
                A build plugin is a black box.
              </h3>
              <p className={styles.dossierBody}>
                Rules you don&rsquo;t write, settings you can&rsquo;t
                see, order you don&rsquo;t control. Debugged with{" "}
                <code>println</code>.
              </p>
            </article>
          </Reveal>

          <Reveal delay={80}>
            <article>
              <h3 className={styles.dossierTitle}>
                <span className={styles.dossierNum} style={{ marginRight: "0.5em" }}>02</span>{" "}
                Most plugin work isn&rsquo;t build work.
              </h3>
              <p className={styles.dossierBody}>
                Signing, containers, docs, CI glue: distribution.
                None of it runs when you save a file. None of it
                needs a task DAG.
              </p>
            </article>
          </Reveal>

          <Reveal delay={160}>
            <article>
              <h3 className={styles.dossierTitle}>
                <span className={styles.dossierNum} style={{ marginRight: "0.5em" }}>03</span>{" "}
                The rule.
              </h3>
              <p className={styles.dossierBody}>
                Generates files the compiler reads &rarr; the build
                runs it as <code>sourcegen</code> before compile.
                Operates on what compile produced &rarr; you run it
                after. Both are programs. No plugin API.
              </p>
            </article>
          </Reveal>
        </div>

        <Reveal delay={220}>
          <p
            className={styles.sectionLede}
            style={{ marginTop: "2.25rem", textAlign: "center" }}
          >
            Every build plugin you&rsquo;ve used falls on one side of
            that line. The rest didn&rsquo;t belong in the build.
          </p>
        </Reveal>

        <Reveal delay={220}>
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
          Failures show up the moment they happen. Suites compile and run
          in parallel across every CPU, the terminal stays live, and the
          summary at the end is short enough to act on. No two-minute
          pause. No fifty-thousand-line transcript.
        </SectionHeader>

        <Reveal>
          <div className={styles.testRunnerVideo}>
            <video
              src="https://github.com/user-attachments/assets/e772a346-e999-495f-8a57-f8680b061cd3"
              controls
              loop
              muted
              playsInline
              preload="metadata"
              aria-label="bleep test runner showing parallel execution and live progress"
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
                Test suites run in forked JVMs across every available
                CPU. Each test project gets its own classpath, its own
                JVM, its own lifecycle. The bottleneck is your hardware,
                not the build tool.
              </p>
            </article>
            <article className={styles.mcpCard}>
              <h3 className={styles.mcpCardTitle}>
                <em>Live</em> TUI
              </h3>
              <p className={styles.mcpCardBody}>
                The terminal shows which suites are compiling, which are
                running, which finished, which failed. Failures land the
                instant they happen, not at the end of the run. Pass{" "}
                <code>--no-tui</code> for plain CI logs.
              </p>
            </article>
            <article className={styles.mcpCard}>
              <h3 className={styles.mcpCardTitle}>
                Precise <em>summary</em>
              </h3>
              <p className={styles.mcpCardBody}>
                When the run ends, you get exact suite and test names,
                pass/fail counts per project, and the diff against the
                previous run, not a wall of stdout you have to
                grep through. JUnit XML is one flag away (
                <code>--junit-report</code>).
              </p>
            </article>
          </div>
        </Reveal>
      </div>
    </section>
  );
}

/* ------------------------------------------------------------------
   MCP server, tooling for the future, agent-aware
   ------------------------------------------------------------------ */
function McpSection() {
  return (
    <section className={`${styles.section} ${styles.sectionPaper}`}>
      <div className={styles.container}>
        <SectionHeader
          eyebrow="Tooling for the future"
          title={
            <>
              Built for the <em>agents</em>.
            </>
          }
        >
          Claude Code, Cursor, and the next generation of dev tools talk
          to bleep through MCP, the Model Context Protocol. One
          command (<Link to="/docs/reference/cli/setup-mcp-server/"><code>bleep setup-mcp-server</code></Link>) and an agent can
          compile, test, run, and inspect your build through 18
          structured tool calls. The design assumptions are blunt: more
          than one agent will be running at once, tokens are scarce, and
          any tool that takes seconds blocks every agent attached to it.
        </SectionHeader>

        <Reveal>
          <div className={styles.mcpGrid}>
            <article className={styles.mcpCard}>
              <h3 className={styles.mcpCardTitle}>
                <em>Performance</em>
              </h3>
              <p className={styles.mcpCardBody}>
                Four parallel agents on the same build mean four parallel
                tool calls. If the build tool is slow, every one of them
                stalls, aggregate latency multiplies. Bleep&rsquo;s
                MCP server runs in-process against the BSP daemon. Every
                call is sub-second after warmup, sub-100ms once warm.
              </p>
            </article>
            <article className={styles.mcpCard}>
              <h3 className={styles.mcpCardTitle}>
                <em>Ease</em>
              </h3>
              <p className={styles.mcpCardBody}>
                <Link to="/docs/reference/cli/setup-mcp-server/"><code>bleep setup-mcp-server</code></Link> writes
                <code>.mcp.json</code> in your build root. Any MCP client
                picks it up automatically. No adapter to configure, no
                protocol to translate, bleep speaks MCP natively.
              </p>
            </article>
            <article className={styles.mcpCard}>
              <h3 className={styles.mcpCardTitle}>
                <em>Structured</em> output
              </h3>
              <p className={styles.mcpCardBody}>
                Every tool returns a compact JSON summary by default
                : error counts, failure suites, the diff against
                the previous run. Full diagnostics are one extra call
                away. Per-project errors stream as MCP notifications the
                instant a project finishes, not at the end. Latency
                floor for surfacing a real problem: milliseconds.
              </p>
            </article>
          </div>
        </Reveal>

        <Reveal delay={140}>
          <p className={styles.compareCta}>
            <Link
              className={styles.compareCtaLink}
              to="/docs/usage/mcp-server/"
            >
              MCP server docs &nbsp;→
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
  const installCmd = "curl -fsSL https://bleep.build/install | sh";

  const onCopy = () => {
    navigator.clipboard?.writeText(installCmd);
    setCopied(true);
    setTimeout(() => setCopied(false), 1600);
  };

  return (
    <section className={styles.cta}>
      <div className={styles.container}>
        <SectionHeader
          eyebrow="Stop fighting your build"
          title={
            <>
              One line. One <em>file</em>. Get on with your day.
            </>
          }
        >
          Bleep is open source under Apache 2.0. Java, Kotlin, and Scala on the JVM.
          Cross-build to JS and Native if you want. Or don&rsquo;t.
        </SectionHeader>

        <Reveal>
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
export default function Home() {
  return (
    <Layout
      title="A build tool that gives a damn"
      description="Bleep is a JVM build tool for Java, Kotlin, and Scala. One YAML file. Native CLI. One-second IDE imports. No code in your build. No project scopes, no XML, no build plugin acrobatics."
    >
      <div className={styles.page}>
        <Hero />
        <main>
          <SpecimenSection />
          <RefusalsSection />
          <BuildExtensionsSection />
          <PerformanceSection />
          <CISection />
          <RoundtripSection />
          <TestRunnerSection />
          <McpSection />
          <InstallCTA />
        </main>
      </div>
    </Layout>
  );
}
