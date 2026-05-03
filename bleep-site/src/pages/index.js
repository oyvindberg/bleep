import React, { useEffect, useRef, useState } from "react";
import Link from "@docusaurus/Link";
import Layout from "@theme/Layout";
import Snippet from "@site/src/components/Snippet";
import styles from "./index.module.css";

/* ------------------------------------------------------------------
   Reveal — scroll-triggered fade + rise
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
   Hero — cursor-tracked glow, drift aurora, staggered entrance
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
          Your build is <em>data</em>.<br />
          Not a program.
        </h1>

        <p className={styles.heroTagline}>
          One YAML file. Native performance. IDE imports over{" "}
          <code>BSP</code> (Build Server Protocol; what IntelliJ and Metals already
          speak), agent tool calls via <code>MCP</code> (Model Context Protocol;
          what Claude Code already speaks) &mdash; both open standards, both
          first-class.
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

      <div className={styles.heroFacts}>
        <div className={styles.heroFact}>
          <span className={styles.heroFactLabel}>Cold start</span>
          <span className={styles.heroFactValue}>
            <em>10</em>
            <span className={styles.heroFactSub}> ms</span>
          </span>
          <span className={styles.heroFactSub}>GraalVM native image</span>
        </div>
        <div className={styles.heroFact}>
          <span className={styles.heroFactLabel}>IDE import</span>
          <span className={styles.heroFactValue}>
            <em>1</em>
            <span className={styles.heroFactSub}> sec</span>
          </span>
          <span className={styles.heroFactSub}>IntelliJ · Metals · BSP</span>
        </div>
        <div className={styles.heroFact}>
          <span className={styles.heroFactLabel}>Build format</span>
          <span className={styles.heroFactValue}>
            <em>1</em>
            <span className={styles.heroFactSub}> file</span>
          </span>
          <span className={styles.heroFactSub}>readable, diffable YAML</span>
        </div>
        <div className={styles.heroFact}>
          <span className={styles.heroFactLabel}>Code in your build</span>
          <span className={styles.heroFactValue}>
            <em>0</em>
            <span className={styles.heroFactSub}> lines</span>
          </span>
          <span className={styles.heroFactSub}>scripts live elsewhere</span>
        </div>
      </div>
    </header>
  );
}

/* ------------------------------------------------------------------
   Refusals — the things we will not have
   ------------------------------------------------------------------ */
const refusals = [
  {
    title: <>No <em>build-as-program</em>.</>,
    body: (
      <>
        Your build should be readable by somebody who has never opened
        the project. Turing-complete build files import
        complexity in exchange for flexibility most teams will never use
        &mdash; and the complexity stays whether the flexibility gets
        used or not.
      </>
    ),
  },
  {
    title: <>No <em>scopes</em>.</>,
    body: (
      <>
        Dependencies are a flat list. <code>compile</code>,
        <code>provided</code>, <code>runtime</code>, <code>Test/test/it/Compile</code>
        graft a second axis on top. Different context? Different
        project. Flat is enough.
      </>
    ),
  },
  {
    title: <>No <em>plugin acrobatics</em>.</>,
    body: (
      <>
        No <code>preCompile</code> mutators, no autoplugins,
        no <code>requires</code> graphs, no <code>Plugin&lt;Project&gt;</code>{" "}
        registration, no plugins fighting at runtime over which Guava
        is the real one. If you want custom build logic, write a
        regular Java, Kotlin, or Scala program. It compiles. It
        debugs. It has a <code>main</code>.
      </>
    ),
  },
  {
    title: <>No <em>30-second imports</em>.</>,
    body: (
      <>
        We will not negotiate with a Gradle or sbt import that needs a coffee
        break to figure out which Scala version your project uses, or which
        Kotlin compiler plugins it wants. Bleep imports through BSP in about
        one second.
      </>
    ),
  },
  {
    title: <>No <em>whole-module rebuilds</em>.</>,
    body: (
      <>
        Change one file in a 200-class Maven module and Maven recompiles all 200.
        Bleep uses Zinc with file-level incremental tracking — one file changed,
        one (or two) recompiled. Your CI minute count is not a place to economise on
        engineering effort.
      </>
    ),
  },
];

function RefusalsSection() {
  return (
    <section className={styles.section}>
      <div className={styles.container}>
        <SectionHeader
          eyebrow="The refusal"
          title={
            <>
              Things we will <em>not</em> tolerate.
            </>
          }
        >
          Build tools have spent twenty years getting more clever and more opaque.
          Bleep is the build tool that says <strong>no</strong> for you, on principle,
          before someone proposes a Maven enforcer plugin in your standup.
        </SectionHeader>

        <div className={styles.dossierGrid}>
          {refusals.map((r, i) => (
            <Reveal key={i} delay={(i % 4) * 60}>
              <article className={`${styles.dossierCard} ${styles.dossierCardRefuse}`}>
                <div className={styles.dossierHead}>
                  <span className={styles.dossierKicker}>
                    <span className={styles.dossierDot} />
                    <span>Refusal</span>
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
   Specimen — real bleep.yaml, loaded from the integration-test snippets
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
          eyebrow="The specimen"
          title={
            <>
              What declarative <em>actually</em> looks like.
            </>
          }
        >
          A real <code>bleep.yaml</code>. Not pseudocode. Not a marketing render.
          Anyone on your team can read this and tell you exactly what the build does
          — without launching anything.
        </SectionHeader>

        <div className={styles.specimenSplit}>
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

          <div className={styles.specimenNotes}>
            <Reveal delay={80}>
              <div className={styles.specimenNote}>
                <strong>Two projects. One template. Plain text.</strong>
                An app (<code>myapp</code>) and its tests (<code>myapp-test</code>),
                both extending <code>template-common</code>. Tens of lines, no
                plugins, no {lang.competitor}.
              </div>
            </Reveal>
            <Reveal delay={140}>
              <div className={styles.specimenNote}>
                <strong>Tests are projects.</strong>
                <code>myapp-test</code> is just another project. It depends on
                <code>myapp</code>, picks up the template defaults, and flips
                <code>isTestProject: true</code>. No <code>Test/test/itTest/Compile</code>
                scope dance. No second build file.
              </div>
            </Reveal>
            <Reveal delay={200}>
              <div className={styles.specimenNote}>
                <strong>{lang.bumpLabel}</strong>
                {lang.bumpFromTo} and every project that extends it follows. Code
                review tells you exactly what changed &mdash; in plain text, with
                no graph viewer.
              </div>
            </Reveal>
            <Reveal delay={260}>
              <div className={styles.specimenNote}>
                <strong>This is a real test fixture.</strong>
                The YAML on the left is loaded from
                <code>{fixturePath}</code>
                &mdash; a workspace bleep&rsquo;s own integration tests build,
                compile, and run end-to-end on every commit. Not a brochure.
              </div>
            </Reveal>
          </div>
        </div>
      </div>
    </section>
  );
}

/* ------------------------------------------------------------------
   Round-trip — the build tool can rewrite the build, because it's data.
   ------------------------------------------------------------------ */
function RoundtripSection() {
  return (
    <section className={styles.section}>
      <div className={styles.container}>
        <SectionHeader
          eyebrow="Round-trip"
          title={
            <>
              Read it. Change it. Write it <em>back</em>.
            </>
          }
        >
          Because <code>bleep.yaml</code> is data, the build tool can do to it
          what any other tool can do to a file: read, transform, write. No DSL
          to interpret. No plugin lifecycle to mutate. Just the file and a
          small library of commands that round-trip cleanly through the same
          model bleep itself uses.
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
            expanded view &mdash; exactly what bleep sees, exactly what you can
            grep through or feed to CI. <em>The compactness is for humans.
            The transparency is for tools.</em>
          </aside>
        </Reveal>

      </div>
    </section>
  );
}

/* ------------------------------------------------------------------
   Tenets — what we stand for (closes with "bleeping performance")
   ------------------------------------------------------------------ */
const tenets = [
  {
    title: <>Builds are <em>data</em>.</>,
    body: (
      <>
        A build is a description of projects and dependencies, not a program that produces one.
        Data is greppable, diffable, generable, machine-rewritable. Code is none of those things
        without a parser you usually don't have.
      </>
    ),
  },
  {
    title: <>Round-trip, <em>always</em>.</>,
    body: (
      <>
        Bleep reads its build, transforms it, and writes it back. That&rsquo;s
        how <code>update-deps</code>, <code>project-rename</code>, and
        <code>templates-reapply</code> work; that&rsquo;s how your scripts can
        do the same. The model on disk and the model in memory are the
        <em> same model</em>.
      </>
    ),
  },
  {
    title: <>Open <em>standards</em>.</>,
    body: (
      <>
        Bleep speaks <code>BSP</code> — the Build Server Protocol. Any editor
        that speaks BSP talks to bleep on day one: IntelliJ, Metals, VS Code
        via Metals. We didn&rsquo;t invent a private wire format. We didn&rsquo;t
        ship our own IDE plugin. The ecosystem already agreed on the protocol.
      </>
    ),
  },
  {
    title: <>Debuggable <em>end to end</em>.</>,
    body: (
      <>
        Bleep is a normal program; scripts are normal programs. You can attach a debugger to
        the build tool itself, to the BSP server, to your scripts. There is no privileged
        layer of magic dust.
      </>
    ),
  },
  {
    title: <><em>Repeatable</em>, on purpose.</>,
    body: (
      <>
        <code>bleep.yaml</code> pins what turns YAML into bytecode &mdash;
        bleep itself, the JVM, and (for Scala.js) Node &mdash; and downloads
        the right versions on first run. Two checkouts of the same commit
        produce the same build, no matter what&rsquo;s installed on the host.
        No <code>.tool-versions</code>, no <code>nvmrc</code>, no
        &ldquo;works on my machine.&rdquo;
      </>
    ),
  },
  {
    title: <><em>Simplicity</em>, on purpose.</>,
    body: (
      <>
        We will refuse features that buy 5% flexibility for 50% complexity.
        We will pick the boring answer. We will not invent a fourth way to declare a dependency.
      </>
    ),
  },
  {
    title: <>Native, <em>by default</em>.</>,
    body: (
      <>
        Bleep is GraalVM native-image. The CLI starts in roughly the time it takes for your
        terminal to render the prompt. That is not a feature; that is the floor.
      </>
    ),
  },
  {
    title: <>Performance — bleeping <em>performance</em>.</>,
    body: (
      <>
        Instant CLI. One-second imports. Fast incremental compiles via Zinc, javac, and
        kotlinc, in a single BSP server we wrote for the job. We do not compete with build
        tools that are "fast enough." We are not.
      </>
    ),
  },
];

function TenetsSection() {
  return (
    <section className={`${styles.section} ${styles.tenetSection}`}>
      <div className={styles.container}>
        <SectionHeader
          eyebrow="The tenets"
          title={
            <>
              What we propose <em>instead</em>.
            </>
          }
        >
          A small list. We can recite it from memory at standup, in code review,
          and during the inevitable argument about whether to add a build plugin.
        </SectionHeader>

        <div className={styles.dossierGrid}>
          {tenets.map((t, i) => (
            <Reveal key={i} delay={(i % 4) * 60}>
              <article className={`${styles.dossierCard} ${styles.dossierCardTenet}`}>
                <div className={styles.dossierHead}>
                  <span className={styles.dossierKicker}>
                    <span className={styles.dossierDot} />
                    <span>Tenet</span>
                  </span>
                  <span className={styles.dossierNum}>{String(i + 1).padStart(2, "0")}</span>
                </div>
                <h3 className={styles.dossierTitle}>{t.title}</h3>
                <p className={styles.dossierBody}>{t.body}</p>
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
   Build extensions — the codegen + scripts argument.
   Reassures readers that everything plugins do can still be done.
   ------------------------------------------------------------------ */
function BuildExtensionsSection() {
  return (
    <section className={`${styles.section} ${styles.sectionPaper}`}>
      <div className={styles.container}>
        <SectionHeader
          eyebrow="What about plugins?"
          title={
            <>
              <em>Anything</em> a plugin does.
            </>
          }
        >
          Anyone arriving from sbt or Gradle gets the same uneasy thought
          looking at the YAML &mdash; <em>where does my codegen go? the
          publish step? the thing that signs my JARs?</em> Bleep has two
          places for that work, and both are real programs.
        </SectionHeader>

        <div
          style={{
            display: "grid",
            gridTemplateColumns: "repeat(2, minmax(0, 1fr))",
            gap: "1.5rem",
            marginTop: "2.25rem",
          }}
        >
          <Reveal>
            <article className={`${styles.dossierCard} ${styles.dossierCardTenet}`}>
              <div className={styles.dossierHead}>
                <span className={styles.dossierKicker}>
                  <span className={styles.dossierDot} />
                  <span>Before compile</span>
                </span>
                <span className={styles.dossierNum}>01</span>
              </div>
              <h3 className={styles.dossierTitle}>
                <em>Generate</em> code from what the build can see.
              </h3>
              <p className={styles.dossierBody}>
                A schema, a protobuf descriptor, a build version, an OpenAPI
                spec. You inspect the build, you generate the source, the
                build keeps compiling. <strong>Sourcegen</strong> is a
                first-class field in <code>bleep.yaml</code> &mdash; bleep
                runs your generator, hashes inputs, recompiles when needed,
                and never leaves stale generated code behind on failure.
              </p>
              <div className={styles.dossierAccent} aria-hidden="true" />
            </article>
          </Reveal>

          <Reveal delay={80}>
            <article className={`${styles.dossierCard} ${styles.dossierCardTenet}`}>
              <div className={styles.dossierHead}>
                <span className={styles.dossierKicker}>
                  <span className={styles.dossierDot} />
                  <span>After compile</span>
                </span>
                <span className={styles.dossierNum}>02</span>
              </div>
              <h3 className={styles.dossierTitle}>
                <em>Run</em> a program against the artifacts.
              </h3>
              <p className={styles.dossierBody}>
                Publish them, sign them, ship them, package them, post a
                release note. <strong>Scripts</strong> are regular Java,
                Kotlin, or Scala programs registered in <code>bleep.yaml
                </code>. They have a <code>main</code>. You debug them with
                breakpoints. They get a typed handle on the resolved build
                model. There is no privileged layer of magic dust between
                you and the JVM.
              </p>
              <div className={styles.dossierAccent} aria-hidden="true" />
            </article>
          </Reveal>
        </div>

        <Reveal delay={160}>
          <p
            className={styles.sectionLede}
            style={{ marginTop: "2.25rem", textAlign: "center" }}
          >
            The liberating part isn&rsquo;t the syntax. It&rsquo;s that you
            stop forcing every random thing into the build. <em>Compile.
            Follow the graph. Then say what you want in code.</em>
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
   Test runner — parallel, live TUI, actionable summary
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
          You don&rsquo;t watch a build tool think about tests for two
          minutes and get a fifty-thousand-line transcript dumped at the
          end. You watch suites compile, watch them run, see failures the
          second they happen, and walk away with a precise summary you
          can act on.
        </SectionHeader>

        <Reveal>
          <div className={styles.testRunnerVideo}>
            <video
              src="https://github.com/user-attachments/assets/5fc771a3-78b1-45bc-84f0-dd9d9822ca69"
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
                previous run &mdash; not a wall of stdout you have to
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
   MCP server — tooling for the future, agent-aware
   ------------------------------------------------------------------ */
function McpSection() {
  return (
    <section className={`${styles.section} ${styles.sectionPaper}`}>
      <div className={styles.container}>
        <SectionHeader
          eyebrow="Tooling for the future"
          title={
            <>
              Built for the <em>agents</em> next to you.
            </>
          }
        >
          Claude Code, Cursor, and the next generation of dev tools talk
          to bleep through MCP &mdash; the Model Context Protocol. One
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
                stalls &mdash; aggregate latency multiplies. Bleep&rsquo;s
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
                protocol to translate &mdash; bleep speaks MCP natively.
              </p>
            </article>
            <article className={styles.mcpCard}>
              <h3 className={styles.mcpCardTitle}>
                <em>Structured</em> output
              </h3>
              <p className={styles.mcpCardBody}>
                Every tool returns a compact JSON summary by default
                &mdash; error counts, failure suites, the diff against
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
   Pull quote — the manifesto punch
   ------------------------------------------------------------------ */
function PullQuoteSection() {
  return (
    <section className={styles.section}>
      <div className={styles.container}>
        <Reveal>
          <p className={styles.pullQuote}>
            We don&rsquo;t want a <em>build tool that lets you do anything</em>.
            We want a build tool that lets you do <em>the obvious thing</em>,
            in a file that anyone on the team can read.
          </p>
        </Reveal>
      </div>
    </section>
  );
}

/* ------------------------------------------------------------------
   Numbers
   ------------------------------------------------------------------ */
function NumbersSection() {
  return (
    <section className={`${styles.section} ${styles.sectionPaper}`}>
      <div className={styles.container}>
        <SectionHeader
          eyebrow="By the numbers"
          title={
            <>
              Small numbers, on <em>purpose</em>.
            </>
          }
        />
        <Reveal>
          <div className={styles.numbersGrid}>
            <div className={styles.numberCell}>
              <span className={styles.numberKicker}>Cold start</span>
              <span className={styles.numberValue}>
                <em>10</em>
                <span className={styles.numberValueUnit}>ms</span>
              </span>
              <span className={styles.numberCaption}>
                Native CLI. No JVM warmup, no daemon to wake, no Gradle daemon to forget about.
              </span>
            </div>
            <div className={styles.numberCell}>
              <span className={styles.numberKicker}>IDE import</span>
              <span className={styles.numberValue}>
                ~<em>1</em>
                <span className={styles.numberValueUnit}>sec</span>
              </span>
              <span className={styles.numberCaption}>
                IntelliJ and Metals over BSP. You hit import, you start typing.
              </span>
            </div>
            <div className={styles.numberCell}>
              <span className={styles.numberKicker}>Build files</span>
              <span className={styles.numberValue}>
                <em>1</em>
              </span>
              <span className={styles.numberCaption}>
                One <code>bleep.yaml</code>. Not a tree of <code>build.sbt</code>,
                <code>plugins.sbt</code>, <code>project/</code>, and a <code>Dependencies.scala</code>.
              </span>
            </div>
            <div className={styles.numberCell}>
              <span className={styles.numberKicker}>Lines of build code</span>
              <span className={styles.numberValue}>
                <em>0</em>
              </span>
              <span className={styles.numberCaption}>
                Custom logic lives in scripts: real programs, with a <code>main</code> and a debugger.
              </span>
            </div>
          </div>
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
      description="Bleep is a JVM build tool for Java, Kotlin, and Scala. One YAML file. Native CLI. One-second IDE imports. No code in your build, no scopes, no XML, no plugin acrobatics."
    >
      <div className={styles.page}>
        <Hero />
        <main>
          <RefusalsSection />
          <TenetsSection />
          <BuildExtensionsSection />
          <SpecimenSection />
          <RoundtripSection />
          <TestRunnerSection />
          <McpSection />
          <PullQuoteSection />
          <NumbersSection />
          <InstallCTA />
        </main>
      </div>
    </Layout>
  );
}
