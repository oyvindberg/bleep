# Personas: site walk-through

A fresh walk through the bleep marketing landing page (`bleep-site/src/pages/index.js`) and the Docusaurus docs (`docs/`) read in character as the five personas in `personas.md`. Each persona reads `/` first, then drifts where their own concerns take them. Inner monologue is in italics. The cross-persona consolidated wishlist at the bottom is the actionable summary.

---

## David — Senior Java engineer at a Fortune 500 bank

### Lands on `/` (the manifesto landing page)

*Hero says "A build tool for Java, Kotlin & Scala" — good, Java is named first. "Your build is data. Not a program." Fine, that's a real claim. I can see the four hero stats: 10ms cold start, 1s IDE import, 1 file, 0 lines of build code. Numbers are crisp. The tagline mentions BSP and MCP — I don't know what MCP is and I don't care, but BSP I've heard of.*

*Refusals section. "Things we will not tolerate." OK, who is the audience for that tone? I am a 14-year Maven veteran and I am not in the room being addressed here. Refusal 01 ("No build-as-program") name-checks `project/project/project/` and Gradle's `build/`. That's an sbt and Gradle problem. I want to know how it relates to my Maven world. The sub-text could just as easily be "everything Maven already gives you in the POM, but shorter." Refusal 04 ("No 30-second imports") aimed at IDE plugins — I use IntelliJ's Maven import and it imports in seconds. So this needles me a little. Refusal 05 ("No whole-module rebuilds") explicitly calls out Maven by name — "Change one file in a 200-class Maven module and Maven recompiles all 200" — fair, that's true, and it's the first concrete thing on this page that's actually about Maven.*

*Tenets are abstract. "Builds are data" — yes, my POM is data too, I get it. "Open standards" — they speak BSP, fine. "Native, by default" — the 10ms thing again. "Performance — bleeping performance" — the joke is fine, I roll my eyes a millimetre.*

*The Specimen YAML is a Kotlin example. I am a Java engineer. I get the message — it's terse and readable — but I want to see a Java specimen. Then the Compare grid: pom.xml is summarised as "One file. Closing tags. In 2026." That's a snide line about my entire career, and the lazy version of the comparison: my actual `pom.xml` problems aren't the closing tags, they're plugin XML, parent POM resolution, and `<dependencyManagement>`. None of which the page acknowledges.*

*The "Build extensions: two slots" section is the pitch I actually want to evaluate. "Anything a plugin does." Generate before compile, run after. OK — but the moment I think "Sonar," "Black Duck," "SBOM generation" — those are real things my org runs against my POM and the page is silent on whether bleep generates a POM or whether scanners can read a `bleep.yaml`. I'd want a section called "Your Sonatype/Sonar/Artifactory toolchain still works" and there isn't one.*

### `/docs/installing/`

*Six lines. Coursier install, then `cs install ... bleep`. There's a GitHub Action and a Nix line. Quick. No fuss. Good.*

*But — no `brew install bleep`, no apt/rpm. My laptop is locked-down and Coursier itself is a download from the internet that our security team has not blessed. I'd file a ticket to even try it. Doc doesn't acknowledge that's a real shape of organisation.*

### `/docs/tutorials/your-first-project/` (Java)

*Good, this one's labeled Java. Step 1: `mkdir`, `cd`, `bleep new myapp --lang java`. Step 2 shows the generated `bleep.yaml` (snippet from a test fixture) and explains every line. No surprises — `$schema`, `$version`, `jvm.name`, projects. The `myapp-test` project is a sibling, marked `isTestProject: true`. I notice the absence of a `<scope>test</scope>` line. The note further down — "Coming from Maven? Bleep uses `src/java/` rather than `src/main/java/` because there is no `src/test/java/` to contrast with — tests live in their own sibling project" — is exactly the explanation I want. That paragraph is the first one on the site that respects me.*

*Step 5 adds an SLF4J dep. The "Heads up" box about `::` vs `:` immediately tells me the answer to a question I would have asked. Good.*

*Step 6 is `bleep setup-ide`. Says Open in IntelliJ, don't import as Maven. Fine, but my org's IntelliJ ships pre-configured with Maven imports as the default and a corporate plugin set. I'd want a sentence like "If your IntelliJ install has a corporate Maven plugin running, you may need to disable it for this project."*

### `/docs/compared-to-other-build-tools/maven`

*This is the one I came for. Side-by-side `pom.xml` (55 lines) vs `bleep.yaml` (14 lines). The example pom is honest — it's a real single-module POM with the JAR plugin to set Main-Class. The bleep version is genuinely shorter for that case. The "Faster everything" table is the kind of comparison I'd want, though "Cold startup ~3-5s of JVM warmup before any work" is the kind of pain I've already amortised by leaving Maven in a CI job that doesn't care.*

*"Honest trade-offs" section — this is what earns my trust. It says "Plugin ecosystem... fewer of them." Says "Enterprise tooling... if your organization is built around Nexus, Artifactory, Sonar, JFrog, or a release pipeline that assumes mvn deploy, you'll need to adapt." That is the answer to the question I came in with, and it's stated without pretending. "IDE support — IntelliJ has bespoke Maven import. For bleep, IntelliJ uses BSP — fast and accurate, but a different code path." Acknowledged. "Familiarity" — also acknowledged.*

*But I notice what's not on the trade-offs list: BOM support. The Spring Boot parent / Quarkus platform thing. I scroll down — no. I'd have to find the project status page myself. That's a gap.*

### `/docs/appendix/status` (Project status)

*Found it via the sidebar. Honest list. "No BOM / `dependencyManagement` — every dependency declares its version explicitly. Spring Boot users feel this most." That's me. Game over for any new Spring Boot service. They name it; I respect that. But the first paragraph says "Several teams use it in production for backend services" — I'd want one of those teams named, and a case study link.*

### `/docs/demos/importing-maven-build`

*This is the killer feature for me — does `bleep import-maven` actually work on my repo? The doc lists what gets preserved, what gets stripped. Honest about Maven plugins beyond compile/test/jar. "Becomes your problem to re-implement as scripts." Fine — I wouldn't do this on my Spring Boot service, I might do it on a small library.*

*I like the `--ignore-when-templating` flag — that's the kind of pragmatic flag a tool that's actually been used would expose.*

### David's verdict

The site speaks to a Scala / Gradle audience that already wanted out. For a Java/Maven shop the messaging is correct but the framing is off-target — and the bear case (BOM gap, scanner integration, Artifactory creds, IntelliJ Maven plugin) is mentioned only in scattered places, never in one "Maven shop reality check" page. The Maven comparison page is the best on the site and could be the entry point for my segment.

> "Promising tool, wrong landing page for me. Send me straight to the Maven comparison and the project status, skip the manifesto."

### Wishlist

- Specimen YAML on `/` (`bleep-site/src/pages/index.js`, `SpecimenSection`, around line 277-354) is Kotlin. Add a tab/toggle for Java/Kotlin/Scala — or rotate the example by visit. Java users get sent to a Kotlin file as proof.
- Compare grid on `/` (`bleep-site/src/pages/index.js`, `compareTiles` around line 817-852) reduces `pom.xml` to "Closing tags. In 2026." — the line is a cheap shot. Replace with a real one-line characterisation that names the Maven-specific friction (plugin XML, parent POM walks).
- Refusal 04 ("No 30-second imports", `bleep-site/src/pages/index.js` around line 211-218) names IDE plugins generically. Maven's IntelliJ import is a separate code path that doesn't take 30 seconds. Either make this Gradle/sbt-specific or qualify.
- `/docs/installing.mdx` lists Coursier, Nix, GitHub Action, manual download. No `brew`, no `apt`. Add Homebrew if available and a "locked-down environment" footnote.
- `/docs/compared-to-other-build-tools/maven.mdx` is solid but doesn't mention BOM/`dependencyManagement` gap. Add a one-line trade-off bullet — currently only on the project-status page.
- No "enterprise checklist" page exists: SBOM generation, Sonar/Sonar-cloud, Artifactory auth via `~/.m2/settings.xml`, CycloneDX/SPDX output. Worth one consolidated guide rather than scattered references.
- `docs/installing.mdx` and `docs/tutorials/your-first-project.mdx` don't tell a Maven shop how to coexist with Maven (one repo with both `pom.xml` and `bleep.yaml`) during a migration trial.

---

## Priya — Staff engineer at a Series C fintech (Kotlin)

### Lands on `/` (the manifesto landing page)

*Hero says Kotlin in the meta line — second of three. Tagline mentions BSP and MCP. The 1-second import claim is what I want — Gradle is 3+ minutes on our 50-module repo and I would convert today on import speed alone if the rest holds up.*

*Refusals: I read these as somebody who has lived through Gradle Kotlin DSL hell. "No build-as-program" — yes. "No scopes" — wait, hold on. `implementation` vs `api` is actually load-bearing for a published library; if I publish `myapi` and want consumers to transitively get `commons-foo`, I need `api`, not `implementation`. The Refusal text says "if one belongs in a different context, that's a different project." OK, so the answer is split. I can buy it for an internal service; I'd worry about it for an SDK we ship. The page doesn't address this, but I'll see what the docs say.*

*"No plugin acrobatics" — `Plugin<Project>` registration, autoplugins. Yes. Yes. Yes. This is the language I speak.*

*Specimen section: it's a Kotlin example. Good — I'm the audience. Two projects, one template, twenty-four lines. "Tests are projects" — fine. "Bump Kotlin in one line." I'd test that on day one.*

*"Build extensions: two slots." Sourcegen + scripts. As a Gradle user I'd want to map this to KSP/KAPT/protobuf-gradle-plugin/avro-gradle-plugin and the answer is sourcegen for all of them. KSP is the load-bearing concern. The page doesn't say if KSP is supported; I'd find out the hard way.*

*Compare grid puts Gradle as "A Kotlin program that side-effects a build graph." That's accurate and I would say the same thing in private.*

### `/docs/installing/`

*Coursier. I know what that is. Fine.*

### `/docs/tutorials/your-first-kotlin-project/`

*Step 1: `bleep new myapp --lang kotlin`. Generates a workspace with Kotest. Step 2 shows the YAML. The generated build has `kotlin.version: 2.3.0` in template-common. A note that says "Kotlin's compiler generates a JVM class named `MainKt` for a top-level main function in `Main.kt` — that's the mainClass set in bleep.yaml" — I appreciate that the doc knows I might forget the `Kt` suffix.*

*Step 5 adds a logging dep using `:` (single colon for Java/Kotlin) — fine.*

*"Compiler plugins" section: lists `spring`, `jpa`, `allopen`, `noarg`, `serialization`. That's the standard set, all there. I'm impressed by this.*

*But: no mention of KSP. No mention of kapt. The Gradle question I came in with is unanswered on the tutorial page.*

### `/docs/usage/annotation-processing`

*I navigate here looking for KSP. The page opens with "Bleep supports Java annotation processors" and a callout: "Kotlin annotation processing (KSP / KAPT) is not yet supported." I scroll down to the "Kotlin: KSP / KAPT" section. Says "Status: not yet supported." Then explains what works (Spring/jpa/allopen/noarg/serialization), what's a sourcegen-as-replacement story, and what does not work today: Spring Modulith, Koin's KSP DI verification, Room (we don't use it but I know teams that do), Hilt, "anything that depends on `ksp(...)` or `kapt(...)` declarations in Gradle."*

*The recommendation: "If your build requires KSP / KAPT today, don't migrate that module yet."*

*That's an honest answer that I respect. It's also a hard wall for any team using Spring Modulith verification or kotlinx.serialization's KSP variant. For my team, kotlinx.serialization with the compiler plugin works — so I'm OK. For one of our shared libs that uses Koin KSP, I'd defer that migration. Fair. The page does its job: I have a clear go/no-go answer.*

### `/docs/concepts/cross-building`

*I went here expecting to find the Kotlin Multiplatform story. There's a "Kotlin Multiplatform (KMP)" subsection at the bottom. JVM + JS works via the same `cross:` mechanic. Native is "partially supported." `kotlin-multiplatform` Gradle-plugin features that don't map to bleep's declarative model are out of scope.*

*A worked KMP `expect`/`actual` example with platform-specific dependencies "is on the roadmap" — the source-layout option exists but the example doesn't. That's a half-shipped state. For me on the backend side this is fine. For a team that ships a backend + a JS dashboard from the same Kotlin code (which is exactly what I want for our internal admin tool), this is a yellow flag.*

### `/docs/concepts/scripts` and `/docs/tutorials/your-first-script-kotlin`

*"Scripts and source generation" — the dividing line is whether `compile` needs the output. Crisp. I get it.*

*The Kotlin script tutorial: extend `bleepscript.BleepScript`, register in `bleep.yaml`, run `bleep hello`. Total surface: one class with a `run()` method, three constructor args (`Started`, `Commands`, `args`). I write a `buildSrc` convention plugin once a quarter; this looks like 1/10 the boilerplate. The fact that I can right-click → Run in IntelliJ to debug it — that alone would have saved me 8 hours last month.*

*One thing: my convention plugins do some fancy things like "register a task that runs after every test task with a specific tag." The script model doesn't appear to have a hook into the build lifecycle; if I want my logic to run after every `bleep test`, I'd write a wrapper script that calls test and then does the post-step. That's actually fine — it's just more explicit than the Gradle equivalent — but the docs don't surface this pattern explicitly.*

### `/docs/usage/mcp-server`

*Curious — agents using my build tool? The pitch is concrete: 18 structured tool calls, in-process against the BSP daemon, sub-100ms once warm, compact JSON summaries with diff vs the previous run. The rationale section ("Why these design choices") earns trust — explains compactness over completeness, why errors stream while results summarise. Our team is leaning into Claude Code; if a build tool ships first-class MCP support, that's a real differentiator I can put on a slide for our platform standardisation deck.*

### Priya's verdict

Fast, declarative, the Kotlin-first messaging holds up in the Kotlin tutorial. The KSP gap is honestly stated and is a hard wall for two of our seven services. KMP is on the roadmap shape that I can plan around. The script model is the thing I most want to demo to my team — convention plugins were the part of Gradle I disliked most, and the bleep alternative looks like 1/10 the friction.

> "I'd ship a side project on this tomorrow. The blocker for the main repo is KSP, and they know it."

### Wishlist

- `/` (`bleep-site/src/pages/index.js`) doesn't mention KSP/KAPT status in the Gradle pitch. A tiny "what's not yet here" sentence under the Refusals or a link from the Compare-Gradle tile would save a Kotlin user one click.
- `/docs/tutorials/your-first-kotlin-project.mdx` (lines 115-127, "Compiler plugins") lists the supported compiler plugins. Add a one-line "KSP/KAPT not yet supported — see annotation-processing" so a Kotlin user finds this *before* they build a project that needs it.
- `/docs/concepts/cross-building.mdx` (KMP section, lines 239+) admits the KMP `expect`/`actual` worked example is "on the roadmap." For someone evaluating bleep for KMP, this is exactly the example needed. Promote shipping it.
- `/docs/concepts/scripts.mdx` doesn't show a "this is the bleep equivalent of a Gradle convention plugin" recipe. Add a worked example.
- The Refusal section on `/` doesn't address `api` vs `implementation`. A library author worries about transitive exposure; the answer ("split into projects") deserves a sentence somewhere, not silence.
- `/docs/usage/mcp-server.mdx` is well-written but the marketing landing page treats MCP as one of the two open standards. Worth its own card on the front page among the Tenets — currently it's a separate later section, easy to scroll past.

---

## Marcus — Junior backend dev at a Series A startup

### Lands on `/` (the manifesto landing page)

*"Your build is data. Not a program." OK, I think I get it — like my POM but in YAML? The hero subtitle says "BSP" and "MCP" — I don't know what those are. The four numbers are punchy: 10ms, 1 sec, 1 file, 0 lines. I don't know what 0 lines of build code means. Zero is good?*

*Refusals. The first one says "Turing-complete build files are how you get sbt's `project/project/project/` regress — a build that compiles to compile a build, recursively — or a Gradle `build/` folder with seven layers of indirection." I have heard of sbt and Gradle but I have no idea what `project/project/project/` is or why it's bad. I feel like I'm being told an inside joke at a party where I'm new. The card is also kind of mean? It's setting me up to think bleep is for grown-ups who have been burned by stuff I haven't been burned by.*

*Refusal 02 ("No scopes"): names `compile`, `provided`, `runtime`, `Test/test/it/Compile`. I know `<scope>test</scope>` from my POM. I don't know what `Test/test/it/Compile` is. Mostly OK — the punchline that "tests are a project too" is something I can hold on to.*

*Refusal 05 about Maven recompiling 200 classes when I change one — wait, does it really do that? My CI is 6 minutes, maybe that's why. OK that's interesting.*

*Tenets are dense. "Open standards" — BSP, again, no explanation. "Native, by default" — I get this one, it starts fast. "Performance — bleeping performance." Cute but I'm not the audience for the cleverness.*

*Specimen YAML — Kotlin. I'm a Java guy but the file is short enough I can read it. Two projects, one template. The notes on the right are friendly. The line "Code review tells you exactly what changed — in plain text, with no graph viewer" lands; my last build PR was reviewed by six people and we all squinted at it.*

*The Compare grid. pom.xml is "One file. Closing tags. In 2026." That's a roast of what I work in every day. I don't know whether to be embarrassed or amused.*

### `/docs/installing/`

*Coursier — I don't have it. Click through to "install coursier CLI" and now I'm on a different doc site for a different tool. Not the worst, but it adds friction. I'd rather see `brew install bleep` or even `curl ... | sh`.*

*Wait — the front page has `curl -fsSL https://bleep.build/install | sh` in the Install CTA but the install docs page doesn't mention that command. Inconsistent.*

### `/docs/tutorials/your-first-project/` (Java)

*Step 1: `mkdir`, `cd`, `bleep new myapp --lang java`. Step 2 shows the generated `bleep.yaml`. I read it; it's short. The bullets explain every line. "Bleep auto-infers a template" — I don't know what auto-infers means in this context but I see the template and the projects extending it. OK.*

*Step 3: `bleep run myapp` → "Hello, World!". Step 4: `bleep test myapp-test`. Step 5 adds SLF4J. The note about `::` vs `:` is helpful — I would've gotten that wrong.*

*Step 6: `bleep setup-ide`. Says open in IntelliJ. We have Community at work. The doc only mentions IntelliJ IDEA without specifying Ultimate vs Community. There's a VS Code with Metals option but Metals is "Scala-focused" — am I supposed to use it for Java? The wording leaves me uncertain.*

*Project structure section: `src/java/...` (no `main/` segment). The "Coming from Maven?" callout explains why. Good — that would have confused me otherwise.*

*Next steps: "Write your first script." Click.*

### `/docs/tutorials/your-first-script` (Java)

*Snippet of `bleep.yaml` adds a `scripts` project depending on `bleepscript`. Then a `HelloScript.java` that extends `bleepscript.BleepScript`. Run `bleep hello`. It prints `This build has 2 projects` and `Hello, world!`. That's... legitimately easy. I get it. I could write one of these.*

*"`BleepScript` provides the `main` method via reflection, so you don't need any explicit `public static void main` boilerplate." OK, slight magic, but it's spelled out.*

### `/docs/tutorials/basic-usage`

*Cheat sheet of every command: `compile`, `test`, `run`, `projects`, `fmt`, `clean`. Mentions `--watch` and `--diff-watch`. This is the page I want bookmarked. It's short. Feels like the tool is ten verbs, not a hundred.*

### `/docs/appendix/status` (Project status)

*"No BOM / `dependencyManagement`." I don't know what BOM is. I do know Spring Boot Parent — that note says "Spring Boot users feel this most." We use Spring Boot. So... if I migrated, I'd have to type out 80 dependency versions by hand that the parent gives me for free? My senior would veto that in 3 seconds.*

*"Plugin ecosystem is small compared to Maven/Gradle/sbt." OK. I don't know what plugins we use beyond Spring Boot Maven Plugin. Maybe the Sonar one? I'd ask my senior.*

### Marcus's verdict

The tutorial page is genuinely friendly — I could follow it on a Saturday and ship a hello-world. The marketing landing page talks past me. Multiple references in the manifesto assume I've been bitten by sbt/Gradle, which I haven't. The honest dealbreaker is the Spring Boot parent BOM gap, which I wouldn't know was a dealbreaker until my senior pointed it out.

> "Tutorial was fun. Marketing page wasn't for me. Don't think we're switching, but I'd play with it on a personal project."

### Wishlist

- `/` (`bleep-site/src/pages/index.js`) Refusal 01 (around lines 172-183) name-drops `project/project/project/` — referenced as if every reader knows the joke. Either explain in one line or cut.
- `/` Tenets and Refusals reference `BSP` and `MCP` without expanding them on first use. Add the expansion on first appearance.
- `/docs/installing.mdx` and the front-page Install CTA (`bleep-site/src/pages/index.js`, `InstallCTA` around line 907-962) disagree: the CTA shows `curl | sh`, the install page doesn't. Reconcile.
- `/docs/tutorials/your-first-project.mdx` Step 6 (around lines 102-123) doesn't say IntelliJ Community is fine vs requires Ultimate. Add that.
- A "Migrating a Spring Boot service" page would be the single most-Googled piece of bleep content for a junior at a Spring Boot shop. Currently the BOM gap lives only on the appendix status page; surface it earlier and call out Spring Boot specifically.
- The manifesto-shaped front page is striking but for a junior reader it reads as a tool aimed at someone else. A "Why bleep, in three sentences for normal people" callout near the top would help.
- The compare grid summary of `pom.xml` ("Closing tags. In 2026.") is funny if you're an old hand and embarrassing if you're a junior who writes XML for a living. Reword.

---

## Anya — Principal engineer at a quant fund (Scala/sbt)

### Lands on `/` (the manifesto landing page)

*Hero: "A build tool for Java, Kotlin & Scala." Scala is third. Notable. The 1-second IDE import claim — I have spent literal weeks of my career waiting for sbt project loads. I'm in.*

*Refusals: 01 ("No build-as-program") references `project/project/project/` — yes, I know that path intimately, I have written code that lived there. 02 ("No scopes") — wait. `Compile`, `Test`, `IntegrationTest`, `Compile / scalacOptions` — these are not ceremony, these are how I scope settings in 30-plugin sbt builds at work. The Refusal text says "if one belongs in a different context, that's a different project. Tests are a project too." I'm not convinced — I have a real workflow that uses `Test / fork := true`, `inConfig(IntegrationTest)(Defaults.testSettings)`, and a custom config axis for `Bench`. "It's a different project" is an answer; whether it's the right answer for my codebase is a real question. The page is breezy about something I'd want to study carefully.*

*Refusal 03: "no autoplugins, no requires graphs, no Plugin&lt;Project&gt; registration." I have written ~30 in-house autoplugins. I know what they cost and what they pay back. The pitch is honest about the cost; it underrates the payback.*

*Tenets: "Round-trip, always." `update-deps`, `project-rename`, `templates-reapply`, "the model on disk and the model in memory are the same model." This is the tenet I personally find the most interesting. sbt has nothing like this — `sbt-updates` is a plugin that prints recommendations, and there's no equivalent of `templates-reapply` at all. I sit up.*

*Specimen YAML is Kotlin. Hmm — I am the Scala persona on a site whose specimen is Kotlin. Either the team picked the most universally-readable example or they're signalling that Scala is no longer the primary audience. I parse the YAML in 4 seconds; it's fine.*

*"Build extensions: two slots." Sourcegen + scripts. My instinct: where's `sbt-jmh`? `sbt-protoc`? `sbt-buildinfo`? `sbt-scoverage`? `sbt-explicit-deps`? I can imagine each being either a sourcegen script (buildinfo, protoc, scoverage's instrumentation) or a regular script (jmh, explicit-deps as a linter). The page says "Anything a plugin does." I'd believe it for 80% of the plugins I use.*

*Compare grid. sbt is "Three files. An impenetrable DSL with global state." Three files — `build.sbt`, `project/build.properties`, `project/plugins.sbt`. Plus `Dependencies.scala`, `Common.scala`, etc. that any non-trivial sbt project ends up with. "Impenetrable" is rude, but accurate to a junior; veterans like me have stopped noticing.*

*Pull quote: "We don't want a build tool that lets you do anything. We want a build tool that lets you do the obvious thing, in a file that anyone on the team can read." This is the philosophical pitch. I respect it. It's also the position of the person who has stopped optimising for the 5% case to make the 95% better. As an sbt-plugin author I am in the 5% case.*

### `/docs/installing/`

*Coursier — yes, I have it. `cs install ... bleep`. Done.*

### `/docs/tutorials/your-first-scala-project/`

*Defaults to Scala 3 on the JVM. `bleep new myapp` (no flag). Generates a project with MUnit. Build file looks normal — `template-common` carries `scala.version` and compiler options. Step 5 adds `com.lihaoyi::fansi:0.5.0` — `::` for Scala, fine. The cross-building scaffold (`bleep new mylib --scala 213 --scala 3 --platform jvm --platform js`) is a one-liner — that's exactly the affordance I'd want.*

### `/docs/concepts/cross-building`

*This is the page I came to scrutinise. `cross:` block per project, each entry overrides what's specific to that variant. Looks reasonable. The per-cross customization section — `source-layout: cross-pure`, per-variant `dependencies`, per-variant `scala.options` — all the things I do in sbt. Good.*

*"A common gotcha: `extends:` at the cross level extends templates, not the parent project." This is the kind of detail that bites in practice and the doc is calling it out. Earned trust.*

*"Cross dependencies" section: `app@jvm3` depends on `core@jvm3`, and `app@js3` depends on `core@js3`. Automatic cross-id matching. That's exactly the sbt `%%` semantics carried over. Good.*

*"Publishing Cross-Built Libraries" — `mylib_2.13`, `mylib_3`, `mylib_sjs1_3`. Same Maven coordinates as sbt. Critical for compatibility with consumers using either tool. Good.*

### `/docs/tutorials/publish-to-maven` and `/docs/usage/dependencies`

*Publish flow: `bleep publish setup` walks me through PGP keys, then `bleep publish sonatype`. dynver from git tags. Sonatype Central Portal supported (says so explicitly). The GitHub Actions workflow at the bottom is what I need.*

*One concern: I rely on `sbt-ci-release`'s sentinel-based release flow where a tag triggers publish-and-release. The bleep version has `--assert-release` which will refuse to publish a snapshot — close but not the same shape. I'd need to dig into whether the Sonatype staging-close-and-release is automatic. The doc says "Sonatype Central, which closes and releases the staging repository on success." OK — that's the sbt-ci-release behavior, baked in. Good.*

*Dependencies page is excellent. The library-version-schemes section is *literally* the sbt eviction-warning model — `early-semver`, `semver-spec`, `pvp`, `strict`, `always` — same names, same semantics, "because the underlying check is the sbt code" liberated into bleep-nosbt. That's the answer I needed without asking.*

### `/docs/porting-sbt-plugins`

*This is the page I want to see if the bleep team is realistic. The translation table maps `lazy val fooSetting` → constructor parameter, `lazy val fooTask` → `def`, `sbt.Logger` → `ryddig.Logger`. That's the real translation. The DynVerPlugin source is shown in full as a worked example — I can read the whole port end-to-end. Liberated plugins listed: sbt-ci-release, sbt-sonatype, sbt-dynver, sbt-pgp, sbt-native-image, mdoc, sbt-scalafix, sbt-jni. That's the release-pipeline set, plus mdoc and scalafix. No `sbt-jmh`, no `sbt-protoc`, no `sbt-buildinfo`, no `sbt-explicit-deps`.*

*"Limitations" section is honest: scopes don't exist, global mutable state has to become explicit parameters, multi-project semantics differ. The page is doing what it should — letting me decide whether my plugins port cleanly without overselling.*

### `/docs/compared-to-other-build-tools/sbt`

*"What sbt does that bleep doesn't" — scoped task customization, `scripted`, mature plugin ecosystem, `reload` flow. Listed without flinching. "When sbt is still the right answer" — the niche-plugin and shell-as-environment cases. I'd reach my own conclusion: my release pipeline ports cleanly, my JMH benchmarks would need a script-port (writeable in an afternoon), my custom IntelliJ-debug-and-profile flow against sbt-shell is gone.*

### Anya's verdict

The Scala story is honest, the cross-building parity is real, the publish flow ports my exact pipeline, and the dependency-eviction-scheme machinery is literally my sbt code. The plugin-port catalogue is short but covers releases. The 30-plugin sbt builds I maintain at work would each need 2-3 plugin ports per project — a real but bounded amount of work. I'd try this on one library first, with `sbt-explicit-deps` as my first port to write.

> "I'd port a small library this weekend and decide. The roundtrip-mutate command surface is more interesting than the speed pitch."

### Wishlist

- `/` (`bleep-site/src/pages/index.js`) Specimen YAML is Kotlin. Either rotate language by visit, add a tab, or add a second specimen for Scala. The Scala persona reads it as deprioritisation.
- `/` Refusal 02 ("No scopes", around lines 184-196) is dismissive of a real sbt power-user workflow. Say something like "scopes you can't replace with separate projects: ~5% of sbt builds, see [page]" — currently silence, which makes the refusal seem unaware.
- `/docs/porting-sbt-plugins.mdx` lists ported plugins — extend with a "your plugin not here? Here are the patterns" section. Currently lists patterns as "Limitations" only.
- A page or section titled "sbt-X equivalents" listing the most-used 30 sbt plugins and what to do for each (already ported / port pattern in scripts / no equivalent) — currently a reader has to puzzle this out.
- `docs/concepts/cross-building.mdx` "What's not yet supported" subsection sits *only* under the KMP heading. The Scala cross story has its own gaps (no `sbt-projectmatrix`-style fully arbitrary axes); document them adjacent, not just the Kotlin gaps.
- The `bleep build` mutating commands (`update-deps`, `project-rename`, `templates-reapply`) get one bullet on the manifesto Round-trip section. They deserve a dedicated page or guide — these are the most differentiated thing bleep has and they're hidden in the CLI reference.
- `/docs/tutorials/publish-to-maven.mdx` is solid but doesn't explicitly address an `sbt-ci-release` migration path (use these env-var names, replace this workflow). Section title would be "Coming from sbt-ci-release."

---

## Tariq — Engineering manager at a polyglot SaaS

### Lands on `/` (the manifesto landing page)

*"A build tool for Java, Kotlin & Scala" — Java first, three named, equal billing. That's the line I've been waiting for. I have three teams using three tools and the org cost is real.*

*Hero subtitle calls out BSP and MCP. MCP is interesting — our platform team is evaluating Claude Code adoption company-wide; if a build tool is first-class for AI agents, that's a slide in the standardisation deck.*

*Refusals: each one is a real production-cost line my teams hit — sbt's project/project, Gradle's daemon, Maven's whole-module rebuild. Reads as battle-tested.*

*Tenets are abstract but the one that matters for me is "Open standards" — BSP. If we standardise on bleep, every team's IDE story is the same. That's a non-trivial reduction in onboarding friction across teams.*

*Specimen YAML is Kotlin. For me as an EM, the language doesn't matter — what matters is "this file is short and any of my engineers can read it." It is and they could.*

*Build extensions section: "two slots." Codegen + scripts. My Scala data team uses sbt-protoc and sbt-buildinfo. My Kotlin team uses kapt for some Spring stuff. My Java team uses Maven plugins for shade and SBOM. The page promises "anything a plugin does." I'll need to check the gaps.*

*MCP section: "more than one agent will be running at once, tokens are scarce, any tool that takes seconds blocks every agent attached to it." That's the realistic future-state for my org. Worth a deeper look.*

*Compare grid is the slide I want — same project, four tools, one of them is plain text. I can imagine showing this to my CTO.*

### `/docs/installing/`

*Coursier-based. Fine for the engineers; my Linux ops would prefer apt/rpm. We can package internally if needed.*

### `/docs/tutorials/your-first-project/` (Java) and `/docs/tutorials/your-first-kotlin-project/` and `/docs/tutorials/your-first-scala-project/`

*Three sibling tutorials, parallel structure. Each: prereqs, six steps, generated `bleep.yaml` snippet, `bleep run`, `bleep test`, add a dep, `bleep setup-ide`, project structure. The three commands are identical across languages: `bleep run`, `bleep test`, `bleep setup-ide`. That's the consistency I want for cross-team parity. If a Kotlin engineer rotates to the Scala team, the muscle memory carries over.*

*The fact that Kotlin and Scala both reach for IntelliJ via BSP is notable: same import path, same setup ritual. Same thing for VS Code with Metals (with the Kotlin extension as an addon).*

### `/docs/appendix/status`

*Honest list. Three entries hit my org:*
- *No BOM / `dependencyManagement`*: my Java teams use Spring Boot Parent and Quarkus Platform. Hard wall.
- *No KSP / KAPT*: my Kotlin team uses kapt for Spring annotations. Hard wall on those projects.
- *No Gradle import*: one of my three teams (Kotlin/Gradle) has no automated import path. Manual port required.

*Three teams, three blockers. Different blockers per team but each team has at least one. The page tells me bluntly that I cannot do a single-quarter all-teams migration. I can do a single-team trial, probably the Scala data team given Anya's profile.*

### `/docs/compared-to-other-build-tools/maven`, `/docs/compared-to-other-build-tools/gradle`, `/docs/compared-to-other-build-tools/sbt`

*The three pages share the same shape: side-by-side, "Faster everything" table, multi-module section, plugin-and-lifecycle, honest trade-offs. Symmetry is what I want — I can put all three in front of three team leads and they'll each find the page that speaks to them.*

*The Maven page mentions enterprise tooling Sonar/Artifactory; the sbt page mentions the niche-plugin reality; the Gradle page mentions Android (we don't ship Android, so this isn't blocking) and KMP. Each acknowledges what it can't do.*

### `/docs/usage/mcp-server`

*This is a differentiator for me. None of Maven, Gradle, or sbt has anything like this. If I'm building the case to standardise on one tool across three teams, "first-class AI-agent integration" is a multi-year strategic argument. The doc is concrete — 18 tools, in-process against the BSP daemon, sub-100ms once warm. Compact-by-default JSON summaries with diff vs previous run is exactly the design principle I'd hope for.*

### Tariq's verdict

The "first-class for Java, Kotlin, and Scala" promise holds at the docs level — three sibling tutorials, identical command shape, identical IDE story, identical script model, identical publish flow. The blockers are real (BOM gap → Java, KSP → Kotlin, no Gradle import → Kotlin again), and the docs name them honestly. I cannot do a one-quarter migration of my org. I can do a per-team trial, starting with the Scala data team. The MCP-server story is the strongest strategic argument for centralising.

> "Polyglot story is the right framing. Migration is a multi-quarter project, not a sprint, and the docs are clear about why."

### Wishlist

- The "Java, Kotlin, Scala equal citizens" claim isn't visible on the front page beyond the meta line. A Tenet card titled "Three first-class languages" with a one-line each on what bleep does for each would carry the polyglot pitch.
- `/docs/appendix/status.mdx` lists gaps. The Java, Kotlin, and Scala-specific gaps are mixed in one bullet list. Group by language so a team-lead can quickly see "what's blocking my team."
- No "migration playbook" page exists for organizations with multiple build tools. Add a guide called "Adopting bleep team-by-team" with a recommended order (greenfield → Scala/sbt teams → Maven teams → Gradle/Kotlin teams).
- The "no Gradle import" gap (`docs/appendix/status.mdx`) is mentioned in one line. For a polyglot shop this is the largest single migration gap. Expand into its own page with manual-port patterns.
- `/docs/usage/mcp-server.mdx` is an underweighted strategic differentiator. Promote it from "for AI agents" sidebar entry to a Tenet-level callout on `/`.
- The CI/CD setup guide (`docs/guides/ci-cd-setup.mdx`) shows a single-job example. For a polyglot org with multiple monorepos / multiple repos, a "shared CI templates across teams" pattern would be valuable — currently absent.

---

## Cross-persona consolidated wishlist

Sorted by how many personas would benefit. Each item references a specific file/section.

### 5/5 — every persona

1. **Front page Specimen YAML is Kotlin-only.** `bleep-site/src/pages/index.js`, `SpecimenSection` (lines ~277-354). David sees Java second-class; Marcus is a Java dev shown a Kotlin file; Anya reads it as Scala deprioritisation; Priya is the only persona it serves directly; Tariq notices because his polyglot pitch needs three. Add a language toggle, rotate per visit, or show all three side by side.

2. **`/docs/appendix/status.mdx` is the most-clicked page in the docs but is buried in Appendix.** Every persona arrives there asking "what doesn't work yet?" and finds it via search or the Maven/Gradle/sbt-comparison page footer. Promote into the Getting Started sidebar (after Installation), or surface a banner-link from `/`.

### 4/5

3. **Front page never expands `BSP`/`MCP` on first use.** `bleep-site/src/pages/index.js` Hero (line ~110-114) and Tenets (lines ~446-454, ~654-728). Marcus doesn't know what BSP is; David has heard of it but not MCP; Tariq knows MCP because his org uses Claude Code; Anya/Priya know both. First mention should expand each acronym in plain English.

4. **The `bleep build` round-trip subcommands (`update-deps`, `project-rename`, `templates-reapply`, `normalize`, `diff effective`) are the most differentiated capability bleep has and have no dedicated guide.** They appear only in the auto-generated CLI reference under `docs/reference/cli/build/*` and as bullets on `docs/tutorials/basic-usage.mdx` (lines 92-101). Anya wants this; Priya would too once she sees it; Tariq's platform team would script against it; David would recognise it as "what we wish Maven `versions:set` could do." Write a Concepts or Guides page titled "Round-trip your build."

### 3/5

5. **No "migration playbook" or per-tool-of-origin landing.** David wants Maven-shop reality check; Tariq wants per-team migration order; Marcus would be pointed by his senior. Currently each comparison page (`docs/compared-to-other-build-tools/{maven,gradle,sbt}.mdx`) has a "Migrating from X" section but no per-tool playbook with timelines, gotchas, what-breaks-first.

6. **Spring Boot BOM / `dependencyManagement` gap is mentioned only on `docs/appendix/status.mdx` and `docs/usage/dependencies.mdx` (line 264 onward).** David, Marcus, and Tariq all have Spring Boot in scope and only find this on a third-tier page. Add a "Spring Boot users" callout to `docs/compared-to-other-build-tools/maven.mdx` and to the Java tutorial.

7. **The Compare grid copy on `/` (`bleep-site/src/pages/index.js`, `compareTiles` lines 817-852) reads as snarky to readers who write the maligned tools daily.** "One file. Closing tags. In 2026." (Maven) and "An impenetrable DSL with global state" (sbt) are funny if you're already converted, alienating if you're not. Soften without losing teeth.

### 2/5

8. **Tutorials drift in parallel-language-coverage.** The three "Your First Project" tutorials are well-aligned; the three "Your First Script" tutorials are aligned; the three "Your First Sourcegen" tutorials reference snippets that don't exist (the build is failing on them). Get the sourcegen tutorials shipping or remove from sidebar — currently in the Tutorials section, lines 149-161 of `bleep-site/sidebars.js`.

9. **KSP/KAPT gap discovery requires three clicks.** `docs/usage/annotation-processing.mdx` has the answer but a Kotlin user starts at the Kotlin tutorial (`docs/tutorials/your-first-kotlin-project.mdx` lines 115-127, "Compiler plugins") which mentions only the supported set without a "what's missing" link. Add a one-line forward reference.

10. **`docs/concepts/cross-building.mdx` "What's not yet supported" only appears in the KMP section.** Anya wants Scala-side cross-build gap honesty; Priya wants Kotlin-side. Either move the "What's not yet supported" pattern up to the page level or duplicate for Scala.

### 1/5

11. **No "Coming from sbt-ci-release" recipe in `docs/tutorials/publish-to-maven.mdx`.** Anya specifically wants this — a 5-line section showing the env var name correspondence between sbt-ci-release and bleep publish-sonatype.

12. **No `brew install bleep` (or `apt`).** David's locked-down org and Marcus's casual install both want a one-line install. `docs/installing.mdx` lists Coursier, GitHub Action, Nix, manual. The front page CTA shows `curl | sh`; the install page doesn't. Reconcile and add Homebrew if available.

13. **The script model has no "convention plugin equivalent" recipe.** `docs/concepts/scripts.mdx` and the script tutorials don't show a "this is what your Gradle convention plugin / sbt autoplugin looks like as a bleep script." Priya would want this; Anya would want it for autoplugin equivalents.

14. **No "enterprise checklist" page (SBOM, Sonar, Artifactory, license scanning).** David's segment lives or dies on this. Each item is mentioned somewhere but never together.

15. **`/docs/installing.mdx` and `bleep-site/src/pages/index.js` `InstallCTA` (line ~907-962) disagree on the install command.** Front page says `curl -fsSL https://bleep.build/install | sh`; install doc says `cs install ...`. Either is fine; both should agree.
