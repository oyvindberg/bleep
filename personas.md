# Personas

Working document. Five composite personas to test product decisions and
documentation choices against. Mix of seniority, company shape, and
language/ecosystem allegiance. Names are arbitrary.

---

## 1. David — Senior Java engineer at a Fortune 500 bank

**Quick stats**

- 14 years experience
- Tech Lead on the backend platform team
- ~10,000 engineers company-wide; thousands of JVM services
- Daily tools: Java 21, Spring Boot, **Maven**, Jenkins, Artifactory,
  IntelliJ IDEA Ultimate, SonarQube, Checkmarx, Black Duck

### Profile

David has run Maven builds for over a decade. He can read a
multi-module `pom.xml` like English. He's been the person other juniors
came to with `<dependencyManagement>` scope questions. His build is a
six-line copy-paste away from any other Maven build at the company,
which is exactly how he likes it. His CI takes 18 minutes; a cold local
build takes 4. The build itself is rarely the bottleneck — the
*release pipeline* is.

He's been told three times by enthusiastic juniors that "Gradle is
faster" and his answer is always: "Gradle has a daemon, Maven doesn't,
and our security team doesn't trust either of you."

He has heard of Scala. He has not heard of Kotlin in any serious way
beyond Android, which is "not his department".

### What he wants from a build tool

- **Stability over novelty.** A 5-year-old `pom.xml` had better still build.
- **Read-the-source obviousness.** Every line of build config should be
  greppable from another team's repo without context.
- **Plugged-in security scanning.** SonarQube, Checkmarx, Black Duck,
  SBOM generation, CVE feeds — all of these read `pom.xml`.
- **Enterprise repository auth.** Artifactory with corporate credentials
  via `~/.m2/settings.xml`.
- **BOM-managed dependencies.** Spring Boot starter parent. Quarkus
  platform. Without that, every service repo has to track 80+ versions
  by hand.

### Bull case

- `bleep import` reads his `pom.xml` and produces a working build —
  including the multi-module hierarchy, `dependencyManagement`, and the
  Spring Boot main class. He can keep his existing CI step that runs
  `mvn package`.
- Native binary means CI Maven steps shave 30s of JVM warmup × 4 stages
  × 100 services / day.
- Generated `pom.xml` so security scanners don't change.

### Bear case

- "I can't get our Artifactory creds to resolve in the new tool" — game
  over.
- "Sonar plugin doesn't recognize the new build format" — game over.
- BOM support not first-class. Without it he'd be writing 80 explicit
  versions per service.
- Anything that breaks his IntelliJ workflow. He uses native Maven
  import, not BSP.

David is **the hardest to convert** — but the most populous. If bleep
wins David's category, it's a category leader.

---

## 2. Priya — Staff engineer at a Series C fintech

**Quick stats**

- 8 years experience, deep Kotlin
- Staff Engineer on the platform team
- ~200 engineers, B2B SaaS / payments
- Daily tools: **Kotlin** (almost exclusively), Spring Boot + Quarkus,
  Ktor for internal services, **Gradle Kotlin DSL**, GitHub Actions,
  IntelliJ IDEA, Datadog, GitHub Packages

### Profile

Priya has lived through the Groovy → Kotlin DSL migration and has
opinions. She wrote her team's `buildSrc` and a handful of convention
plugins. She knows what `Configuration.api` vs `implementation` means
without thinking. She also knows that Gradle's daemon eats 4GB of her
laptop's RAM and that "config cache invalidates on a Tuesday for no
reason" is a meme in her Slack.

She's tried Bazel for a hot minute and stopped because "the tooling
isn't there for Kotlin yet". She's interested in Kotlin Multiplatform
for shared code between her backend and a small JS dashboard but the
Gradle KMP plugin makes her tired.

She'd happily kill the Gradle daemon if there were anything else.

### What she wants from a build tool

- **Build speed**, especially IDE imports. Their 50-module repo takes
  3+ minutes to import in IntelliJ; she'd kill for sub-1-second.
- **Less DSL.** Gradle Kotlin DSL is "fine" but she's tired of learning
  the difference between `dependencies {}` (configuration block) and
  `Configuration` (Java API).
- **First-class KMP** — JVM + JS shared targets without 200 lines of
  setup.
- **Ergonomic publishing.** GitHub Packages auth + Maven Central with
  one config.
- **Compiler plugin support** — KSP, kotest, kapt, ksp's
  generator-of-the-month.

### Bull case

- Bleep's BSP-driven IDE import does 50 modules in ~1 second. That's a
  10× IDE iteration win.
- Kotlin compiler plugins are first-class (`compilerPlugins:` field)
  with the standard set (allopen, jpa, spring, noarg, serialization)
  shipped.
- Build is data, not code. No buildSrc. No configuration cache. No
  daemon.
- Cross-building Kotlin/JVM + Kotlin/JS works in one `cross:` block
  with a shared `Main.kt`.

### Bear case

- KSP not yet wired in: she'd hit a wall the first day. (Currently a
  known gap.)
- Missing kapt for legacy annotation-processor-based libs.
- "How do I write a script that does what my convention plugin did" —
  if scripts can't replace her existing build logic, no migration.
- No Android target. (Bleep doesn't target Android — fine for her
  backend work, hard limit if her team also owns the Android client.)

Priya is **early-adopter-shaped**: she'll try it on a side project and
talk about it on her team if it sticks.

---

## 3. Marcus — Junior backend dev at a Series A startup

**Quick stats**

- 1.5 years out of college
- Software Engineer
- ~12 engineers, building a B2B SaaS API
- Daily tools: Java 21, Spring Boot, Maven, GitHub Actions,
  **IntelliJ IDEA Community**, Postgres, Stack Overflow

### Profile

Marcus learned Maven from his bootcamp's Spring Boot tutorial. He can
add a dependency confidently. He has *not* had to think about
`<dependencyManagement>` because Spring Boot Parent does it for him,
and he's slightly afraid of the day he has to. His CI is 6 minutes,
which feels fine. He has never heard of Bazel, has heard of Gradle but
"only on Android".

When the build broke last quarter — somebody added a plugin that
needed `<configuration>` he didn't understand — he asked the senior
engineer in Slack. The fix took 4 hours and he still doesn't know what
Maven did.

He cares about shipping his ticket. The tool should get out of his way.

### What he wants from a build tool

- **It just works** when he clones the repo and opens IntelliJ.
- **Errors he can act on.** "Could not resolve foo" is fine. A 200-line
  Java stack trace from the build tool is not.
- **A getting-started page that doesn't assume Maven knowledge.**
- **A single command** to add a dependency, run tests, run the app.
- **A trustworthy senior** who tells him this is what they're using
  now.

### Bull case

- Bleep's Your-First-Project (Java) tutorial holds his hand from
  `mkdir` to `bleep run`. He copies and pastes and it works.
- `bleep test` runs the tests. `bleep run` runs the app. There's no
  `mvn package -DskipTests=false -P prod` ceremony.
- Errors quote the `bleep.yaml` line that's wrong, not a Java stack
  trace.

### Bear case

- His tech lead says "we're staying on Maven" and the conversation is
  over.
- Stack Overflow has 200,000 answers about Maven errors and 3 about
  bleep — when something breaks, he can't paste the error and find a
  fix.
- IntelliJ Community has weaker BSP support than Ultimate; if his
  laptop ships with Community he might hit edge cases nobody warned
  him about.

Marcus is **a leading indicator** — he's the user every senior
ostensibly mentors. If juniors find bleep approachable, the seniors
notice.

---

## 4. Anya — Principal engineer at a quant trading firm

**Quick stats**

- 11 years experience, deep Scala
- Principal Engineer
- ~40 engineers, several large Scala codebases
- Daily tools: **Scala 3** + Scala 2.13 cross-build, **sbt**, Bloop,
  Metals + IntelliJ both, custom CI built on `sbt-ci-release`,
  Sonatype Central, scalafmt, scalafix

### Profile

Anya has written sbt plugins as a hobby. She maintains ~30 in-house
sbt plugins for her firm: telemetry decorators, regulatory
attestations, a custom artifact uploader to their internal mirror. She
ships ScalaJS for two internal dashboards and ScalaNative for one
latency-sensitive market-data adapter. She knows what `Project`,
`AutoPlugin`, `inConfig`, and `Compile / fork` actually do.

She also knows sbt's start time is 90 seconds and she's typed "exit"
into the sbt shell more times than she's typed her password. The
build's metadata is in 4 places — `build.sbt`, `project/`, `Plugins.scala`,
`buildSrc.sbt` — and she's the only person on the team who fully
understands which is which.

She is exactly the kind of person bleep was originally written for.

### What she wants from a build tool

- **Cross-building parity** with sbt: JVM × {2.13, 3} × {JVM, JS, Native}
  matrix that Just Works.
- **Sonatype Central publishing** with PGP signing, sbt-ci-release-style
  release flow.
- **Scripts she can `setup-dev-script` and debug** in IntelliJ — sbt
  plugins as macros are a debugging nightmare.
- **Native binary startup.** 90s sbt → 0s bleep is the killer feature.
- **Incremental compile.** Zinc-grade. No regressions vs sbt.
- **scalafmt + scalafix** integration that doesn't require its own
  dance.

### Bull case

- bleep ships `bleep-plugin-ci-release`, `bleep-plugin-sonatype`,
  `bleep-plugin-pgp`, `bleep-plugin-scalafix` as ports of the sbt
  equivalents. Her release pipeline can be re-implemented in a script
  she can debug.
- Cross-building works — `cross: { jvm213, jvm3, native3, ... }` — and
  publishing produces correct Maven coordinates with Scala suffixes.
- BSP integration in IntelliJ matches Metals quality.

### Bear case

- A single sbt feature she relies on is missing: scope axes (especially
  `Compile` vs `Test` task overrides), per-config classpath manipulation,
  custom task dependencies.
- A sbt plugin she uses (`sbt-jmh`, `sbt-explicit-deps`,
  `sbt-protoc`, ...) doesn't have a bleep port — and she has to write
  it herself the week before a release.
- Anything subtly wrong with cross-version publishing — wrong
  artifact name, missing metadata, version-handling regression. Her CI
  release uses `dynver` from git tags.

Anya is **bleep's natural champion**: if she adopts it, she'll port
plugins and write blog posts. If she rejects it, she'll write a blog
post about that too.

---

## 5. Tariq — Engineering manager at a polyglot mid-size SaaS

**Quick stats**

- 17 years experience, started in Java, drifted through Scala, now
  managing
- Engineering Manager / Principal Architect
- ~80 engineers, mid-market B2B SaaS
- Stack: **Java backend** (Spring Boot + Maven), **Kotlin** for
  Android-flavored shared libs and an SDK (Gradle), **Scala** for the
  data pipeline team (sbt). Three teams, three build tools, three
  release pipelines.

### Profile

Tariq doesn't write much code anymore — maybe 10% of his time. He
spends it reviewing architecture proposals, standardizing CI, and
firefighting cross-team release coordination. His current pain isn't
any one tool — it's that *every* team uses a different one.

When the security team says "audit your transitive deps", Tariq has to
go to three different teams with three different commands. When the
SRE team rolls out a new Docker base image, three teams figure out
their own CI integration. When a Kotlin engineer wants to rotate to
the Scala team, the build tool is one of the things they have to
re-learn.

He has explicitly *not* tried to standardize on one of the existing
tools because each ecosystem is so deeply tied to its build tool that
saying "everyone use Gradle" or "everyone use Maven" would mean
fighting Scala or Java idiom for years.

### What he wants from a build tool

- **One tool that's a first-class citizen for all three languages.** Not
  "Java with Scala bolted on" or "Scala with Kotlin support".
- **Predictable migration cost.** He wants to convert one team at a
  time — not a 6-month all-or-nothing migration.
- **Consistent CI shape.** Same `bleep test`, `bleep publish` whether
  it's a Java service or a Scala pipeline.
- **Polyglot scripts.** Java/Kotlin/Scala teams should each be able to
  read and modify a build script.
- **Governance.** Centralized templates so platform team can enforce
  Java versions, common deps, security policies.

### Bull case

- Bleep treats Java/Kotlin/Scala as peers. The script API is Java
  (consumed from any language). Templates work across teams. The same
  CI step works for any project.
- `bleep import` from both Maven and sbt (already shipping) makes
  one-team-at-a-time migration plausible.
- Cross-language ergonomics: a Kotlin team and a Scala team can read
  each other's `bleep.yaml` without learning a new tool.

### Bear case

- Any language gets second-class treatment in practice. "Bleep can
  technically do Kotlin but the docs are all Scala" is rejection-level.
- Migration from Gradle is harder than from Maven (no `bleep import`
  for Gradle today) — that's one of his three teams.
- Kotlin/JS gap, KSP gap, Android gap — any one of these blocks one of
  his teams.

Tariq is **bleep's strategic pivot persona**. The Java/Kotlin/Scala
parity story is what makes him possible, and he's the persona who
makes the JVM-build-tool framing pay off in dollars.

---

## How to use this list

When making a product or docs decision, ask:

1. **Does this hurt David's import path?** If yes, put it behind a
   flag.
2. **Does Priya's KMP example still work?** If we drift on Kotlin/JS
   support, she's the canary.
3. **Does Marcus's getting-started page still hold his hand?** Every
   docs change should be re-read with him in mind.
4. **Does Anya still get her cross-publishing fidelity?** No silent
   regressions on `cross:` semantics or publish metadata.
5. **Does Tariq still see Java/Kotlin/Scala as equal citizens?** If a
   feature lands for one language and not the other two, he notices.
