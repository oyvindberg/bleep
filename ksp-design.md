# KSP1 support in bleep — design + implementation plan

Working document. Captures the prior investigation, the design choices, the
implementation milestones, and the integration-test plan. Source-of-truth for
the work; update it as the work progresses.

---

## 1. What KSP is

**KSP = Kotlin Symbol Processing.** JetBrains' Kotlin-native replacement for
KAPT. A processor is a JAR that:

- Exports `META-INF/services/com.google.devtools.ksp.processing.SymbolProcessorProvider`.
- Reads Kotlin symbols (declarations, types, resolved references, nullability,
  suspend, sealed hierarchies) through KSP's API.
- Emits `.kt`, `.java`, `.class`, or resources into compiler-readable output
  directories.

Libraries that ship as KSP processors and are the reason this work matters:
**Room, Hilt/Dagger, Koin KSP, kotlinx.serialization (KSP variant), Spring
Modulith, Moshi codegen, Wire, Ktorfit, kotlin-inject, KMongo.**

### KSP1 vs KSP2

|                       | KSP1                                                  | KSP2                                       |
| --------------------- | ----------------------------------------------------- | ------------------------------------------ |
| Runs as               | A Kotlin compiler plugin loaded into kotlinc          | Standalone, against Kotlin Analysis API    |
| Status                | Stable, ships against every Kotlin release            | Beta in 2.1, GA-tracking in 2.x            |
| Version coupling      | KSP version is pinned to exact kotlinc version        | Loosely coupled to Kotlin version          |
| Integration shape     | JAR on `pluginClasspaths` + `-P plugin:…:k=v` options | Separate process / library invocation      |

**Ship KSP1 first.** Every processor in the ecosystem targets it today.
Bleep's kotlinc plumbing already loads compiler plugins. KSP2 lands later as
its own runner once it stops moving.

---

## 2. Why bleep is well positioned

Each piece KSP needs already exists for another purpose:

| Need                                              | Already in bleep                                                                                                   | Location                                                                                                                       |
| ------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------ |
| In-process kotlinc                                | Reflection-based, isolated classloader, incremental + fallback                                                     | `bleep-bsp/src/scala/bleep/analysis/KotlinSourceCompiler.scala:539, 687`                                                       |
| Compiler-plugin classpath wiring                  | `setPluginClasspaths(Array<String>)` called for spring/jpa/allopen/noarg/serialization                             | `KotlinSourceCompiler.scala:758-768`                                                                                           |
| Plugin options wiring                             | `setPluginOptions(Array<String>)` of `"plugin:id:key=value"`                                                       | `KotlinSourceCompiler.scala:770-783`                                                                                           |
| Plugin-JAR resolution by ID via Coursier          | `CompilerResolver.resolveKotlinPlugin` maps plugin IDs to Maven coords                                             | `CompilerResolver.scala:324-346`                                                                                               |
| Per-project pre-compile resolution task pattern   | `ResolveAnnotationProcessorsTask`, `apPlan`, `apResults` shared map, handler factory, compile-side pickup          | `TaskDag.scala:116, 553`; `MultiWorkspaceBspServer.scala:1128, 1162, 2126`                                                     |
| Generated-sources convention added to source set  | `.bleep/generated-sources/<crossName>/<folder>/` already feeds `ProjectPaths.sourcesDirs`                          | `BuildPaths.scala:57-62, 75-76`; `ResolveProjects.scala:458`                                                                   |

The Java annotation-processor flow is the same shape minus "processor runs
inside kotlinc instead of javac." ~70% code reuse.

---

## 3. Version pairing decision

### The reality

- KSP1 releases are tagged `<kotlinVersion>-<kspMinor>`, e.g. `2.1.0-1.0.29`,
  `2.0.21-1.0.28`. The kotlin prefix is mandatory and exact: a KSP release
  built for Kotlin 2.1.0 won't load into kotlinc 2.1.10.
- Multiple KSP releases per Kotlin version exist (`2.1.0-1.0.27`, …, `-1.0.30`),
  primarily for KSP-side bug fixes.
- So Kotlin version determines the **prefix**. The KSP-side suffix is a choice.

### Options considered

1. **Auto-resolve latest KSP for the given Kotlin version.**
   Rejected: violates bleep's deterministic-build principle (CLAUDE.md). Two
   runs days apart could resolve different KSP versions silently.

2. **Bake a kotlinVersion → kspVersion map into bleep.**
   Rejected: bleep becomes responsible for tracking JetBrains releases. Every
   KSP point release needs a bleep release.

3. **Require the user to specify the full coordinate, e.g. `2.1.0-1.0.29`.**
   Rejected as default UX: redundant with `kotlin.version`, drift between the
   two becomes a footgun.

4. **User specifies the KSP-side suffix only. Bleep concatenates with `kotlin.version`.**
   **Chosen.** One field, no defaults, no magic, no surprise updates, no
   redundancy. The full resolved coordinate appears in errors and logs.

### The model

```yaml
projects:
  myapp:
    kotlin:
      version: 2.3.0
      kspVersion: 1.0.29      # ← KSP suffix only
      symbolProcessors:
        - androidx.room:room-compiler:2.6.1
      symbolProcessorOptions:
        room.schemaLocation: ./schemas
        room.incremental: 'true'
```

Bleep resolves
`com.google.devtools.ksp:symbol-processing-aa-embeddable:2.3.0-1.0.29`. If
that coordinate doesn't exist on Maven Central, Coursier raises; bleep
surfaces the full coord and a hint to check
<https://github.com/google/ksp/releases>.

`kspVersion` is **required** when `symbolProcessors` is non-empty (or, later,
`scanForSymbolProcessors: true`). Fail loud at project load. No default.

### Why not "auto" with a way to pin?

Because then the docs read "by default it's not deterministic; if you want
deterministic, set this field." Reverse the polarity: deterministic by
default, no field for opting out of determinism.

---

## 4. Model surface

`bleep-model/src/scala/bleep/model/Kotlin.scala` adds four fields:

```scala
case class Kotlin(
    version: Option[VersionKotlin],
    options: Options,
    jvmTarget: Option[String],
    compilerPlugins: JsonSet[String],
    // new:
    kspVersion: Option[String],                       // KSP-side version suffix, e.g. "1.0.29"
    scanForSymbolProcessors: Option[Boolean],         // mirror of java.scanForAnnotationProcessors
    symbolProcessors: JsonSet[Dep],                   // explicit processor deps
    symbolProcessorOptions: SymbolProcessorOptions,   // SortedMap[String, String]
    js: Option[KotlinJs],
    native: Option[KotlinNative]
) extends SetLike[Kotlin]
```

`SymbolProcessorOptions` is a sibling to `AnnotationProcessorOptions` (a
SetLike around `SortedMap[String, String]`). Reuse the AP one's encoder
shape verbatim if practical.

`SetLike` operations (union/intersect/removeAll) extended in the obvious
ways. Template normalization works without further change.

Schema (`schema.json`): emit the four fields with descriptions.

---

## 5. Resolution layer

New file `bleep-core/src/scala/bleep/SymbolProcessorResolver.scala`,
modelled directly on `AnnotationProcessorResolver.scala`.

```scala
case class SymbolProcessorResult(
    kspPluginJars: List[Path],         // symbol-processing-aa-embeddable + transitive
    processorJars: List[Path],         // user processors
    kotlinOutputDir: Path,             // .bleep/generated-sources/<cross>/ksp/kotlin/
    javaOutputDir: Path,               //                                  /ksp/java/
    classOutputDir: Path,              //                                  /ksp/classes/
    resourceOutputDir: Path,           //                                  /ksp/resources/
    cachesDir: Path,                   //                                  /ksp/caches/
    kspOutputBaseDir: Path,            //                                  /ksp/
    options: SortedMap[String, String]
) {
  def kotlincPluginOptions(projectDir: Path): List[String] = { /* … */ }
}

object SymbolProcessorResolver {
  def resolve(
      crossName: CrossProjectName,
      kotlin: Kotlin,
      resolvedDependencyJars: List[Path],
      kotlinVersion: VersionKotlin,
      kspVersion: String,
      resolver: CoursierResolver,
      paths: KspPaths,
      logger: Logger
  ): SymbolProcessorResult
}
```

Resolution steps:

1. Validate input:
   - `kspVersion` must match `^[0-9]+\.[0-9]+\.[0-9]+$`. No `-SNAPSHOT`.
   - `symbolProcessors` non-empty OR `scanForSymbolProcessors: true`.
2. Build full KSP coord: `com.google.devtools.ksp:symbol-processing-aa-embeddable:<kotlinVersion>-<kspVersion>`.
3. Resolve via Coursier. Loud-fail if the coord doesn't exist with a message
   pointing at the KSP release page.
4. Resolve user processors via `resolver.force(...)` (mirror of AP path).
5. If `scanForSymbolProcessors: true`, scan resolved dep JARs for
   `META-INF/services/com.google.devtools.ksp.processing.SymbolProcessorProvider`.
6. Validate: scan-opt-in with zero processors found → loud fail (mirror of
   AP). Explicit processors with zero resolved → loud fail.
7. Compute output dirs under `.bleep/generated-sources/<crossName>/ksp/`.
8. Return `SymbolProcessorResult`.

`CompilerResolver.resolveKspPlugin(kotlinVersion, kspVersion)` is a new
method, ~15 lines, mirrors `resolveKotlinPlugin`.

### The `-P plugin:` option surface

KSP1's plugin ID is `com.google.devtools.ksp.symbol-processing`. The options
bleep emits:

```
-P plugin:com.google.devtools.ksp.symbol-processing:apclasspath=<processorJars-pathsep-joined>
-P plugin:com.google.devtools.ksp.symbol-processing:projectBaseDir=<projectDir>
-P plugin:com.google.devtools.ksp.symbol-processing:kotlinOutputDir=<…>
-P plugin:com.google.devtools.ksp.symbol-processing:javaOutputDir=<…>
-P plugin:com.google.devtools.ksp.symbol-processing:classOutputDir=<…>
-P plugin:com.google.devtools.ksp.symbol-processing:resourceOutputDir=<…>
-P plugin:com.google.devtools.ksp.symbol-processing:kspOutputDir=<…>
-P plugin:com.google.devtools.ksp.symbol-processing:cachesDir=<…>
-P plugin:com.google.devtools.ksp.symbol-processing:incremental=true
-P plugin:com.google.devtools.ksp.symbol-processing:incrementalLog=false
-P plugin:com.google.devtools.ksp.symbol-processing:apoption=<k>=<v>     # one per option
```

`incremental=true` is critical for the incremental-compilation tests below;
ship with it on.

---

## 6. DAG wiring

`bleep-bsp/src/scala/bleep/bsp/TaskDag.scala`:

```scala
case class ResolveSymbolProcessorsTask(project: CrossProjectName) extends Task {
  val id: TaskId = TaskId.ResolveSymbolProcessors(project)
  val dependencies: Set[TaskId] = Set.empty
}

case class SymbolProcessorPlan(projects: Set[CrossProjectName]) {
  def needsResolution(p: CrossProjectName): Boolean = projects.contains(p)
  def isEmpty: Boolean = projects.isEmpty
}
```

In `compileDeps()` (currently `TaskDag.scala:537-550`) add KSP deps next to
AP deps:

```scala
val kspDeps: Set[TaskId] =
  if (ctx.kspPlan.needsResolution(project)) Set(TaskId.ResolveSymbolProcessors(project): TaskId)
  else Set.empty
(projectDeps, compileTaskDeps ++ sourcegenDeps ++ apDeps ++ kspDeps)
```

`annotationProcessorTasks()` gets a sibling `symbolProcessorTasks()`. The
three `buildXxxDag` functions include the new tasks alongside the existing
AP tasks.

`TaskId` sealed trait: add `ResolveSymbolProcessors(project)`.

---

## 7. Handler + shared state

`bleep-bsp/src/scala/bleep/bsp/MultiWorkspaceBspServer.scala`:

- Sibling map: `kspResults: ConcurrentHashMap[CrossProjectName, SymbolProcessorResult]`.
- Sibling builder: `buildSymbolProcessorPlan(started, projects)`. Walks
  projects with `kotlin.symbolProcessors` non-empty or
  `kotlin.scanForSymbolProcessors: true`.
- Sibling handler: `makeSymbolProcessorHandler` mirrors
  `makeAnnotationProcessorHandler`. `.attempt`-wrap so resolver `sys.error`
  surfaces as `TaskResult.Failure` (the executor's Finished event must
  fire, otherwise the build summary's failure counter stays at zero, same
  trap as for AP).
- `BuildSummary` gets a `kspResolutionFailed` counter, parallel to
  `apResolutionFailed`. `commands.compile` throws on misconfig.

---

## 8. Compile-side wiring

`MultiWorkspaceBspServer.scala:2126` already does:

```scala
val apFlags: List[String] = Option(apResults.get(compileTask.project)).fold(List.empty[String])(_.javacFlags)
val config = BleepBuildConverter.toProjectConfig(compileTask.project, started.resolvedProject(compileTask.project), started, apFlags)
```

Extend with KSP:

```scala
val kspResult: Option[SymbolProcessorResult] = Option(kspResults.get(compileTask.project))
val (kspPluginJars, kspKotlincOpts) = kspResult match {
  case Some(r) => (r.kspPluginJars, r.kotlincPluginOptions(projectDir))
  case None    => (List.empty[Path], List.empty[String])
}
val config = BleepBuildConverter.toProjectConfig(
  compileTask.project,
  started.resolvedProject(compileTask.project),
  started,
  apFlags = apFlags,
  kspPluginJars = kspPluginJars,
  kspKotlincOpts = kspKotlincOpts
)
```

`BleepBuildConverter.resolveCompilerPlugins` (currently lines 209-225)
gets extended:

```scala
private def resolveCompilerPlugins(
    kotlinVersion: VersionKotlin,
    pluginIds: List[String],
    extraPluginJars: List[Path],        // ← KSP plugin JAR(s)
    extraPluginOpts: List[String]       // ← KSP -P plugin:…:k=v options
): List[String] = {
  val pluginIdJars = pluginIds.map(id => CompilerResolver.resolveKotlinPlugin(id, kotlinVersion))
  val allJars      = pluginIdJars ++ extraPluginJars
  val xpluginOpt   = if (allJars.isEmpty) Nil else List(s"-Xplugin=${allJars.map(_.toString).mkString(",")}")
  val presetOpts   = pluginIds.flatMap { /* unchanged */ }
  xpluginOpt ++ presetOpts ++ extraPluginOpts
}
```

The `-Xplugin=` flag is comma-separated, so multiple plugins (existing
compiler plugins + KSP) merge into one. `setPluginClasspaths` handles the
split correctly: `applyCompilerOptions` already splits the comma-joined
string at `KotlinSourceCompiler.scala:763`.

---

## 9. Source-set inclusion

`bleep-model/src/scala/bleep/BuildPaths.scala` (currently lines 58-62):

```scala
val annotationProcessing =
  p.java
    .filter(j => j.scanForAnnotationProcessors.contains(true) || j.annotationProcessors.values.nonEmpty)
    .map(_ => generatedSourcesDir(crossName, "annotations"))
val kspGenerated: List[Path] =
  p.kotlin.filter(k => k.scanForSymbolProcessors.contains(true) || k.symbolProcessors.values.nonEmpty).toList.flatMap { _ =>
    val ksp = generatedSourcesDir(crossName, "ksp")
    List(ksp.resolve("kotlin"), ksp.resolve("java"))
  }
```

`ProjectPaths.DirsByOrigin` either grows a `ksp: List[Path]` field or the
existing `annotationProcessing: Option[Path]` becomes a more general
`generated: List[Path]`. Prefer the latter rename to keep things clean.

`ResolveProjects.scala:458` already merges all source-dir origins; no
further change required there once `DirsByOrigin` exposes the KSP dirs.

---

## 10. The noop-manifest hash, the one subtle thing

`MultiWorkspaceBspServer.scala:2129` short-circuits compilation when the
project's inputs hash matches the previous run. **Must include in the
hash, or stale KSP output sneaks through:**

- The KSP plugin JAR fingerprint (SHA over the resolved coord + jar bytes).
- The processor JAR fingerprints.
- The processor option map.
- The `kspVersion` and `kotlin.version` literal strings.

This is the one place the AP analogy is **insufficient**, because the AP
flow already incorporates `javac` flags into the manifest, and AP changes
flow through `apResult.javacFlags`. KSP plugin classpath changes flow
through `kotlinc` options, not `javac` options. Need to verify
`ZincBridge.isNoop`'s digest covers kotlinc options the same way it covers
javac options. If not, extend.

**Action item:** read `ZincBridge.isNoop` before implementation. Decide
whether to extend the noop digest input or to make `kspResult` invalidate
the manifest by other means.

---

## 11. Implementation milestones

### M1 — Model + schema (1 day)

- [ ] Extend `model.Kotlin` with `kspVersion`, `scanForSymbolProcessors`,
      `symbolProcessors`, `symbolProcessorOptions`.
- [ ] `SymbolProcessorOptions` SetLike type.
- [ ] Decoder/encoder round-trip tests.
- [ ] `schema.json` updated.
- [ ] Validation in build-loading: `kspVersion` required when processors
      configured.

### M2 — Resolver + plugin JAR resolution (1 day)

- [ ] `SymbolProcessorResolver.resolve` end-to-end.
- [ ] `CompilerResolver.resolveKspPlugin`.
- [ ] Unit tests (no kotlinc): given a project with two processors and
      three options, produce the expected plugin-options list.

### M3 — DAG + handler wiring (1 day)

- [ ] `ResolveSymbolProcessorsTask`, `TaskId`, `SymbolProcessorPlan`.
- [ ] `buildSymbolProcessorPlan`, `makeSymbolProcessorHandler`.
- [ ] Three `buildXxxDag` builders include the new tasks.
- [ ] `BuildSummary.kspResolutionFailed`.

### M4 — Compile-side wiring (1 day)

- [ ] `BleepBuildConverter.toProjectConfig` takes the new args, threads
      through to `BleepBuildConverter.resolveCompilerPlugins`.
- [ ] `MultiWorkspaceBspServer.makeCompileHandler` reads `kspResults`.
- [ ] **Noop-manifest digest** extended to incorporate kotlinc plugin
      classpath + options.

### M5 — Generated-sources source-set inclusion (½ day)

- [ ] `BuildPaths` emits KSP output dirs.
- [ ] `ProjectPaths.DirsByOrigin` exposes them (or rename
      `annotationProcessing` → `generated`).
- [ ] `bleep clean` wipes `.bleep/generated-sources/<cross>/ksp/`.

### M6 — Integration tests, basic (2 days)

See section 12.

### M7 — Integration tests, incremental (3 days)

See section 13.

### M8 — Docs (½ day)

- [ ] `docs/usage/annotation-processing.mdx` "Kotlin: KSP / KAPT" section
      replaced with KSP1 user guide.
- [ ] New tutorial: `docs/tutorials/kotlin-room.mdx` (Room is the canonical
      processor; nail it end-to-end).
- [ ] `docs/appendix/status.mdx` flipped: KSP1 supported, KSP2 tracking
      JetBrains GA.

### M9 — Status doc + landing page (½ day)

- [ ] Persona maturity matrix lights up the Kotlin row.
- [ ] Release notes mention KSP and Room support.

**Total: ~10-11 engineer-days of focused work**, plus stabilization on
real-world processors.

---

## 12. Integration tests, basic

All under `bleep-tests/src/scala/bleep/`, suffix `IT`, follow
`IntegrationTestHarness` pattern.

### Custom toy processor

Build a tiny KSP processor we control, published locally via
`my-publish-local` for tests. Lives under
`liberated/` or `bleep-tests/test-fixtures/toy-ksp-processor/`. Three
properties:

1. Reads `@bleep.test.Generate("name")` on a class, emits
   `<name>Generated.kt` containing a function `fun <name>Hello(): String`.
2. Writes a side-effect file `.bleep/ksp-invocations.log` each time the
   processor runs (with timestamp and inputs seen). Used to count
   invocations and prove incrementality.
3. Reads option `bleep.toy.suffix` (default `"Hello"` — wait, no defaults;
   require it) to vary output.

### `KspBasicIT`

Cases:

1. **`kspResolves`**: minimal project with one toy processor, one
   `@Generate` annotation. Run `bleep compile`. Assert:
   - `.bleep/generated-sources/<cross>/ksp/kotlin/FooGenerated.kt` exists.
   - Generated `.class` exists in the project's classes dir.
   - Generated `fooHello` symbol resolves from a downstream caller in the
     same project.
2. **`kspMissingKspVersion`**: project has `symbolProcessors` but no
   `kspVersion`. Build load fails loud, message includes the field name.
3. **`kspBadCoord`**: `kspVersion: 99.0.0`. Compile fails, error mentions
   the full coord `<kotlin>-99.0.0`.
4. **`kspBadKotlinVersion`**: KSP version exists but kotlin pin mismatches
   (force by setting `kotlin.version` to one without a published KSP).
   Compile fails with clear message.
5. **`kspOptionPassthrough`**: processor reads
   `bleep.toy.suffix: "World"`, generated function is
   `fun fooWorld(): String`. Assert it landed.
6. **`kspMultipleProcessors`**: two processors (toy + a second toy that
   generates `<name>Other.kt`). Both run, both outputs exist, both
   downstream-resolved.
7. **`kspNoProcessorJars`**: `scanForSymbolProcessors: true` with no
   matching jars in `dependencies`. Fail loud.
8. **`kspWithCompilerPlugin`**: KSP + existing `compilerPlugins: [serialization]`.
   Both `-Xplugin=` JARs land, both run. Tests the `-Xplugin=`
   comma-joining path.
9. **`kspCleanWipesGenSources`**: `bleep clean myproject` removes
   `.bleep/generated-sources/<cross>/ksp/`.
10. **`kspCrossBuilding`**: same project, `cross: { jvm21, jvm17 }`, both
    variants run KSP, outputs land at the right per-cross paths.

### `KspRoomIT` (canonical real-world)

Pulls Room (`androidx.room:room-compiler:2.6.1`,
`androidx.room:room-runtime:2.6.1`). Pure-JVM, no Android. Defines:

- One `@Entity` class.
- One `@Dao` interface.
- One `@Database` class.

Asserts:

- `<package>/<DatabaseName>_Impl.kt` and `<package>/<DaoName>_Impl.kt`
  generated.
- Compile succeeds. A small main using `Room.databaseBuilder(...)` runs in
  an in-memory configuration.

Room is JetBrains' poster-child KSP processor. Passing this end-to-end is
the public proof that bleep KSP1 works for ecosystem libraries.

---

## 13. Integration tests, incremental

This is the half the user explicitly called out. Every test here uses the
toy processor (because we need to count invocations) plus reads from the
side-effect log `.bleep/ksp-invocations.log`. Tests live in
`KspIncrementalIT`.

### Setup pattern

Each test starts a workspace, runs a sequence of compiles with edits
between, and after each compile reads
`.bleep/ksp-invocations.log` to assert how many times the processor ran
and on which inputs.

The toy processor writes one line per invocation:
```
<timestamp> processed: <symbol1>,<symbol2> emitted: <out1>,<out2>
```

### Cases

#### A. **`coldCompile`**
Empty workspace. First compile. Assert:
- Processor ran once.
- Processed all annotated symbols.
- Generated files exist.

#### B. **`noopRecompile`**
After A, run `bleep compile` again without touching anything. Assert:
- **Processor did NOT run.** Log unchanged.
- The compile noop'd entirely (no kotlinc invocation either).

#### C. **`editUnrelatedSource`**
After A, edit a source file that doesn't carry an annotation and isn't
referenced by an annotated class. Recompile. Assert:
- Processor either didn't run (KSP's incremental decided no), OR ran but
  emitted same bytes (acceptable per KSP1's semantics).
- Generated files: identical bytes vs previous run.

#### D. **`editAnnotatedSource`**
After A, edit the body of a class carrying `@Generate("foo")` (rename
`foo` → `bar`). Recompile. Assert:
- Processor ran.
- New generated file `barGenerated.kt`. Old `fooGenerated.kt` removed.
- Downstream compile picks up rename.

#### E. **`editProcessorOption`**
After A, change `bleep.toy.suffix` from `"Hello"` to `"World"` in
`bleep.yaml`. Recompile. Assert:
- **Full reprocess.** Processor ran on all symbols.
- Generated files now `fooWorld.kt` etc.

#### F. **`changeProcessorVersion`**
Toy processor published locally at two versions (v1 emits `Hello`, v2
emits `Howdy`). After running with v1, bump `bleep.yaml` to v2. Recompile.
Assert:
- Full reprocess.
- Output reflects v2.
- KSP caches dir (`/ksp/caches/`) was wiped or invalidated.

This is the **noop-manifest digest** test. If the digest didn't include
the processor JAR fingerprint, this test would fail (output stale).

#### G. **`changeKspVersion`**
After A, bump `kspVersion` from `1.0.29` to `1.0.30` (same kotlin
version, KSP minor bump). Recompile. Assert:
- Full reprocess.
- Caches dir wiped or invalidated.
- Output still correct.

#### H. **`changeKotlinVersion`**
After A, bump `kotlin.version` from `2.3.0` to `2.3.10`. Assert:
- Resolver fetches a new KSP coord (`2.3.10-1.0.29`).
- Full reprocess.
- Caches dir behavior: separate sub-key, or cleared.

#### I. **`bspDaemonRestart`**
After A and B (noop), force-restart the bleep-bsp daemon (`./kill-bsp.sh`
or BSP-level signal). Run `bleep compile` again. Assert:
- Processor did **not** run. Incremental survived the daemon restart
  because KSP caches live on disk.
- Compile finished in noop time.

This proves the incremental story works across bleep's BSP daemon
lifecycle, which is a real user concern.

#### J. **`branchSwitch`**
Workspace on branch `main` with `kotlin.version: 2.3.0` runs A. Switch to
branch `legacy` with `kotlin.version: 2.0.0` (and matching KSP). Run
compile. Switch back to `main`. Run compile. Assert:
- Each branch's KSP caches under `.bleep/generated-sources/<cross>/ksp/`
  is independent. The second compile on `main` is a noop again.

#### K. **`multiRound`**
Toy v3 processor: when it sees `@GenerateAndDefer("foo")`, in round 1 it
emits a marker class and defers; in round 2 it emits the real
`fooGenerated.kt` based on the marker. Recompile. Assert:
- Multi-round completes within one kotlinc invocation.
- Final output correct.
- Log shows two processor invocations within the same compile.

#### L. **`processorThrows`**
Toy v4: throws on a specific input. Compile. Assert:
- Compile fails with a clear KSP error attributable to the processor
  (not a kotlinc internal crash).
- BSP diagnostics carry the failure.
- Subsequent fixed-input compile succeeds and produces correct output.

#### M. **`addProcessor`**
After A with one processor, add a second processor to `symbolProcessors`.
Recompile. Assert:
- Full reprocess (because processor set changed).
- Both sets of outputs land.

#### N. **`removeProcessor`**
After M, remove the second processor. Recompile. Assert:
- Full reprocess.
- Second processor's outputs gone or stale (acceptable to leave behind in
  v1; document).
- Better: explicit cleanup. Decide.

#### O. **`mixedKotlinJava`**
Project with both `.kt` and `.java` sources where KSP emits `.java` (some
processors do). Assert:
- Generated `.java` lands in `ksp/java/`.
- javac picks it up in the mixed-compile pass (interacts with the
  pending Kotlin+Java mixed-compile work; see CLAUDE.md memory
  `project_kotlin_java_mixed.md`).

This may need to land **after** the Kotlin+Java mixed-compile work.
Mark it as conditional in the test suite.

---

## 14. Risks and open questions

### Verified before implementation

1. **Noop-manifest digest must include kotlinc options + plugin JAR
   fingerprints.** Verify `ZincBridge.isNoop`'s digest input. If it only
   covers javac flags, extend. Without this, test F fails.

2. **KSP1's incremental cache lifecycle inside a long-running BSP
   daemon.** Read KSP1's incremental docs. Specifically: does KSP1 reuse
   the in-memory state across kotlinc invocations? If yes, the
   reflection-based kotlinc invocation must not destroy that state.
   If no, caches on disk are the only handoff and we're fine.

3. **Kotlin 2.x KSP1 coord name.** Confirm
   `com.google.devtools.ksp:symbol-processing-aa-embeddable` is the
   correct artifact for Kotlin 2.x. KSP1 for Kotlin 1.x was
   `symbol-processing` and `symbol-processing-api`. The `-aa-` (Analysis
   API) variant is the Kotlin-2.x path. Pin this in code.

4. **Mixed-compile interaction.** Test O depends on the Kotlin+Java
   mixed-compile project work. Coordinate order.

### Punted

5. **KSP2.** Standalone runner, not a kotlinc plugin. Out of scope here.
   Defer until KSP2 ships GA-tagged and JetBrains commits to its API.

6. **KAPT.** Don't ship. KAPT is in maintenance, JetBrains discourages
   new use, and porting is non-trivial. Document that KAPT users should
   migrate to KSP first.

7. **Kotlin/JS KSP.** KSP supports KMP and JS, but bleep's Kotlin/JS path
   has its own gaps. Stage as a follow-up; document JVM-only in v1.

8. **Per-cross `kspVersion` overrides.** Today `cross:` block can override
   `kotlin.version`. If a cross variant uses a different `kotlin.version`,
   the variant needs its own `kspVersion`. Verify the SetLike normalization
   handles this; add a cross-build test.

### Decisions to revisit at end of implementation

9. **Should `scanForSymbolProcessors` ship in MVP?** Possibly punt to v1.1
   if explicit processors cover all observed use cases. The Java AP scan
   exists because Lombok and Mapstruct are widely deployed without
   explicit listing; the equivalent default for KSP isn't as load-bearing.
   Decision: ship in MVP because it's nearly free and mirrors AP shape.

10. **Cleanup of stale generated sources.** When a user removes a
    processor or an `@Generate` annotation, the old generated file
    persists. KSP1 has its own cleanup tracking. Decide whether bleep
    needs to layer additional cleanup or trust KSP. Decision: trust KSP1,
    add a documentation note that `bleep clean` is the manual reset.

---

## 15. MVP cut

Smallest shippable surface that closes Priya's blocker:

1. KSP1 only, Kotlin 2.x via `symbol-processing-aa-embeddable`.
2. Explicit `symbolProcessors:` list. `scanForSymbolProcessors` also lands
   in MVP because the marginal cost is small.
3. `symbolProcessorOptions:` map.
4. JVM platform only. JS and Native deferred.
5. Mixed Kotlin+Java: KSP-emitted `.java` lands in `ksp/java/` and is
   picked up by the mixed-compile path **if** that path is in.
   Otherwise documented gap.
6. Integration tests: full basic suite + incremental suite. Room
   end-to-end.
7. Docs: AP page section rewritten; Room tutorial; status flipped.

This unblocks Room, Hilt, kotlinx.serialization-via-KSP, Koin KSP,
Spring Modulith, Wire, Moshi codegen, kotlin-inject, Ktorfit, KMongo.
Single highest-leverage Kotlin change.

---

## 16. Out of scope

- KAPT (KAPT users get a migration note).
- KSP for Kotlin/JS, Kotlin/Native. See §24 for the technical reason —
  this isn't a "stage later" item, it's blocked on upstream architecture.
- Bleep-side caching of resolved KSP plugin JARs beyond what
  `CompilerResolver.jarCache` already does.
- KSP processor authoring as a bleep-native shape (writing KSP processors
  IN bleep). That's just a regular Kotlin library project.

---

## 17. Status

- [x] M1 Model + schema (6 unit tests passing)
- [x] M2 Resolver (6 unit tests passing)
- [x] M3 DAG + handler (7 new dag tests passing; SourcegenDag + LinkDag still green)
- [x] M4 Compile wiring → reverted after pivot to path (a); kotlinc no longer needs KSP plumbing
- [x] M5 Source set (Kotlin paths under `ksp/kotlin`, `ksp/java`, `ksp/resources` exposed to bleep clean and project sources)
- [x] M6 Basic ITs — **5/5 passing** with real Moshi codegen end-to-end after path-(a) pivot
- [x] M7 Incremental ITs — **5/5 passing** (scaled down from §13's 15 cases; see §20)
- [x] M8 Docs — `docs/usage/annotation-processing.mdx` rewritten with KSP section; `docs/tutorials/kotlin-with-ksp.mdx` added; sidebar wired; status page flipped
- [x] M9 Landing page — `MaturitySection` added showing per-language first-class / partial / not-in-scope; the Kotlin column calls out KSP support explicitly

**Total tests passing**: 89 (6 model + 8 resolver + 13 KspIncrementalState + 7 KSP dag + 18 sourcegen dag + 14 link dag + 5 AP IT + 4 Kotlin IT + 1 YourFirstKotlin IT + 5 KSP IT basic + 5 KSP IT incremental + 3 KSP IT mixed-compile).

## 21. Per-file change tracking landed

`KspIncrementalState` (in `bleep-core/src/scala/bleep/`) maintains an `inputs-manifest.json` per project+variant under the per-variant KSP state dir. It tracks
per-source SHA-256 plus fingerprints over processor jars, libraries, KSP/Kotlin versions, and the processor-options map. On each KSP run:

- No prior state → `Decision.FullRebuild` (KSP runs with `-incremental=false`).
- Non-source input changed (processor version, jar fingerprint, options, libraries) → `Decision.CacheBust` (wipe `caches/`, run with `-incremental=false`).
- Same processor/libraries/options, only source-file content/set changed → `Decision.Incremental(modifiedSources, removedSources)` (run with
  `-incremental=true` and the right delta lists).

The wiring lives in `MultiWorkspaceBspServer.makeSymbolProcessorHandler`: compute decision, optionally wipe `caches/`, invoke `KspRunner.run(ksp, decision, …)`,
save manifest on success. Manifest is the bleep-side ground truth; KSP's own caches are populated/cleared in lockstep.

Test coverage: 13 unit tests in `KspIncrementalStateTest` cover every Decision branch + fingerprint-change detection + corrupt-manifest recovery + sortSources.
Updated `runnerArgs` to take a `Decision`; 8 unit tests in `SymbolProcessorResolverTest` cover the new arg layout per decision.

## 22. Shared vs per-variant KSP output split landed

The earlier discussion about separating user-source-like KSP outputs (shared across variants) from build-state-like outputs (per-variant) is now implemented.
`SymbolProcessorResolver.resolve` takes two base paths:

- `kspSharedOutputBaseDir = .bleep/projects/<cross>/generated-sources/ksp/` — `kotlin/`, `java/`, `resources/`
- `kspVariantStateDir = .bleep/projects/<cross>/builds/<variant>/ksp/` — `classes/`, `caches/`, `inputs-manifest.json`

Generated source files survive variant toggles and the IDE/CLI symmetry; the per-variant incremental cache is properly isolated so BSP and Normal don't
corrupt each other's KSP state.

## 23. Mixed-compile current state

`KspMixedCompileIT` pins the current behavior in three cases:

| Case | Status |
|---|---|
| Handwritten `.java` (leaf) + Kotlin | passes |
| Handwritten `.java` references Kotlin | fails — javac-first compile order doesn't see Kotlin classes; tracked as the broader Kotlin+Java mixed-compile work (CLAUDE.md memory `project_kotlin_java_mixed.md`) |
| KSP output dirs land in the right shared/per-variant locations | passes |

KSP-emits-Java end-to-end requires either Dagger/Hilt as a fixture or a custom toy processor; both are deferred to a follow-up. Once the broader mixed-compile
work lands, flip the `intercept[Throwable]` in `KspMixedCompileIT` to a success assertion.

## 20. M7 — what's covered, what's deferred

The §13 plan listed 15 cases A-O that all assumed a custom in-house KSP toy processor with per-invocation logging. That fixture (a published `bleep-test-ksp-processor` module) is non-trivial — Kotlin module, KSP SPI implementation, local-publish pipeline, multiple versions for upgrade tests. Deferring it would block M7 indefinitely, so this pass covers the indirectly-verifiable cases using **Moshi codegen** as the real processor.

**Shipped in `KspIncrementalIT`** (5 cases):

| Case | What it asserts | Signal |
|---|---|---|
| A | Cold compile generates JsonAdapter; KSP caches dir created | file existence |
| B | No-op recompile produces byte-identical output | SHA-256 equality |
| C | Add a second annotated source → new adapter on next compile | file existence on second compile |
| D | `bleep clean` wipes the KSP output tree | clean removes generated adapter |
| E | clean → recompile is byte-deterministic | SHA-256 equality across clean+rebuild |

**Deferred to a follow-up that introduces the toy processor**:

| §13 case | Why deferred |
|---|---|
| C `editUnrelatedSource` | Needs invocation counter to be meaningful with deterministic processor |
| E `editProcessorOption` | Moshi codegen has limited options; needs custom processor that respects options |
| F `changeProcessorVersion` | Needs two versions of a custom processor |
| G `changeKspVersion` | Slow; not all kotlin/ksp pairs valid |
| H `changeKotlinVersion` | Slow |
| I `bspDaemonRestart` | Harness can't kill the daemon mid-test |
| J `branchSwitch` | Needs git fixture |
| K `multiRound` | Needs custom processor with deferred symbols |
| L `processorThrows` | Needs custom processor that throws |
| M / N `add` / `remove` processor | Scope |
| O `mixedKotlinJava` | Blocked on the unshipped Kotlin+Java mixed-compile work |

**Discovered + fixed during M7**: KSP1's `-incremental=true` mode requires the caller to pass `-modified-sources` / `-removed-sources` / `-changed-classes` for KSP to know what changed; bleep doesn't track per-file changes between runs, and without those lists KSP silently treats "no info → no changes → skip new files." Test C exposed this (added Animal.kt was not processed even though kotlinc saw it). The fix is conservative: default `incremental = false` in `SymbolProcessorResolver`, accepting the perf cost of full re-processing each compile. Tracking modified/removed sources to enable incremental mode is a worthwhile follow-up.

## 19. Path (a) landed — what changed since §18

Pivot from "KSP as kotlinc compiler plugin" to "KSP as standalone runner" implemented and verified:

1. **`CompilerResolver.resolveKspPlugin`** now resolves `com.google.devtools.ksp:symbol-processing-aa-embeddable:<kotlin>-<ksp>` and returns the full transitive closure (not filtered). The runner JVM needs everything (the embeddable JAR bundles its own kotlinc Analysis API).
2. **`SymbolProcessorResolver`** produces a `SymbolProcessorResult` with `runnerClasspath`, `processorJars`, `librariesClasspath`, output dirs, and `processorOptions`. Method `runnerArgs` builds `KSPJvmMain`'s `-name=value` argv (no more `-P plugin:` kotlinc options).
3. **New `bleep.analysis.KspRunner`** forks `java -cp <runner-cp> com.google.devtools.ksp.cmdline.KSPJvmMain <args>`. Drains output, hooks into the per-task kill signal, surfaces `Success / Failure(exitCode, output) / Cancelled`.
4. **DAG task renamed** `ResolveSymbolProcessorsTask` → `RunSymbolProcessorsTask`. New dependency: every transitive upstream project's `CompileTask` so KSP's `-libraries=` sees their class dirs.
5. **All M4 kotlinc-side wiring reverted.** `BleepBuildConverter.toProjectConfig` and `resolveCompilerPlugins` back to their original signatures. The forced `-language-version=1.9` K1 fallback is gone — kotlinc compiles normally with K2.
6. **Source set unchanged** (M5 still in place). KSP emits to `.bleep/generated-sources/<cross>/ksp/{kotlin,java,resources}`; bleep's projectPaths exposes those dirs to subsequent kotlinc compile and `bleep clean`.
7. **Scan-mode fix**: when `scanForSymbolProcessors: true` finds a processor in the resolved deps, the processor classpath includes the entire resolved-dependencies set (not just the matched jar). KSP's standalone runner loads processors in an isolated URLClassLoader, so a processor's clinit references (e.g. moshi-kotlin-codegen needs `com.squareup.moshi.JsonClass`) must resolve on the same classloader.
8. **Known caveat — path separators**: KSP's CLI takes lists as `:`-separated on Unix / `;` on Windows. If a project's directory path itself contains a `:`, KSP misparses. The test harness's temp-dir names include the test name; tests with `:` in names break. Tracked as a future hardening item; users with normal filesystem paths are unaffected.

The renaming of events flows through BSP protocol (`Event.RunSymbolProcessors{Started,Finished}`), DagEvent, BuildEvent, TraceCategory, all displays. New code paths: `run-ksp:<project>` task ID prefix, `run-symbol-processors` trace category.

## 18. Discovered constraint: KSP1 + Kotlin 2.x frontend mismatch (blocks M6 end-to-end)

The wiring through model → resolver → DAG → handler → kotlinc plugin classpath works correctly. Error-path ITs all pass (`kspMissingKspVersion`, `kspBadCoord`, `kspNoProcessorJars`). But the two end-to-end real-processor ITs (Moshi codegen) fail because **KSP1's `symbol-processing-cmdline` JAR registers via the legacy K1 `ComponentRegistrar` service file only**, not the K2 `CompilerPluginRegistrar`. With kotlinc 2.0+ defaulting to K2 frontend, the plugin loads but never gets invoked.

Verified by inspection:

| Plugin                                | K1 `ComponentRegistrar` | K2 `CompilerPluginRegistrar` |
|---------------------------------------|--------------------------|-------------------------------|
| `kotlin-allopen-compiler-plugin`      | ✓                        | ✓                             |
| `symbol-processing-cmdline`           | ✓                        | ✗                             |

Workarounds attempted:

1. Filter resolved JARs to `symbol-processing-*` only (avoid loading conflicting kotlin-compiler-embeddable). Done. Didn't help by itself.
2. Inject `-language-version=1.9` / `-api-version=1.9` to force K1 mode. Done. Didn't trigger plugin loading either; possibly the K2 frontend dispatch is hardcoded in K2JVMCompiler.exec at this version.

### Paths forward

a. **Use the KSP standalone runner (`symbol-processing-aa-embeddable`).** This is the K2-native Analysis-API-based KSP runner. It's NOT a kotlinc plugin: it runs as a separate Java program before kotlinc, reads sources via Analysis API, emits generated files, then kotlinc compiles original + generated. Different architecture, more work, but the future-forward path.

b. **Run KSP1 via a separate kotlinc invocation with K1 frontend explicitly forced.** Two-pass model: first kotlinc invocation runs KSP1 only (just plugin loading, no compilation) and produces generated sources; second kotlinc invocation does the actual compile with K2. Requires understanding how to invoke kotlinc in "KSP-only" mode.

c. **Wait for K2-compatible KSP1.** Vendor-side. Not in our control.

### Recommendation

Take **path (a) — KSP standalone runner** as the shippable approach. Architecturally similar to the existing `sourcegen` DAG step: KSP becomes a pre-compile codegen step running in a separate process, emitting `.kt` and `.java` files that the next kotlinc invocation picks up via the source set wiring already in place (M5). This decouples KSP from kotlinc's internal plugin API entirely and makes K1/K2 not a concern.

The current wiring (M1-M5) is mostly reusable for path (a):
- Model fields (kspVersion, symbolProcessors, options) unchanged.
- Resolver: switches from `symbol-processing-cmdline` to `symbol-processing-aa-embeddable` coord; result no longer includes kotlinc plugin opts.
- DAG: `ResolveSymbolProcessorsTask` stays; a new `RunKspTask` runs the standalone KSP main, much like a sourcegen task.
- Source set: unchanged (KSP still emits to `.bleep/generated-sources/<cross>/ksp/`).
- Compile wiring: kotlinc no longer needs KSP plugin classpath/options; M4's threading becomes unused (revert).

What changes:
- New `bleep.analysis.KspRunner` that forks the standalone KSP main.
- DAG: replace `ResolveSymbolProcessorsTask` → `RunKspTask` (resolution + processor execution in one task, like Sourcegen).
- The K1/K2 frontend question goes away.

### Decision required

Continue on path (a) (architectural pivot, ~2-3 more days) or pause/ship the M1-M5 plumbing as a foundation and revisit later when KSP2 is GA-tagged.

---

## 24. Toy processor fixture landed

The `bleep-test-ksp-processor` project is now in-tree (`bleep-test-ksp-processor/src/{kotlin,resources}/`).
It exposes three annotations consumed by KSP integration tests:

- `@GenerateKotlin(suffix)` — emit a sibling `.kt` class
- `@GenerateJava(suffix)` — emit a sibling `.java` class, so the KSP→javac mixed-compile path has
  a deterministic fixture (vs. relying on Dagger/Hilt)
- `@ThrowOnMe` — make `process()` throw, exercising bleep's processor-error surface

Resolution wiring: tests reference the fixture as `build.bleep:bleep-test-ksp-processor:${BLEEP_VERSION}`,
and `BleepDevDeps` resolves dev versions to (a) the project's compiled `classes/` and (b) its
`src/resources/` directory. The resources path is needed because KSP's standalone runner discovers
processors via `META-INF/services/...SymbolProcessorProvider`, which lives in `src/resources/`.
Adding the source-resources path to `resolveAllClassDirs` is a general fix that also mirrors how a
published Maven artifact would expose resources to consumers; the synthetic-artifact code in
`CoursierResolver.TemplatedVersions` does not need changes.

New suite `KspToyProcessorIT` (4 tests) exercises:

| # | Case | Validates |
|---|---|---|
| 1 | `@GenerateKotlin` round-trip | dev-deps wiring + KSP→kotlinc |
| 2 | `@GenerateJava` mixed-compile | KSP-emits-Java → javac → Kotlin references generated symbol |
| 3 | `@ThrowOnMe` surfaces failure | processor-error compile-failure path |
| 4 | `symbolProcessorOptions` reach processor | option pass-through via toy-processor invocation log |

Each test gates on `requireToyProcessorBuilt()` which checks that `.bleep/projects/bleep-test-ksp-processor/builds/normal/classes` exists. Until layout v2 is deployed in
the running bleep CLI, the toy processor's classes live at the legacy path and the tests cancel
themselves with a clear message rather than producing a misleading hard failure. Once the layout-v2
deploy cycle (sourcegen → compile → my-publish-local → re-deploy CLI) runs, the tests start
passing without any source change.

Several §13 / §20 cases (invocation counting, multi-round, delete-source orphan cleanup) are still
not covered by ITs — they need processor-options threading through templating or harness
support for mid-test model reload, both deferred.

---

## 25. KSP for Kotlin/JS and Kotlin/Native — blocked on KSP upstream, not bleep

This was previously catalogued as "stage as a follow-up". A sharper read of the upstream
situation, after closer inspection of the runner artifact and the K1/K2 transition:

- **KSP2 (the standalone Analysis-API-based runner we ship)** has only one platform-main today:
  `com.google.devtools.ksp.cmdline.KSPJvmMain`. There is no `KSPJsMain` / `KSPNativeMain`
  shipped as part of `symbol-processing-aa-embeddable`. Routing won't help — the targets
  don't exist.
- **KSP1 (`symbol-processing` kotlinc plugin)** does have JS and Native variants, but it
  registers via the legacy K1 `ComponentRegistrar` SPI only (see §18). Bleep's `kotlinc 2.x`
  invocation defaults to the K2 frontend, where K1 plugins silently don't fire. This is the
  exact pitfall that forced the M6 pivot from KSP1 to KSP2; adding it back for JS/Native
  alone would resurrect the K1/K2 mismatch on those targets.
- **Kotlin Multiplatform's own resolution** is to route JVM through KSP2 and JS/Native through
  KSP1+K1 with separate kotlinc invocations targeting the older frontend. That's a 2x
  invocation-path bifurcation per Kotlin module, plus K1-frontend bleep wiring we don't have.

Net: this is not a "spend a day routing" item, it is **wait for KSP2 to grow JS/Native targets
upstream**. The Google/JetBrains roadmap for KSP2 is the gating event. When `KSPJsMain` and
`KSPNativeMain` appear in `symbol-processing-aa-embeddable`, the bleep side reduces to a
platform-dispatch tweak in `KspRunner` and per-target output-dir conventions. Until then,
bleep validates `kotlin.kspVersion` against JVM-only and rejects Kotlin/JS or Kotlin/Native
projects that set `symbolProcessors`. (Currently the JVM-only contract is implicit because
`SymbolProcessorResolver` is wired only through the JVM platform branch — adding an explicit
fail-fast in `Kotlin`-platform validation is a worthwhile small follow-up.)

