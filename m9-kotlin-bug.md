# Bug: bleep 1.0.0-M9 fails to compile Kotlin 2.3.0 projects (ClassCastException)

## Summary

Upgrading bleep from `1.0.0-M3` to `1.0.0-M9` breaks Kotlin compilation
for any project on Kotlin 2.3.0. Every Kotlin tester fails immediately
with a `ClassCastException` thrown from inside Kotlin's
`CommonCompilerArgumentsConfigurator`. Reverting to `1.0.0-M3` makes
the same projects compile cleanly with no other changes.

## Environment

- bleep `1.0.0-M9` (works on `1.0.0-M3`)
- Kotlin `2.3.0`, `jvmTarget: "21"`
- JVM `temurin:21`
- macOS 24.1.0 (arm64), zsh
- Reproduced on a clean BSP server (all bleep processes killed,
  cache untouched, fresh start).

## Symptom

Building a Kotlin project (no source changes) fails with:

```
java.lang.ClassCastException: class java.lang.String cannot be cast to
  class org.jetbrains.kotlin.cli.common.arguments.ManualLanguageFeatureSetting
  (java.lang.String is in module java.base of loader 'bootstrap';
   ManualLanguageFeatureSetting is in unnamed module of loader
   java.net.URLClassLoader @214d2c64)
    at org.jetbrains.kotlin.cli.common.arguments
        .CommonCompilerArgumentsConfigurator
        .configureLanguageFeaturesFromInternalArgs(
            CommonCompilerArgumentsConfigurator.kt:100)
    at ...CommonCompilerArgumentsConfigurator.configureLanguageFeatures(
            CommonCompilerArgumentsConfigurator.kt:77)
    at ...K2JVMCompilerArgumentsConfigurator.configureLanguageFeatures(
            K2JVMCompilerArgumentsConfigurator.kt:77)
    at ...CommonCompilerArgumentsConfiguratorKt.toLanguageVersionSettings(
            CommonCompilerArgumentsConfigurator.kt:224)
    at ...ArgumentsKt.setupLanguageVersionSettings(arguments.kt:108)
    at ...ArgumentsKt.setupCommonArguments(arguments.kt:77)
    at ...AbstractConfigurationPhase.setupCommonConfiguration(
            AbstractConfigurationPhase.kt:67)
    at ...K2JVMCompiler.doExecutePhased(K2JVMCompiler.kt:51)
    ...
    at bleep.analysis.KotlinProjectCompiler$
        .compileKotlin$$anonfun$1(ProjectCompiler.scala:277)
```

The bleep call site is `bleep.analysis.KotlinProjectCompiler` line 277.

## Likely cause

Kotlin 2.3.0's `CommonCompilerArgumentsConfigurator` reads
`internalArguments` and casts each entry to
`ManualLanguageFeatureSetting`. M9 (or its bundled Kotlin compiler
adapter) appears to be putting raw `String` values into that field —
something compatible with the older Kotlin API but incompatible with
2.3.0's expected type.

Mismatch between bleep's Kotlin compiler-args plumbing and Kotlin
2.3.0's internal API.

## Reproduce

1. Project with Kotlin set to:
   ```yaml
   template-kotlin:
     kotlin:
       version: 2.3.0
       jvmTarget: "21"
   ```
2. `$version: 1.0.0-M9` in `bleep.yaml`.
3. `bleep compile <any-kotlin-project>` (clean cache; doesn't matter).
4. Crash above.

Switching `$version: 1.0.0-M3` (no other change) → compiles fine.

## Workaround

Pin bleep to `1.0.0-M3` until M9 is fixed.

## Notes

- This is not a stale-BSP issue: I killed every bleep process before
  retrying with M9 and got the same crash on the first invocation.
- Affects every Kotlin project in the repo (`testers/{db2,duckdb,
  mariadb,oracle,pg,sqlserver,combined}/kotlin`) — all 7 Kotlin
  testers go red identically.
- Scala compilation under M9 is fine; only Kotlin breaks.
- Repo: typr-3 RC6 migration branch.

## What I'd expect

bleep M9 to compile Kotlin 2.3.0 sources the same way M3 does, or to
fail at config time with a clear "Kotlin 2.3.0 not supported, use
2.x.y" message rather than a cast deep inside the compiler.
