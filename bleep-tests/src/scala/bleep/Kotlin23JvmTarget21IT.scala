package bleep

/** Regression repro for the M9 + Kotlin 2.3.0 ClassCastException documented in `m9-kotlin-bug.md` at the repo root.
  *
  * The trigger is the combination of:
  *   - Kotlin 2.3.0
  *   - any non-empty `kotlin.options:` value (here: `-Xskip-prerelease-check`, copied from typr-3's Kotlin testers where this was first noticed)
  *
  * Path: with options present, `KotlinSourceCompiler#applyAdditionalOptions` tries `CLITool.parseArguments` reflectively. The signature lookup fails on Kotlin
  * 2.3.0 (the broad `case _: Exception`) and the fallback shoves the raw option string into `K2JVMCompilerArguments.setInternalArguments(List<String>)`. Kotlin
  * 2.3.0 expects `List<InternalArgument>` there; later, when `CommonCompilerArgumentsConfigurator.configureLanguageFeaturesFromInternalArgs` casts each entry
  * to `ManualLanguageFeatureSetting`, the `ClassCastException` fires.
  *
  * Drop the `options:` field from the YAML below and the bug stops reproducing &mdash; that's the canary.
  */
class Kotlin23JvmTarget21IT extends IntegrationTestHarness {
  integrationTest("Kotlin 2.3.0 with jvmTarget 21 and -Xskip-prerelease-check (M9 bug repro)") { ws =>
    ws.yaml(
      content = s"""projects:
                   |  myapp:
                   |    kotlin:
                   |      jvmTarget: "21"
                   |      version: ${model.VersionKotlin.Kotlin23.kotlinVersion}
                   |      options: -Xskip-prerelease-check
                   |    platform:
                   |      name: jvm
                   |      mainClass: com.example.MainKt
                   |""".stripMargin
    )

    ws.file(
      "myapp/src/kotlin/com/example/Main.kt",
      content = """package com.example
                  |
                  |fun main() {
                  |  println("Hello from Kotlin ${KotlinVersion.CURRENT}")
                  |}
                  |""".stripMargin
    )

    ws.compileAll()
    succeed
  }
}
