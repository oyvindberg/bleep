package bleep

import java.nio.file.Files

/** End-to-end tests for the Kotlin + Java mixed-compile path, with and without KSP in the picture.
  *
  * The interesting cases:
  *
  *   - Handwritten `.java` alongside `.kt`, the `.java` is leaf (no references to Kotlin): javac runs first, kotlinc reads the resulting classes. This is the
  *     classic mixed-compile flow and bleep handles it today.
  *   - Handwritten `.java` references a Kotlin type: javac-first would fail (no Kotlin classes yet). Needs kotlinc-first or stubs.
  *   - KSP emits `.java` that references the original Kotlin: same shape as the above. The KSP plumbing puts the emitted files on the source set; the compile
  *     order is the open question.
  *
  * These tests document the current behaviour and pin the limitation so anyone fixing the broader mixed-compile work knows what to validate.
  */
class KspMixedCompileIT extends IntegrationTestHarness {

  private val kotlinVersion = "2.1.20"
  private val kspVersion = "1.0.32"
  private val moshi = "com.squareup.moshi:moshi:1.15.0"
  private val moshiCodegen = "com.squareup.moshi:moshi-kotlin-codegen:1.15.0"

  private def projectName: model.CrossProjectName = model.CrossProjectName(model.ProjectName("myapp"), None)

  integrationTest("baseline — handwritten Java + Kotlin where Java is leaf compiles") { ws =>
    ws.yaml(
      s"""projects:
         |  myapp:
         |    source-layout: kotlin
         |    kotlin:
         |      version: $kotlinVersion
         |    platform:
         |      name: jvm
         |""".stripMargin
    )
    // A self-contained Java class (no references to the Kotlin code).
    ws.file(
      "myapp/src/java/myapp/Greeting.java",
      """package myapp;
        |public final class Greeting {
        |  public static String hello(String name) { return "Hello, " + name + "!"; }
        |}
        |""".stripMargin
    )
    // Kotlin code uses the Java class.
    ws.file(
      "myapp/src/kotlin/myapp/Main.kt",
      """package myapp
        |
        |fun main() { println(Greeting.hello("World")) }
        |""".stripMargin
    )
    val (_, commands, _) = ws.start()
    commands.compile(List(projectName))
    succeed
  }

  integrationTest("handwritten Java references Kotlin — kotlinc-first compile order resolves cross-language refs") { ws =>
    ws.yaml(
      s"""projects:
         |  myapp:
         |    source-layout: kotlin
         |    kotlin:
         |      version: $kotlinVersion
         |    platform:
         |      name: jvm
         |""".stripMargin
    )
    ws.file(
      "myapp/src/kotlin/myapp/Person.kt",
      """package myapp
        |
        |class Person(val name: String)
        |""".stripMargin
    )
    // Java code that references the Kotlin type. Phase 1 (kotlinc) compiles Kotlin and reads the Java source for symbols. Phase 2 (javac) compiles Java with
    // kotlinc's output on the classpath, so Person is resolvable.
    ws.file(
      "myapp/src/java/myapp/PersonHolder.java",
      """package myapp;
        |public final class PersonHolder {
        |  public final Person person;
        |  public PersonHolder(Person p) { this.person = p; }
        |}
        |""".stripMargin
    )
    val (_, commands, _) = ws.start()
    commands.compile(List(projectName))
    succeed
  }

  integrationTest("KSP output dirs land in the expected shared and per-variant locations") { ws =>
    ws.yaml(
      s"""projects:
         |  myapp:
         |    source-layout: kotlin
         |    kotlin:
         |      version: $kotlinVersion
         |      kspVersion: $kspVersion
         |      symbolProcessors:
         |        - $moshiCodegen
         |    dependencies:
         |      - $moshi
         |    platform:
         |      name: jvm
         |""".stripMargin
    )
    ws.file(
      "myapp/src/kotlin/myapp/Person.kt",
      """package myapp
        |
        |import com.squareup.moshi.JsonClass
        |
        |@JsonClass(generateAdapter = true)
        |data class Person(val name: String, val age: Int)
        |""".stripMargin
    )

    val (started, commands, _) = ws.start()
    commands.compile(List(projectName))

    // Source-like outputs (kotlin/, java/, resources/) live under the cross-project's generated-sources dir.
    val kspBase = started.buildPaths.generatedSourcesDir(projectName, "ksp")
    val kotlinAdapter = kspBase.resolve("kotlin/myapp/PersonJsonAdapter.kt")
    assert(Files.exists(kotlinAdapter), s"expected PersonJsonAdapter.kt at $kotlinAdapter")
    assert(Files.exists(kspBase.resolve("java")), s"expected ksp/java directory at ${kspBase.resolve("java")} (may be empty for Moshi)")
    assert(Files.exists(kspBase.resolve("resources")), s"expected ksp/resources directory at ${kspBase.resolve("resources")}")

    // State / class outputs live under the per-variant build dir.
    val variantKsp = started.buildPaths.variantBuildDir(projectName).resolve("ksp")
    assert(Files.exists(variantKsp.resolve("classes")), s"expected ksp/classes directory at ${variantKsp.resolve("classes")}")
    // caches/ may or may not have content (depends on whether KSP wrote a cache this run), but the inputs-manifest written by the bleep tracker should exist
    assert(Files.exists(variantKsp.resolve("inputs-manifest.json")), s"expected inputs-manifest.json at ${variantKsp.resolve("inputs-manifest.json")}")
    succeed
  }
}
