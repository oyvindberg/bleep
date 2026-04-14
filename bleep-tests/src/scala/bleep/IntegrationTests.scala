package bleep

import bleep.bsp.{BspServerClasspathSource, InProcessBspServer}
import bleep.internal.{bleepLoggers, FileUtils}
import cats.data.NonEmptyList
import org.scalactic.TripleEqualsSupport
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite
import ryddig.*

import java.nio.file.{Files, Path}
import java.time.Instant
import java.util.jar.JarOutputStream
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

class IntegrationTests extends AnyFunSuite with TripleEqualsSupport {
  val userPaths = UserPaths.fromAppDirs
  val logger0 = Loggers.decodeJsonStream(
    bleepLoggers.BLEEP_JSON_EVENT,
    Loggers
      .stdout(LogPatterns.interface(Some(Instant.now), noColor = false), disableProgress = true)
      .acquire()
      .value
      .withMinLogLevel(LogLevel.info)
  )
  val ec: ExecutionContextExecutor = ExecutionContext.global

  val lazyBleepBuild: Lazy[Started] =
    Lazy {
      val existing = BuildLoader.find(FileUtils.cwd).existing.orThrow
      val pre = Prebootstrapped(logger0, userPaths, BuildPaths(FileUtils.cwd, existing, model.BuildVariant.Normal), existing, ec)
      bootstrap.from(pre, ResolveProjects.InMemory, Nil, model.BleepConfig.default, CoursierResolver.Factory.default).orThrow
    }

  val prelude =
    s"""$$schema: https://raw.githubusercontent.com/oyvindberg/bleep/master/schema.json
      |$$version: dev
      |jvm:
      |  name: ${model.Jvm.graalvm.name}
      |""".stripMargin

  // note: passing stored log messages is a hack for now. soon commands will return values, and `run` for instance will return printed lines
  // Use NewEachInvocation mode so each test gets its own BSP server, avoiding socket conflicts
  val testConfig: model.BleepConfig = model.BleepConfig(
    compileServerMode = Some(model.CompileServerMode.NewEachInvocation),
    authentications = None,
    logTiming = None,
    bspServerConfig = None,
    remoteCacheCredentials = None
  )

  def runTest(testName: String, yaml: String, files: Map[RelPath, String])(f: (Started, Commands, TypedLogger[Array[Stored]]) => Assertion): Assertion = {
    val storingLogger = Loggers.storing()
    val stdLogger = logger0.withContext("testName", testName)
    val testTempFolder = Files.createTempDirectory(s"bleep-test-$testName")

    val withBuildScript = files.updated(RelPath.of(BuildLoader.BuildFileName), prelude ++ yaml)
    FileSync.syncStrings(testTempFolder, withBuildScript, FileSync.DeleteUnknowns.No, soft = false).discard()
    val existingBuild = BuildLoader.find(testTempFolder).existing.orThrow
    val buildPaths = BuildPaths(cwd = testTempFolder, existingBuild, model.BuildVariant.Normal)

    try {
      val started = bootstrap
        .from(
          Prebootstrapped(storingLogger.zipWith(stdLogger), userPaths, buildPaths, existingBuild, ec),
          ResolveProjects.ReplaceBleepDependencies(lazyBleepBuild, BspServerClasspathSource.InProcess(InProcessBspServer.connect)),
          Nil,
          testConfig,
          CoursierResolver.Factory.default
        )
        .orThrow
      val commands = new Commands(started)
      val ret = f(started, commands, storingLogger)
      FileUtils.deleteDirectory(testTempFolder)
      ret
    } finally
      stdLogger.info(s"Ran in $testTempFolder")
  }

  test("run prefer jvmRuntimeOptions") {
    assume(!sys.env.contains("CI"), "Skipped on CI: spawns child JVMs that exceed runner memory")
    runTest(
      "run prefer jvmRuntimeOptions",
      """projects:
      a:
        platform:
          name: jvm
          jvmRuntimeOptions: -Dfoo=2
          jvmOptions: -Dfoo=1
          mainClass: test.Main
        scala:
          version: 3.4.2
""",
      Map(
        RelPath.force("./a/src/scala/Main.scala") ->
          """package test
        |object Main {
        |  def main(args: Array[String]): Unit =
        |    println("foo was: " + sys.props("foo"))
        |}""".stripMargin
      )
    ) { (_, commands, storingLogger) =>
      commands.run(model.CrossProjectName(model.ProjectName("a"), None))
      assert(storingLogger.underlying.exists(_.message.plainText == "foo was: 2"))
    }
  }
  test("annotation processing disabled by default") {
    runTest(
      "annotation processing disabled by default",
      """projects:
        |  a:
        |    dependencies: org.projectlombok:lombok:1.18.30
        |    source-layout: java
        |    java:
        |      options: -Xlint:all
        |""".stripMargin,
      Map(
        RelPath.force("./a/src/main/java/test/Person.java") ->
          """package test;
            |
            |import lombok.Data;
            |
            |@Data
            |public class Person {
            |    private String name;
            |    private int age;
            |}""".stripMargin
      )
    ) { (started, commands, storingLogger) =>
      val projectName = model.CrossProjectName(model.ProjectName("a"), None)
      val resolved = started.resolvedProjects(projectName).forceGet("test")

      // Should have -proc:none in java options
      val javaOptions = resolved.language.javaOptions
      assert(javaOptions.contains("-proc:none"))
      assert(javaOptions.contains("-Xlint:all"))

      // Generated sources directory should not be in sources
      assert(!resolved.sources.exists(_.toString.contains("generated-sources")))
    }
  }

  test("annotation processing enabled") {
    runTest(
      "annotation processing enabled",
      """projects:
        |  a:
        |    dependencies: org.projectlombok:lombok:1.18.30
        |    source-layout: java
        |    java:
        |      options: -Xlint:all
        |      annotationProcessing:
        |        enabled: true
        |""".stripMargin,
      Map(
        RelPath.force("./a/src/main/java/test/Person.java") ->
          """package test;
            |
            |import lombok.Data;
            |
            |@Data
            |public class Person {
            |    private String name;
            |    private int age;
            |}""".stripMargin
      )
    ) { (started, commands, storingLogger) =>
      val projectName = model.CrossProjectName(model.ProjectName("a"), None)
      val resolved = started.resolvedProjects(projectName).forceGet("test")

      // Should have -s option with generated sources directory
      val javaOptions = resolved.language.javaOptions
      assert(javaOptions.exists(_.contains("-s")))
      assert(javaOptions.exists(_.contains("generated-sources")))
      assert(javaOptions.exists(_.contains("annotations")))
      assert(!javaOptions.contains("-proc:none"))
      assert(javaOptions.contains("-Xlint:all"))

      // Generated sources directory should be in sources
      assert(resolved.sources.exists(_.toString.contains("generated-sources")))
    }
  }

  test("run fallback to jvmOptions") {
    assume(!sys.env.contains("CI"), "Skipped on CI: spawns child JVMs that exceed runner memory")
    runTest(
      "run fallback to jvmOptions",
      """projects:
      a:
        platform:
          name: jvm
          jvmOptions: -Dfoo=1
          mainClass: test.Main
        scala:
          version: 3.4.2
""",
      Map(
        RelPath.force("./a/src/scala/Main.scala") ->
          """package test
        |object Main {
        |  def main(args: Array[String]): Unit =
        |    println("foo was: " + sys.props("foo"))
        |}""".stripMargin
      )
    ) { (_, commands, storingLogger) =>
      commands.run(model.CrossProjectName(model.ProjectName("a"), None))
      assert(storingLogger.underlying.exists(_.message.plainText == "foo was: 1"))
    }
  }

  // Scala 3.8 introduces a major change: the standard library is now compiled with Scala 3
  // instead of Scala 2.13. This test verifies that bleep can handle Scala 3.8 projects correctly,
  // including the new scala-library versioning (3.x instead of 2.13.x).
  // See: https://docs.scala-lang.org/sips/drop-stdlib-forwards-bin-compat.html
  test("scala 3.8.1 with new stdlib") {
    assume(!sys.env.contains("CI"), "Skipped on CI: spawns child JVMs that exceed runner memory")
    runTest(
      "scala 3.8.1 with new stdlib",
      """projects:
      a:
        platform:
          name: jvm
          mainClass: test.Main
        scala:
          version: 3.8.1
""",
      Map(
        RelPath.force("./a/src/scala/Main.scala") ->
          """package test
        |object Main {
        |  def main(args: Array[String]): Unit = {
        |    // Use stdlib collections to verify they work
        |    val list = List(1, 2, 3).map(_ * 2)
        |    println("result: " + list.sum)
        |  }
        |}""".stripMargin
      )
    ) { (_, commands, storingLogger) =>
      commands.run(model.CrossProjectName(model.ProjectName("a"), None))
      assert(storingLogger.underlying.exists(_.message.plainText == "result: 12"))
    }
  }

  test("resource generator") {
    // This test spawns child JVMs (bleep run) which, combined with the in-process BSP server,
    // exceeds the 7GB memory limit on CI runners and gets OOM-killed (exit code 137).
    assume(!sys.env.contains("CI"), "Skipped on CI: requires more memory than CI runners provide")
    val bleepYaml = """
projects:
  a:
    extends: common
    platform:
      mainClass: test.Main
    sourcegen: scripts/testscripts.SourceGen
  scripts:
    extends: common
    dependencies: build.bleep::bleep-core:${BLEEP_VERSION}
templates:
  common:
    platform:
      name: jvm
    scala:
      version: 3.8.3
"""

    val Main = """
package test

object Main {
  def main(args: Array[String]): Unit =
    println("result: " + testgenerated.GeneratedSource.result)
}
"""

    val SourceGen = """
package testscripts

import bleep.*
import java.nio.file.Files

object SourceGen extends BleepCodegenScript("SourceGen") {
  def run(started: Started, commands: Commands, targets: List[Target], args: List[String]): Unit = {
    targets.foreach { target =>
      val targetFile = target.sources / "testgenerated" / "GeneratedSource.scala"
      Files.createDirectories(targetFile.getParent)
      Files.writeString(
        targetFile,
        "package testgenerated;\n" +
        "\n" +
        "object GeneratedSource {" +
        "  val result = 100" +
        "}"
      )
      started.logger.withContext("targetFile", targetFile).warn("Wrote ")
    }
  }
}
"""

    runTest(
      "resource generator spike",
      bleepYaml,
      Map(
        RelPath.force("./a/src/scala/test/Main.scala") -> Main,
        RelPath.force("./scripts/src/scala/testscripts/SourceGen.scala") -> SourceGen
      )
    ) { (_, commands, storingLogger) =>
      commands.run(model.CrossProjectName(model.ProjectName("a"), None))
      assert(storingLogger.underlying.exists(_.message.plainText == "result: 100"))
    }
  }

  test("test --only works before compilation") {
    runTest(
      "test --only works before compilation",
      // language=yaml
      """projects:
        |  mytest:
        |    dependencies: org.scalatest::scalatest:3.2.15
        |    isTestProject: true
        |    platform:
        |      name: jvm
        |    scala:
        |      version: 3.3.3
        |""".stripMargin,
      Map(
        RelPath.of("mytest/src/scala/MyTest.scala") ->
          // language=scala
          """package example
            |
            |import org.scalatest.funsuite.AnyFunSuite
            |
            |class MyTest extends AnyFunSuite {
            |  test("dummy test") {
            |    assert(1 + 1 == 2)
            |  }
            |}
            |""".stripMargin
      )
    ) { (started, commands, storingLogger) =>
      // commands.test() throws (via force/orThrow) if the BSP server returns non-OK status.
      // Completing without exception means tests ran and passed.
      commands.test(
        projects = List(model.CrossProjectName(model.ProjectName("mytest"), None)),
        watch = false,
        only = Some(NonEmptyList.of("MyTest")),
        exclude = None
      )
      succeed
    }
  }

  /** Create an empty jar file at the given path */
  private def createEmptyJar(jarPath: Path): Unit = {
    Files.createDirectories(jarPath.getParent)
    val out = new JarOutputStream(Files.newOutputStream(jarPath))
    out.close()
  }

  test("unmanaged jars on classpath") {
    val testName = "unmanaged jars on classpath"
    val storingLogger = Loggers.storing()
    val stdLogger = logger0.withContext("testName", testName)
    val testTempFolder = Files.createTempDirectory(s"bleep-test-$testName")

    // Create jar files before syncing the bleep build
    createEmptyJar(testTempFolder.resolve("lib/foo.jar"))
    createEmptyJar(testTempFolder.resolve("lib/bar.jar"))

    val yaml =
      // language=yaml
      """projects:
        |  myapp:
        |    jars:
        |      - lib/foo.jar
        |      - lib/bar.jar
        |    platform:
        |      name: jvm
        |    scala:
        |      version: 3.3.3
        |""".stripMargin

    val files = Map(
      RelPath.force("./myapp/src/scala/Main.scala") ->
        """package test
          |object Main {
          |  def main(args: Array[String]): Unit = println("ok")
          |}
          |""".stripMargin
    )

    val withBuildScript = files.updated(RelPath.of(BuildLoader.BuildFileName), prelude ++ yaml)
    FileSync.syncStrings(testTempFolder, withBuildScript, FileSync.DeleteUnknowns.No, soft = false).discard()
    val existingBuild = BuildLoader.find(testTempFolder).existing.orThrow
    val buildPaths = BuildPaths(cwd = testTempFolder, existingBuild, model.BuildVariant.Normal)

    try {
      val started = bootstrap
        .from(
          Prebootstrapped(storingLogger.zipWith(stdLogger), userPaths, buildPaths, existingBuild, ec),
          ResolveProjects.ReplaceBleepDependencies(lazyBleepBuild, BspServerClasspathSource.InProcess(InProcessBspServer.connect)),
          Nil,
          testConfig,
          CoursierResolver.Factory.default
        )
        .orThrow
      val projectName = model.CrossProjectName(model.ProjectName("myapp"), None)

      // Verify the project model parsed jars from YAML
      val exploded = started.build.explodedProjects(projectName)
      assert(exploded.jars.values.size === 2, s"Expected 2 jars, got: ${exploded.jars.values}")

      // Verify both jars are on the resolved classpath
      val resolved = started.resolvedProjects(projectName).forceGet("test")
      val fooJar = resolved.classpath.find(_.toString.contains("foo.jar"))
      val barJar = resolved.classpath.find(_.toString.contains("bar.jar"))
      assert(fooJar.isDefined, s"Expected foo.jar on classpath, got: ${resolved.classpath.mkString(", ")}")
      assert(barJar.isDefined, s"Expected bar.jar on classpath, got: ${resolved.classpath.mkString(", ")}")
      assert(Files.exists(fooJar.get), s"Jar file does not exist: ${fooJar.get}")
      assert(Files.exists(barJar.get), s"Jar file does not exist: ${barJar.get}")
      succeed
    } finally {
      stdLogger.info(s"Ran in $testTempFolder")
      FileUtils.deleteDirectory(testTempFolder)
    }
  }

  test("kotlin compilation") {
    runTest(
      "kotlin compilation",
      """projects:
        |  myapp:
        |    source-layout: kotlin
        |    kotlin:
        |      version: 2.1.20
        |    platform:
        |      name: jvm
        |""".stripMargin,
      Map(
        RelPath.force("./myapp/src/main/kotlin/test/Hello.kt") ->
          """package test
            |
            |fun greet(name: String): String = "Hello, $name!"
            |""".stripMargin
      )
    ) { (started, commands, _) =>
      commands.compile(List(model.CrossProjectName(model.ProjectName("myapp"), None)))
      succeed
    }
  }

  test("kotlin test with JUnit") {
    runTest(
      "kotlin test with JUnit",
      """projects:
        |  myapp:
        |    source-layout: kotlin
        |    kotlin:
        |      version: 2.1.20
        |    platform:
        |      name: jvm
        |  myapp-test:
        |    dependencies:
        |    - com.github.sbt:junit-interface:0.13.3
        |    - org.jetbrains.kotlin:kotlin-test-junit:2.1.20
        |    dependsOn: myapp
        |    isTestProject: true
        |    source-layout: kotlin
        |    kotlin:
        |      version: 2.1.20
        |    platform:
        |      name: jvm
        |    testFrameworks: com.novocode.junit.JUnitFramework
        |""".stripMargin,
      Map(
        RelPath.force("./myapp/src/main/kotlin/test/Hello.kt") ->
          """package test
            |
            |fun greet(name: String): String = "Hello, $name!"
            |""".stripMargin,
        RelPath.force("./myapp-test/src/test/kotlin/test/HelloTest.kt") ->
          """package test
            |
            |import org.junit.Test
            |import kotlin.test.assertEquals
            |
            |class HelloTest {
            |    @Test
            |    fun testGreet() {
            |        assertEquals("Hello, World!", greet("World"))
            |    }
            |}
            |""".stripMargin
      )
    ) { (_, commands, _) =>
      commands.test(
        projects = List(model.CrossProjectName(model.ProjectName("myapp-test"), None)),
        watch = false,
        only = None,
        exclude = None
      )
      succeed
    }
  }

  test("kotlin internal visibility with friend-paths") {
    runTest(
      "kotlin internal visibility with friend-paths",
      """projects:
        |  mylib:
        |    source-layout: kotlin
        |    kotlin:
        |      version: 2.1.20
        |    platform:
        |      name: jvm
        |  mylib-test:
        |    dependencies:
        |    - com.github.sbt:junit-interface:0.13.3
        |    - org.jetbrains.kotlin:kotlin-test-junit:2.1.20
        |    dependsOn: mylib
        |    isTestProject: true
        |    source-layout: kotlin
        |    kotlin:
        |      version: 2.1.20
        |    platform:
        |      name: jvm
        |    testFrameworks: com.novocode.junit.JUnitFramework
        |""".stripMargin,
      Map(
        RelPath.force("./mylib/src/main/kotlin/mylib/Internal.kt") ->
          """package mylib
            |
            |internal fun secretGreet(name: String): String = "Secret hello, $name!"
            |""".stripMargin,
        RelPath.force("./mylib-test/src/test/kotlin/mylib/InternalTest.kt") ->
          """package mylib
            |
            |import org.junit.Test
            |import kotlin.test.assertEquals
            |
            |class InternalTest {
            |    @Test
            |    fun testInternalAccess() {
            |        assertEquals("Secret hello, World!", secretGreet("World"))
            |    }
            |}
            |""".stripMargin
      )
    ) { (_, commands, _) =>
      commands.test(
        projects = List(model.CrossProjectName(model.ProjectName("mylib-test"), None)),
        watch = false,
        only = None,
        exclude = None
      )
      succeed
    }
  }

  test("kotlin compiler plugins (allopen)") {
    runTest(
      "kotlin compiler plugins (allopen)",
      """projects:
        |  myapp:
        |    source-layout: kotlin
        |    kotlin:
        |      version: 2.1.20
        |      compilerPlugins:
        |      - allopen
        |      options: -P plugin:org.jetbrains.kotlin.allopen:annotation=myapp.MyOpen
        |    platform:
        |      name: jvm
        |  myapp-test:
        |    dependencies:
        |    - com.github.sbt:junit-interface:0.13.3
        |    - org.jetbrains.kotlin:kotlin-test-junit:2.1.20
        |    dependsOn: myapp
        |    isTestProject: true
        |    source-layout: kotlin
        |    kotlin:
        |      version: 2.1.20
        |    platform:
        |      name: jvm
        |    testFrameworks: com.novocode.junit.JUnitFramework
        |""".stripMargin,
      Map(
        RelPath.force("./myapp/src/main/kotlin/myapp/MyOpen.kt") ->
          """package myapp
            |
            |annotation class MyOpen
            |""".stripMargin,
        RelPath.force("./myapp/src/main/kotlin/myapp/Service.kt") ->
          """package myapp
            |
            |// The allopen plugin should make this class open (non-final)
            |@MyOpen
            |class MyService {
            |    fun greet(name: String): String = "Hello, $name!"
            |}
            |""".stripMargin,
        RelPath.force("./myapp-test/src/test/kotlin/myapp/ServiceTest.kt") ->
          """package myapp
            |
            |import org.junit.Test
            |import kotlin.test.assertEquals
            |
            |// This subclass would fail to compile without allopen plugin
            |// because Kotlin classes are final by default
            |class TestService : MyService() {
            |    @Test
            |    fun testGreet() {
            |        assertEquals("Hello, World!", greet("World"))
            |    }
            |}
            |""".stripMargin
      )
    ) { (_, commands, _) =>
      commands.test(
        projects = List(model.CrossProjectName(model.ProjectName("myapp-test"), None)),
        watch = false,
        only = None,
        exclude = None
      )
      succeed
    }
  }
}
