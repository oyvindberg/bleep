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
    runTest(
      "run prefer jvmRuntimeOptions",
      """projects:
      a:
        platform:
          name: jvm
          jvmRuntimeOptions: -Xmx512m -Xms64m -Dfoo=2
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
  test("annotation processors: default off — Lombok in deps does not run") {
    runTest(
      "AP default off",
      """projects:
        |  a:
        |    dependencies: org.projectlombok:lombok:1.18.46
        |    source-layout: java
        |    java:
        |      options: -Xlint:all
        |""".stripMargin,
      Map(
        RelPath.force("./a/src/java/test/Person.java") ->
          """package test;
            |
            |public class Person {
            |    private String name;
            |    private int age;
            |}""".stripMargin
      )
    ) { (started, commands, storingLogger) =>
      val projectName = model.CrossProjectName(model.ProjectName("a"), None)
      val resolved = started.resolvedProjects(projectName).forceGet("test")
      val javaOptions = resolved.language.javaOptions
      assert(javaOptions.contains("-proc:none"), s"expected -proc:none in $javaOptions")
      assert(!javaOptions.exists(_.contains("-processorpath")), s"unexpected -processorpath in $javaOptions")
      assert(!javaOptions.exists(_.contains("-s")), s"unexpected -s in $javaOptions")
      assert(javaOptions.contains("-Xlint:all"))
      assert(!resolved.sources.exists(_.toString.contains("generated-sources")))
      assert(!storingLogger.underlying.exists(_.message.plainText.contains("auto-discovered annotation processor")))
    }
  }

  test("annotation processors: scanForAnnotationProcessors: true scans deps and runs Lombok end-to-end") {
    // Lombok bypasses JPMS via sun.misc.Unsafe (it does NOT use --add-opens, which is what
    // older docs claim is required). On JDK 25 + Lombok 1.18.46 + in-process javac via Zinc
    // it generates @Data members successfully — printed by `commands.run` at the bottom of
    // this test.
    runTest(
      "AP scan true",
      """projects:
        |  a:
        |    dependencies: org.projectlombok:lombok:1.18.46
        |    source-layout: java
        |    platform:
        |      name: jvm
        |      mainClass: test.Main
        |    scala:
        |      version: 3.4.2
        |    java:
        |      scanForAnnotationProcessors: true
        |""".stripMargin,
      Map(
        RelPath.force("./a/src/java/test/Person.java") ->
          """package test;
            |
            |import lombok.Data;
            |
            |@Data
            |public class Person {
            |    private String name;
            |    private int age;
            |
            |    public Person() {}
            |    public Person(String name, int age) { this.name = name; this.age = age; }
            |}""".stripMargin,
        RelPath.force("./a/src/java/test/Main.java") ->
          """package test;
            |
            |public class Main {
            |    public static void main(String[] args) {
            |        Person p = new Person("alice", 33);
            |        System.out.println(p.toString());
            |    }
            |}""".stripMargin
      )
    ) { (started, commands, storingLogger) =>
      val projectName = model.CrossProjectName(model.ProjectName("a"), None)
      val resolved = started.resolvedProjects(projectName).forceGet("test")

      // Bootstrap state: javaOptions does NOT contain AP flags here. Those are computed
      // by the AP DAG handler at compile time. Only the gen-sources reservation is visible
      // at bootstrap (since BuildPaths reserves it conservatively).
      assert(resolved.sources.exists(_.toString.contains("generated-sources")))

      // End-to-end: Lombok actually generated toString().
      commands.run(projectName, maybeOverriddenMain = Some("test.Main"))
      assert(
        storingLogger.underlying.exists(_.message.plainText == "Person(name=alice, age=33)"),
        "expected Lombok-generated @Data toString() to print"
      )
      // Auto-discovery log line is emitted by the AP DAG handler during the run.
      assert(
        storingLogger.underlying.exists { ev =>
          val txt = ev.message.plainText
          txt.contains("auto-discovered annotation processor JAR") && txt.contains("lombok")
        },
        "expected auto-discovered annotation processor JAR log line"
      )
    }
  }

  test("annotation processors: end-to-end with Mapstruct (pure JSR 269)") {
    // Mapstruct ships its annotations and processor as separate jars and registers via the
    // standard META-INF/services/javax.annotation.processing.Processor — exactly the pattern
    // bleep's auto-discovery and explicit lookup are designed for. This is the headline
    // "processors actually run" test.
    runTest(
      "AP mapstruct e2e",
      """projects:
        |  a:
        |    dependencies: org.mapstruct:mapstruct:1.5.5.Final
        |    source-layout: java
        |    platform:
        |      name: jvm
        |      mainClass: test.Main
        |    scala:
        |      version: 3.4.2
        |    java:
        |      annotationProcessors:
        |        - org.mapstruct:mapstruct-processor:1.5.5.Final
        |""".stripMargin,
      Map(
        RelPath.force("./a/src/java/test/User.java") ->
          """package test;
            |public class User {
            |    public String name;
            |    public int age;
            |    public User(String name, int age) { this.name = name; this.age = age; }
            |}""".stripMargin,
        RelPath.force("./a/src/java/test/UserDto.java") ->
          """package test;
            |public class UserDto {
            |    public String name;
            |    public int age;
            |}""".stripMargin,
        RelPath.force("./a/src/java/test/UserMapper.java") ->
          """package test;
            |import org.mapstruct.Mapper;
            |@Mapper
            |public interface UserMapper {
            |    UserDto userToUserDto(User user);
            |}""".stripMargin,
        RelPath.force("./a/src/java/test/Main.java") ->
          """package test;
            |public class Main {
            |    public static void main(String[] args) {
            |        UserMapper mapper = new UserMapperImpl();
            |        UserDto dto = mapper.userToUserDto(new User("alice", 33));
            |        System.out.println("name=" + dto.name + " age=" + dto.age);
            |    }
            |}""".stripMargin
      )
    ) { (started, commands, storingLogger) =>
      val projectName = model.CrossProjectName(model.ProjectName("a"), None)
      val resolved = started.resolvedProjects(projectName).forceGet("test")

      // mapstruct-processor must NOT be on the runtime classpath — annotationProcessors entries
      // resolve into a separate jar set used only for `-processorpath`. This assertion is
      // bootstrap-time state (resolvedProject.classpath), not AP-task output.
      val classpathPaths = resolved.classpath.map(_.toString)
      assert(
        !classpathPaths.exists(_.contains("mapstruct-processor")),
        s"mapstruct-processor must NOT be on runtime classpath, got $classpathPaths"
      )

      commands.run(projectName, maybeOverriddenMain = Some("test.Main"))
      assert(
        storingLogger.underlying.exists(_.message.plainText == "name=alice age=33"),
        "expected Mapstruct-generated UserMapperImpl to map correctly"
      )

      val genDir = resolved.sources
        .find(_.toString.contains("generated-sources"))
        .getOrElse(sys.error(s"expected a generated-sources dir in ${resolved.sources}"))
      val mapstructGenerated = genDir.resolve("test").resolve("UserMapperImpl.java")
      assert(Files.isRegularFile(mapstructGenerated), s"expected UserMapperImpl.java at $mapstructGenerated")
    }
  }

  test("annotation processors: annotationProcessorOptions compiles cleanly with -A flag") {
    // The DAG handler emits `-A<key>=<value>` flags for every entry in annotationProcessorOptions.
    // We can't directly observe the javac arg list from this test (it's per-invocation, inside
    // the compile handler), so we settle for: the build must compile without errors. If the
    // -A flag was malformed (e.g. quoting wrong) javac would fail.
    runTest(
      "AP options",
      """projects:
        |  a:
        |    dependencies: org.projectlombok:lombok:1.18.46
        |    source-layout: java
        |    platform:
        |      name: jvm
        |    scala:
        |      version: 3.4.2
        |    java:
        |      scanForAnnotationProcessors: true
        |      annotationProcessorOptions:
        |        lombok.addLombokGeneratedAnnotation: "true"
        |""".stripMargin,
      Map(
        RelPath.force("./a/src/java/test/Person.java") ->
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
    ) { (_, commands, _) =>
      val projectName = model.CrossProjectName(model.ProjectName("a"), None)
      commands.compile(List(projectName))
      // If we got here without `commands.compile` throwing, the -A flag was assembled correctly.
      succeed
    }
  }

  test("annotation processors: explicit list runs, scanning stays off") {
    // Auto-discovery is OFF (no scanForAnnotationProcessors). Mapstruct-processor is the only
    // explicit entry; Lombok is in dependencies but should NOT be auto-discovered. Verified by:
    //   (a) End-to-end run uses Mapstruct's generated UserMapperImpl successfully (proves explicit
    //       list ran).
    //   (b) The "auto-discovered annotation processor JAR" log line is NOT emitted (proves
    //       scanning didn't fire — Lombok in deps was correctly ignored).
    runTest(
      "AP explicit only",
      """projects:
        |  a:
        |    dependencies:
        |      - org.projectlombok:lombok:1.18.46
        |      - org.mapstruct:mapstruct:1.5.5.Final
        |    source-layout: java
        |    platform:
        |      name: jvm
        |      mainClass: test.Main
        |    scala:
        |      version: 3.4.2
        |    java:
        |      annotationProcessors:
        |        - org.mapstruct:mapstruct-processor:1.5.5.Final
        |""".stripMargin,
      Map(
        RelPath.force("./a/src/java/test/User.java") ->
          """package test;
            |public class User {
            |    public String name;
            |    public User(String name) { this.name = name; }
            |}""".stripMargin,
        RelPath.force("./a/src/java/test/UserDto.java") ->
          """package test;
            |public class UserDto {
            |    public String name;
            |}""".stripMargin,
        RelPath.force("./a/src/java/test/UserMapper.java") ->
          """package test;
            |import org.mapstruct.Mapper;
            |@Mapper
            |public interface UserMapper {
            |    UserDto userToUserDto(User user);
            |}""".stripMargin,
        RelPath.force("./a/src/java/test/Main.java") ->
          """package test;
            |public class Main {
            |    public static void main(String[] args) {
            |        UserDto dto = new UserMapperImpl().userToUserDto(new User("alice"));
            |        System.out.println("explicit-only:" + dto.name);
            |    }
            |}""".stripMargin
      )
    ) { (_, commands, storingLogger) =>
      val projectName = model.CrossProjectName(model.ProjectName("a"), None)
      commands.run(projectName, maybeOverriddenMain = Some("test.Main"))
      assert(
        storingLogger.underlying.exists(_.message.plainText == "explicit-only:alice"),
        "expected explicit AP list to run mapstruct end-to-end"
      )
      assert(
        !storingLogger.underlying.exists(_.message.plainText.contains("auto-discovered annotation processor")),
        "scanning is off — should not have emitted any auto-discovery log line, even though lombok is in dependencies"
      )
    }
  }

  test("annotation processors: scanForAnnotationProcessors: true with no processors fails loud") {
    val ex = intercept[Throwable] {
      runTest(
        "AP empty scan fails",
        """projects:
          |  a:
          |    dependencies: org.slf4j:slf4j-api:2.0.9
          |    source-layout: java
          |    platform:
          |      name: jvm
          |    scala:
          |      version: 3.4.2
          |    java:
          |      scanForAnnotationProcessors: true
          |""".stripMargin,
        Map(
          RelPath.force("./a/src/java/test/Person.java") ->
            "package test; public class Person {}"
        )
      ) { (_, commands, _) =>
        val projectName = model.CrossProjectName(model.ProjectName("a"), None)
        // The error now surfaces during the DAG's ResolveAnnotationProcessorsTask, which runs
        // when compilation kicks off — not at bootstrap.
        commands.compile(List(projectName))
        succeed
      }
    }
    val msg = Option(ex.getMessage).getOrElse("") + Option(ex.getCause).map(_.getMessage).getOrElse("")
    assert(
      msg.contains("Annotation processor resolution failed"),
      s"unexpected error message: $msg"
    )
  }

  test("run fallback to jvmOptions") {
    runTest(
      "run fallback to jvmOptions",
      """projects:
      a:
        platform:
          name: jvm
          jvmOptions: -Xmx512m -Xms64m -Dfoo=1
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
  //
  // Verifies via compile + classpath inspection rather than `commands.run`. The resolution
  // (the actual thing being tested) is fully exercised by compile; forking a 3.8 runtime JVM
  // just to prove `println` works adds substantial memory pressure (the new stdlib is ~9MB
  // vs 2.13's ~5.6MB, plus extra TASTy/classloader overhead) and doesn't add test coverage.
  test("scala 3.8.1 with new stdlib") {
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
    ) { (started, commands, _) =>
      val projectName = model.CrossProjectName(model.ProjectName("a"), None)
      // Compile must succeed — this exercises scalac 3.8 loading the new stdlib's TASTy.
      commands.compile(List(projectName))

      // The resolved classpath must include the 3.8.1 stdlib at the Scala-3-versioned
      // coordinate (scala-library:3.8.1), not the old 2.13.x coordinate. This is the
      // heart of the SIP — the scala3-library artifact is now a tiny shim and the real
      // stdlib lives under the renamed, 3.x-versioned scala-library.
      val resolved = started.resolvedProjects(projectName).forceGet("test")
      val classpath = resolved.classpath.map(_.toString)
      assert(
        classpath.exists(p => p.contains("scala-library") && p.contains("3.8.1")),
        s"expected scala-library-3.8.1 on classpath for scala 3.8.1 project; got:\n${classpath.mkString("\n")}"
      )
      // And the 3-series scala3-library shim should be present at the matching version.
      assert(
        classpath.exists(p => p.contains("scala3-library_3") && p.contains("3.8.1")),
        s"expected scala3-library_3-3.8.1 on classpath; got:\n${classpath.mkString("\n")}"
      )
    }
  }

  test("resource generator") {
    // Forked JVMs (sourcegen script + `bleep run`) get bounded heaps via `jvmRuntimeOptions`
    // so the total (test JVM + in-process BSP + forks) fits within the 7GB CI runner budget.
    val bleepYaml = """
projects:
  a:
    extends: common
    platform:
      mainClass: test.Main
      jvmRuntimeOptions: -Xmx512m -Xms64m
    sourcegen: scripts/testscripts.SourceGen
  scripts:
    extends: common
    dependencies: build.bleep::bleep-core:${BLEEP_VERSION}
    platform:
      jvmRuntimeOptions: -Xmx512m -Xms64m
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

  test("sourcegen script project that fails to compile: build fails cleanly, does not hang") {
    // Same shape as "resource generator" above, but the scripts project pins Scala 3.3.3 while
    // bleep-core it depends on is built for a newer Scala — the script project won't compile.
    // The DAG should route the failure as: Compile(scripts) fails → Sourcegen(..) Skipped →
    // Compile(a) Skipped → build fails with a BleepException. Before the sourcegen-in-DAG fix,
    // this scenario would hang the BSP handler via unsafeRunSync.
    //
    // This test never spawns forked JVMs (compile fails before sourcegen runs), so it's
    // lighter on memory than the resource-generator test above.
    val bleepYaml = """
projects:
  a:
    extends: common
    platform:
      mainClass: test.Main
    sourcegen: scripts/testscripts.SourceGen
  scripts:
    dependencies: build.bleep::bleep-core:${BLEEP_VERSION}
    platform:
      name: jvm
    scala:
      version: 3.3.3
templates:
  common:
    platform:
      name: jvm
    scala:
      version: 3.8.3
"""

    // Target Main references a symbol that would be produced by sourcegen if it ran.
    val Main = """
package test

object Main {
  def main(args: Array[String]): Unit = println(testgenerated.GeneratedSource.result)
}
"""

    // A real sourcegen script — it uses bleep-core symbols that won't compile on Scala 3.3.3.
    val SourceGen = """
package testscripts

import bleep.*

object SourceGen extends BleepCodegenScript("SourceGen") {
  def run(started: Started, commands: Commands, targets: List[Target], args: List[String]): Unit =
    started.logger.info("should never run — this script project must fail to compile first")
}
"""

    runTest(
      "sourcegen script compile fails",
      bleepYaml,
      Map(
        RelPath.force("./a/src/scala/test/Main.scala") -> Main,
        RelPath.force("./scripts/src/scala/testscripts/SourceGen.scala") -> SourceGen
      )
    ) { (_, commands, storingLogger) =>
      val thrown = intercept[BleepException] {
        commands.compile(List(model.CrossProjectName(model.ProjectName("a"), None)))
      }
      // The failure must be compile-related, not a timeout or hang signature.
      val msg = thrown.getMessage
      assert(msg.contains("compile") || msg.contains("failed"), s"unexpected failure message: $msg")

      val loggedLines = storingLogger.underlying.map(_.message.plainText)

      // The scripts project's compile failure should have been reported through BSP events.
      // We assert loose evidence: either an explicit scripts compile failure or a skipped downstream.
      val scriptsCompileFailed = loggedLines.exists(l => l.contains("scripts") && (l.contains("failed") || l.contains("❌")))
      val targetSkipped = loggedLines.exists(l => l.contains("a") && (l.contains("Skipped") || l.contains("skipped") || l.contains("⏭️")))
      assert(scriptsCompileFailed || targetSkipped, s"expected evidence of scripts compile failure or target skip in logs; got: ${loggedLines.mkString("\n")}")

      // And critically: the generated source should NOT have been produced — sourcegen never ran.
      val noGeneratedSource = !loggedLines.exists(_.contains("testgenerated.GeneratedSource"))
      assert(noGeneratedSource, "target compile should not have proceeded; sourcegen output should be absent")

      succeed
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

  test("test --only with fully qualified class name") {
    runTest(
      "test --only with fully qualified class name",
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
      commands.test(
        projects = List(model.CrossProjectName(model.ProjectName("mytest"), None)),
        watch = false,
        only = Some(NonEmptyList.of("example.MyTest")),
        exclude = None
      )
      succeed
    }
  }

  test("test --only filters to matching suite among multiple") {
    runTest(
      "test --only filters to matching suite among multiple",
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
        RelPath.of("mytest/src/scala/PassingTest.scala") ->
          // language=scala
          """package example
            |
            |import org.scalatest.funsuite.AnyFunSuite
            |
            |class PassingTest extends AnyFunSuite {
            |  test("this passes") {
            |    assert(true)
            |  }
            |}
            |""".stripMargin,
        RelPath.of("mytest/src/scala/FailingTest.scala") ->
          // language=scala
          """package example
            |
            |import org.scalatest.funsuite.AnyFunSuite
            |
            |class FailingTest extends AnyFunSuite {
            |  test("this fails") {
            |    assert(false)
            |  }
            |}
            |""".stripMargin
      )
    ) { (started, commands, storingLogger) =>
      commands.test(
        projects = List(model.CrossProjectName(model.ProjectName("mytest"), None)),
        watch = false,
        only = Some(NonEmptyList.of("PassingTest")),
        exclude = None
      )
      succeed
    }
  }

  test("test --exclude skips matching suite") {
    runTest(
      "test --exclude skips matching suite",
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
        RelPath.of("mytest/src/scala/PassingTest.scala") ->
          // language=scala
          """package example
            |
            |import org.scalatest.funsuite.AnyFunSuite
            |
            |class PassingTest extends AnyFunSuite {
            |  test("this passes") {
            |    assert(true)
            |  }
            |}
            |""".stripMargin,
        RelPath.of("mytest/src/scala/FailingTest.scala") ->
          // language=scala
          """package example
            |
            |import org.scalatest.funsuite.AnyFunSuite
            |
            |class FailingTest extends AnyFunSuite {
            |  test("this fails") {
            |    assert(false)
            |  }
            |}
            |""".stripMargin
      )
    ) { (started, commands, storingLogger) =>
      commands.test(
        projects = List(model.CrossProjectName(model.ProjectName("mytest"), None)),
        watch = false,
        only = None,
        exclude = Some(NonEmptyList.of("FailingTest"))
      )
      succeed
    }
  }

  test("test --only with nonexistent suite fails") {
    runTest(
      "test --only with nonexistent suite fails",
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
      assertThrows[BleepException] {
        commands.test(
          projects = List(model.CrossProjectName(model.ProjectName("mytest"), None)),
          watch = false,
          only = Some(NonEmptyList.of("NonExistentTest")),
          exclude = None
        )
      }
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
