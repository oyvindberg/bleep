package bleep

import bleep.internal.FileUtils
import bleep.logging.TypedLogger.Stored
import bleep.logging.{LogLevel, Loggers, TypedLogger}
import bleep.model.BuildVariant
import org.scalactic.TripleEqualsSupport
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.Files
import java.time.Instant
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

class IntegrationTests extends AnyFunSuite with TripleEqualsSupport {
  val userPaths = UserPaths.fromAppDirs
  val logger0 = Loggers.stdout(LogPatterns.interface(Some(Instant.now), noColor = false), disableProgress = true).untyped.unsafeGet().minLogLevel(LogLevel.info)
  val ec: ExecutionContextExecutor = ExecutionContext.global

  val lazyBleepBuild: Lazy[Started] =
    Lazy {
      val existing = BuildLoader.find(FileUtils.cwd).existing.orThrow
      val pre = Prebootstrapped(logger0, userPaths, BuildPaths(FileUtils.cwd, existing, BuildVariant.Normal), existing, ec)
      bootstrap.from(pre, GenBloopFiles.SyncToDisk, Nil, model.BleepConfig.default, CoursierResolver.Factory.default).orThrow
    }

  val prelude =
    """$schema: https://raw.githubusercontent.com/oyvindberg/bleep/master/schema.json
      |$version: dev
      |jvm:
      |  name: graalvm-java17:22.3.0
      |""".stripMargin

  // note: passing stored log messages is a hack for now. soon commands will return values, and `run` for instance will return printed lines
  def runTest(testName: String, yaml: String, files: Map[RelPath, String])(f: (Started, Commands, TypedLogger[Array[Stored]]) => Assertion): Assertion = {
    val storingLogger = Loggers.storing()
    val stdLogger = logger0.withContext(testName)
    val testTempFolder = Files.createTempDirectory(s"bleep-test-$testName")

    val withBuildScript = files.updated(RelPath(List(BuildLoader.BuildFileName)), prelude ++ yaml)
    FileSync.syncStrings(testTempFolder, withBuildScript, FileSync.DeleteUnknowns.No, soft = false)
    val existingBuild = BuildLoader.find(testTempFolder).existing.orThrow
    val buildPaths = BuildPaths(cwd = testTempFolder, existingBuild, model.BuildVariant.Normal)

    try {
      val started = bootstrap
        .from(
          Prebootstrapped(storingLogger.untyped.zipWith(stdLogger).untyped, userPaths, buildPaths, existingBuild, ec),
          GenBloopFiles.SyncToDiskWith(GenBloopFiles.ReplaceBleepDependencies(lazyBleepBuild)),
          Nil,
          model.BleepConfig.default,
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
          jvmRuntimeOptions: -Dfoo=2
          jvmOptions: -Dfoo=1
          mainClass: test.Main
        scala:
          version: 3.2.1
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
  test("run fallback to jvmOptions") {
    runTest(
      "run fallback to jvmOptions",
      """projects:
      a:
        platform:
          name: jvm
          jvmOptions: -Dfoo=1
          mainClass: test.Main
        scala:
          version: 3.2.1
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
}
