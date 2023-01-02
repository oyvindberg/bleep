package bleep

import bleep.internal.FileUtils
import bleep.logging.{LogLevel, TypedLogger}
import bleep.logging.TypedLogger.Stored
import bleep.testing.SnapshotTest
import org.scalactic.TripleEqualsSupport
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.Files
import java.time.Instant
import scala.concurrent.ExecutionContext

class IntegrationTests extends AnyFunSuite with TripleEqualsSupport {
  val userPaths = UserPaths.fromAppDirs
  val logger0 = logging.stdout(LogPatterns.interface(Some(Instant.now), noColor = false), disableProgress = true).untyped.minLogLevel(LogLevel.info)

  val prelude =
    """$schema: https://raw.githubusercontent.com/oyvindberg/bleep/master/schema.json
      |$version: dev
      |""".stripMargin

  // note: passing stored log messages is a hack for now. soon commands will return values, and `run` for instance will return printed lines
  def runTest(testName: String, yaml: String, files: Map[RelPath, String])(f: (Started, Commands, TypedLogger[Array[Stored]]) => Assertion): Unit =
    test(testName) {
      val storingLogger = bleep.logging.storing()
      val stdLogger = logger0.withContext(testName)
      val testTempFolder = Files.createTempDirectory(s"bleep-test-$testName")

      val existingBuild = BuildLoader.Existing(testTempFolder / BuildLoader.BuildFileName, Lazy(Right(prelude ++ yaml)))
      val buildPaths = BuildPaths(cwd = testTempFolder, existingBuild, model.BuildVariant.Normal)
      FileSync.syncStrings(testTempFolder, files, FileSync.DeleteUnknowns.No, soft = false)

      try {
        val started = bootstrap
          .from(
            Prebootstrapped(storingLogger.untyped, userPaths, buildPaths, existingBuild),
            GenBloopFiles.SyncToDisk,
            Nil,
            model.BleepConfig.default,
            CoursierResolver.Factory.default,
            ExecutionContext.global
          )
          .orThrow
        val commands = new Commands(started)
        val ret = f(started, commands, storingLogger)
        FileUtils.deleteDirectory(testTempFolder)
        ret

      } finally
        stdLogger.info(s"Ran in $testTempFolder")
    }

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
