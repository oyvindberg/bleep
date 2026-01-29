package bleep

import bleep.internal.{bleepLoggers, FileUtils}
import bleep.nosbt.librarymanagement.ScalaArtifacts
import org.scalactic.TripleEqualsSupport
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite
import ryddig.*

import java.nio.file.Files
import java.time.Instant
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

/** Tests for SIP-51 (Drop Forwards Binary Compatibility) implementation.
  *
  * SIP-51 requires that for Scala 2.13 and 3:
  *   1. scala-library can float up if dependencies need a higher version
  *   2. scala-library must be at least scalaVersion (floor constraint)
  *   3. scala-library, scala-reflect, scala-compiler must stay at the same version
  *
  * See:
  *   - https://docs.scala-lang.org/sips/drop-stdlib-forwards-bin-compat.html
  *   - https://github.com/sbt/sbt/pull/7480
  */
class Sip51Tests extends AnyFunSuite with TripleEqualsSupport {
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
      bootstrap.from(pre, GenBloopFiles.SyncToDisk, Nil, model.BleepConfig.default, CoursierResolver.Factory.default).orThrow
    }

  val prelude =
    s"""$$schema: https://raw.githubusercontent.com/oyvindberg/bleep/master/schema.json
       |$$version: dev
       |jvm:
       |  name: ${model.Jvm.graalvm.name}
       |""".stripMargin

  def runTest(testName: String, yaml: String, files: Map[RelPath, String] = Map.empty)(f: Started => Assertion): Assertion = {
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
          GenBloopFiles.SyncToDiskWith(GenBloopFiles.ReplaceBleepDependencies(lazyBleepBuild)),
          Nil,
          model.BleepConfig.default,
          CoursierResolver.Factory.default
        )
        .orThrow
      val ret = f(started)
      FileUtils.deleteDirectory(testTempFolder)
      ret
    } finally
      stdLogger.info(s"Ran in $testTempFolder")
  }

  /** Helper to get resolved version of a module from bloop resolution */
  def getResolvedVersion(started: Started, projectName: model.CrossProjectName, org: String, name: String): Option[String] = {
    for {
      resolution <- started.bloopProject(projectName).resolution
      module <- resolution.modules.find(m => m.organization == org && m.name == name)
    } yield module.version
  }

  test("SIP-51: scala-library version must be at least scalaVersion (floor constraint)") {
    // This project has scala.version = 2.13.15 but only depends on a library
    // that needs scala-library 2.13.12. Without the floor constraint,
    // scala-library would resolve to 2.13.12 which is LOWER than scalaVersion.
    // This is wrong - scala-library should be at least 2.13.15.
    runTest(
      "sip51-floor-constraint",
      // language=yaml
      """projects:
        |  myproject:
        |    dependencies:
        |      # cats-core 2.9.0 was compiled with 2.13.10
        |      - org.typelevel::cats-core:2.9.0
        |    platform:
        |      name: jvm
        |    scala:
        |      version: 2.13.15
        |""".stripMargin
    ) { started =>
      val projectName = model.CrossProjectName(model.ProjectName("myproject"), None)
      val scalaVersion = "2.13.15"

      val resolvedLibrary = getResolvedVersion(started, projectName, ScalaArtifacts.Organization, ScalaArtifacts.LibraryID)

      // scala-library MUST be at least scalaVersion
      resolvedLibrary match {
        case Some(version) =>
          assert(
            coursier.core.Version(version) >= coursier.core.Version(scalaVersion),
            s"scala-library ($version) must be >= scalaVersion ($scalaVersion). " +
              "SIP-51 requires scalaVersion as the floor for scala-library resolution."
          )
        case None =>
          fail("scala-library not found in resolution")
      }
    }
  }

  test("SIP-51: scala-library and scala-reflect must be at the same version") {
    // When a project has dependencies that pull in scala-reflect,
    // both scala-library and scala-reflect must resolve to the same version.
    // This is required because these artifacts use cross-artifact inlining
    // and are not binary compatible across versions.
    runTest(
      "sip51-same-version",
      // language=yaml
      """projects:
        |  myproject:
        |    dependencies:
        |      # shapeless pulls in scala-reflect
        |      - com.chuusai::shapeless:2.3.10
        |    platform:
        |      name: jvm
        |    scala:
        |      version: 2.13.15
        |""".stripMargin
    ) { started =>
      val projectName = model.CrossProjectName(model.ProjectName("myproject"), None)

      val resolvedLibrary = getResolvedVersion(started, projectName, ScalaArtifacts.Organization, ScalaArtifacts.LibraryID)
      val resolvedReflect = getResolvedVersion(started, projectName, ScalaArtifacts.Organization, "scala-reflect")

      (resolvedLibrary, resolvedReflect) match {
        case (Some(libVersion), Some(reflectVersion)) =>
          assert(
            libVersion == reflectVersion,
            s"scala-library ($libVersion) and scala-reflect ($reflectVersion) must be at the same version. " +
              "These artifacts use cross-artifact inlining and are not binary compatible across versions."
          )
        case (None, _) =>
          fail("scala-library not found in resolution")
        case (_, None) =>
          // scala-reflect might not be in deps, which is fine
          succeed
      }
    }
  }

  test("SIP-51: scala-library can float up when dependencies need higher version") {
    // This is the original SIP-51 use case: a dependency compiled with a newer
    // Scala version should cause scala-library to be upgraded.
    // zio:2.1.24 was compiled with Scala 2.13.18
    runTest(
      "sip51-float-up",
      // language=yaml
      """projects:
        |  myproject:
        |    dependencies:
        |      - dev.zio::zio:2.1.24
        |    platform:
        |      name: jvm
        |    scala:
        |      version: 2.13.16
        |""".stripMargin
    ) { started =>
      val projectName = model.CrossProjectName(model.ProjectName("myproject"), None)

      val resolvedLibrary = getResolvedVersion(started, projectName, ScalaArtifacts.Organization, ScalaArtifacts.LibraryID)

      resolvedLibrary match {
        case Some(version) =>
          // ZIO 2.1.24 needs 2.13.18, so scala-library should be upgraded
          assert(
            coursier.core.Version(version) >= coursier.core.Version("2.13.18"),
            s"scala-library ($version) should be upgraded to at least 2.13.18 because ZIO 2.1.24 requires it."
          )
        case None =>
          fail("scala-library not found in resolution")
      }
    }
  }

  test("SIP-51: Scala 3 scala-library should also respect floor constraint") {
    // Same test as above but for Scala 3
    runTest(
      "sip51-scala3-floor",
      // language=yaml
      """projects:
        |  myproject:
        |    dependencies:
        |      # cats-effect was compiled with an older Scala
        |      - org.typelevel::cats-effect:3.5.0
        |    platform:
        |      name: jvm
        |    scala:
        |      version: 3.3.3
        |""".stripMargin
    ) { started =>
      val projectName = model.CrossProjectName(model.ProjectName("myproject"), None)
      val scalaVersion = "2.13.12" // Scala 3.3.3 uses scala-library 2.13.12

      val resolvedLibrary = getResolvedVersion(started, projectName, ScalaArtifacts.Organization, ScalaArtifacts.LibraryID)

      resolvedLibrary match {
        case Some(version) =>
          // For Scala 3, we should at least have the scala-library version that Scala 3.3.3 was compiled with
          assert(
            coursier.core.Version(version) >= coursier.core.Version(scalaVersion),
            s"scala-library ($version) must be >= $scalaVersion for Scala 3.3.3"
          )
        case None =>
          fail("scala-library not found in resolution")
      }
    }
  }
}
