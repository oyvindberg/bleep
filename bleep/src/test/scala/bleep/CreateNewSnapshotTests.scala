package bleep

import bleep.commands.BuildCreateNew
import bleep.internal.Lazy
import bleep.logging.LogLevel
import bleep.testing.SnapshotTest
import cats.data.NonEmptyList

import java.nio.file.Path

class CreateNewSnapshotTests extends SnapshotTest {
  val logger = logging.stdout(LogPatterns.logFile).untyped.filter(LogLevel.info)

  test("create-new-build") {
    val buildPaths = BuildPaths.fromBuildDir(_cwd = Path.of("/tmp"), outFolder / "create-new-build", BuildPaths.Mode.Normal)

    val generatedProjectFiles: Map[Path, String] =
      BuildCreateNew(
        logger,
        cwd = buildPaths.buildDir,
        platforms = NonEmptyList.of(model.PlatformId.Jvm, model.PlatformId.Js),
        scalas = NonEmptyList.of(Versions.Scala3, Versions.Scala213),
        name = "myapp"
      ).genAllFiles(buildPaths)

    writeAndCompareEarly(buildPaths.buildDir, generatedProjectFiles)

    val Right(started) = bootstrap.from(Prebootstrapped(buildPaths, logger), GenBloopFiles.InMemory, Nil, Lazy(BleepConfig.default))

    val generatedBloopFiles: Map[Path, String] =
      GenBloopFiles.encodedFiles(buildPaths, started.bloopFiles).map { case (path, s) => (path, absolutePaths.templatize.string(s)) }

    val allGeneratedFiles = generatedProjectFiles ++ generatedBloopFiles

    // flush templated bloop files to disk if local, compare to checked in if test is running in CI
    // note, keep last. locally it "succeeds" with a `pending`
    writeAndCompare(buildPaths.buildDir, allGeneratedFiles)
  }
}
