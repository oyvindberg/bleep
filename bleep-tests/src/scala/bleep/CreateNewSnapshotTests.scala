package bleep

import bleep.commands.BuildCreateNew
import bleep.internal.FileUtils
import bleep.testing.SnapshotTest
import cats.data.NonEmptyList

import java.nio.file.Path

class CreateNewSnapshotTests extends SnapshotTest {
  test("create-new-build") {
    val logger = logger0.withPath("create-new-build")
    val testFolder = outFolder / "create-new-build"
    val buildLoader = BuildLoader.inDirectory(testFolder / "build")
    val buildPaths = BuildPaths(cwd = FileUtils.TempDir, buildLoader, BuildPaths.Mode.Normal)

    TestResolver.withFactory(isCi, testFolder, absolutePaths) { testResolver =>
      val generatedProjectFiles: Map[Path, String] =
        BuildCreateNew(
          logger,
          cwd = buildPaths.buildDir,
          platforms = NonEmptyList.of(model.PlatformId.Jvm, model.PlatformId.Js),
          scalas = NonEmptyList.of(model.VersionScala.Scala3, model.VersionScala.Scala213),
          name = "myapp",
          bleepVersion = model.BleepVersion.dev,
          testResolver
        ).genAllFiles(buildPaths)

      writeAndCompare(buildPaths.buildDir, generatedProjectFiles, logger)

      val bootstrappedPath = testFolder / "bootstrapped"
      val bootstrappedDestinationPaths = BuildPaths(cwd = FileUtils.TempDir, BuildLoader.inDirectory(bootstrappedPath), BuildPaths.Mode.Normal)

      val Right(started) =
        bootstrap.from(
          Prebootstrapped(bootstrappedDestinationPaths, logger, BuildLoader.Existing(buildLoader.bleepYaml)),
          GenBloopFiles.InMemory,
          Nil,
          Lazy(model.BleepConfig.default),
          testResolver
        )

      val generatedBloopFiles: Map[Path, String] =
        GenBloopFiles.encodedFiles(bootstrappedDestinationPaths, started.bloopFiles).map { case (path, s) => (path, absolutePaths.templatize.string(s)) }

      val allGeneratedFiles = generatedBloopFiles

      // flush templated bloop files to disk if local, compare to checked in if test is running in CI
      // note, keep last. locally it "succeeds" with a `pending`
      writeAndCompare(bootstrappedDestinationPaths.buildDir, allGeneratedFiles, logger)
    }
  }
}
