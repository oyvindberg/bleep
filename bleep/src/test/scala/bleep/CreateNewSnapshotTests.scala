package bleep

import bleep.commands.BuildCreateNew
import bleep.logging.LogLevel
import bleep.model.PlatformId
import cats.data.NonEmptyList

import java.nio.file.Paths

class CreateNewSnapshotTests extends SnapshotTest {
  val logger = logging.stdout(LogPatterns.logFile).untyped.filter(LogLevel.info)
  val outFolder = Paths.get("snapshot-tests").toAbsolutePath

  test("create-new-build") {
    val cwd = outFolder / "create-new-build"
    val (started, generatedFiles) = BuildCreateNew(
      logger,
      cwd = cwd,
      platforms = NonEmptyList.of(PlatformId.Jvm, PlatformId.Js),
      scalas = NonEmptyList.of(Versions.Scala3, Versions.Scala213),
      name = "myapp"
    ).generate()

    val generatedBloopFiles: Map[RelPath, String] =
      bootstrap.bloopFileMap(started.bloopFiles).map { case (RelPath(f), s) => (RelPath("bloop" :: f), absolutePaths.templatize.string(s)) }

    val allGeneratedFiles = generatedBloopFiles ++ generatedFiles

    // flush templated bloop files to disk if local, compare to checked in if test is running in CI
    // note, keep last. locally it "succeeds" with a `pending`
    writeAndCompare(cwd, allGeneratedFiles)

  }
}
