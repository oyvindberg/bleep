package bleep.testing

import bleep.{model, BuildPaths, ResolveProjects}
import bloop.config.ConfigCodecs
import com.github.plokhotnyuk.jsoniter_scala.core.{writeToString, WriterConfig}

import java.nio.file.Path

/** Generate bloop JSON files from ResolvedProject for snapshot test comparisons. */
object GenBloopFiles {

  /** Default snapshot location for a bloop file: `<buildDir>/.bleep/builds/<variant>/.bloop/<crossName>.json`. The `.bloop` segment is no longer used by
    * bleep-bsp at runtime (compile state lives under `.bleep/projects/<cross>/builds/<variant>/`), but keeping it here matches the on-disk snapshots checked
    * into `snapshot-tests/` from before the layout-v2 change. Callers can pass a different mapping if they want to relocate snapshots.
    */
  def defaultBloopFilePath(buildPaths: BuildPaths)(crossName: model.CrossProjectName): Path =
    buildPaths.workspaceVariantDir.resolve(".bloop").resolve(crossName.value.replace('/', '-') + ".json")

  /** Generate encoded bloop files from ResolveProjects.Projects. */
  def encodedFiles(
      filePathFor: model.CrossProjectName => Path,
      resolved: ResolveProjects.Projects
  ): Map[Path, String] =
    resolved.map { case (projectName, lazyResolved) =>
      val bloopFile = BloopConversions.toBloopConfig(lazyResolved.forceGet)
      val string = writeToString(bloopFile, WriterConfig.withIndentionStep(2))(using ConfigCodecs.codecFile)
      (filePathFor(projectName), string)
    }
}
