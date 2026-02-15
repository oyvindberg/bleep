package bleep.testing

import bleep.{BuildPaths, ResolveProjects}
import bloop.config.ConfigCodecs
import com.github.plokhotnyuk.jsoniter_scala.core.{writeToString, WriterConfig}

import java.nio.file.Path

/** Generate bloop JSON files from ResolvedProject for test comparisons.
  *
  * This is only used in tests for comparing bloop file output.
  */
object GenBloopFiles {

  /** Generate encoded bloop files from ResolveProjects.Projects */
  def encodedFiles(buildPaths: BuildPaths, resolved: ResolveProjects.Projects): Map[Path, String] =
    resolved.map { case (projectName, lazyResolved) =>
      val bloopFile = BloopConversions.toBloopConfig(lazyResolved.forceGet)
      val string = writeToString(bloopFile, WriterConfig.withIndentionStep(2))(ConfigCodecs.codecFile)
      val file = buildPaths.bloopFile(projectName)
      (file, string)
    }
}
