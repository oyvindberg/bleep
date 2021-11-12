import bloop.config.{Config, ConfigCodecs}
import com.github.plokhotnyuk.jsoniter_scala

import java.nio.file.{Files, Path}

package object bleep {
  implicit class PathOps(path: Path) {
    def /(relPath: RelPath): Path =
      relPath.segments.foldLeft(path)(_.resolve(_))

    def /(str: String): Path =
      RelPath(str) match {
        case Left(err)      => sys.error(err)
        case Right(relPath) => path / relPath
      }
  }

  def cli(cmd: String)(implicit cwd: Path): Int =
    sys.process.Process(cmd, cwd = Some(cwd.toFile)).!

  def readBloopFile(bloopFilesDir: Path, projectName: model.ProjectName): Config.File = {
    val file = bloopFilesDir / s"${projectName.value}.json"
    val contents = Files.readString(file)
    jsoniter_scala.core.readFromString(contents)(ConfigCodecs.codecFile)
  }
}
