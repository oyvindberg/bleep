import bleep.logging.Logger

import java.nio.file.Path
import scala.sys.process.ProcessLogger

package object bleep {

  implicit class PathOps(path: Path) {
    def /(relPath: RelPath): Path =
      relPath.segments.foldLeft(path) {
        case (acc, "..")    => acc.getParent
        case (acc, segment) => acc.resolve(segment)
      }

    def /(str: String): Path =
      RelPath(str) match {
        case Left(err)      => sys.error(err)
        case Right(relPath) => path / relPath
      }
  }

  def cli(cmd: String, logger: Logger)(implicit cwd: Path): Unit =
    sys.process.Process(cmd, cwd = Some(cwd.toFile)).!<(ProcessLogger(logger.info(_), logger.warn(_))) match {
      case 0 => ()
      case n =>
        logger.error(s"FAILED: $cmd")
        System.exit(n)
    }
}
