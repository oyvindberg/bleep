import bleep.logging.Logger

import java.nio.file.Path
import scala.sys.process.Process

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

  def cli(cmd: List[String], logger: Logger, action: String, attachInput: Boolean = false, env: List[(String, String)] = Nil)(implicit cwd: Path): Unit = {
    val builder = Process(cmd, cwd = Some(cwd.toFile), env: _*)
    val processLogger = logger.processLogger(action)
    val p = if (attachInput) builder.!<(processLogger) else builder.!(processLogger)

    p match {
      case 0 => ()
      case n =>
        logger.debug(s"Failed command with error code $n: ${cmd.mkString(" ")}")
        throw new BleepException.Text(s"Failed external command '$action'")
    }
  }
}
