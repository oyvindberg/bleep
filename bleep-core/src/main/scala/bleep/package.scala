import bleep.logging.Logger
import bloop.config.{Config, ConfigCodecs}
import com.github.plokhotnyuk.jsoniter_scala

import java.nio.file.{Files, Path}
import scala.sys.process.ProcessLogger

package object bleep {
  private[bleep] def assertUsed(anies: Any*): Unit = ((), anies)._1

  implicit class PathOps(path: Path) {
    def /(relPath: RelPath): Path =
      relPath.segments.foldLeft(path)(_.resolve(_))

    def /(str: String): Path =
      RelPath(str) match {
        case Left(err)      => sys.error(err)
        case Right(relPath) => path / relPath
      }
  }
  private[bleep] implicit class IterableOps[I[t] <: Iterable[t], T](private val ts: I[T]) extends AnyVal {
    // surprisingly difficult to express with default collections
    def optReduce(op: (T, T) => Option[T]): Option[T] = {
      val it = ts.iterator
      var acc: Option[T] = None
      var first = true
      while (it.hasNext && (acc.nonEmpty || first)) {
        val x = it.next()
        if (first) {
          acc = Some(x)
          first = false
        } else {
          acc = op(acc.get, x)
        }
      }
      acc
    }

    def firstDefined[U](f: T => Option[U]): Option[U] = {
      val it = ts.iterator
      while (it.hasNext) {
        val ou = f(it.next())
        if (ou.isDefined) return ou
      }
      None
    }
  }
  private[bleep] def throwableMessages(th: Throwable): List[String] =
    th.getMessage :: Option(th.getCause).toList.flatMap(throwableMessages)

  def cli(cmd: String, logger: Logger)(implicit cwd: Path): Unit =
    sys.process.Process(cmd, cwd = Some(cwd.toFile)).!<(ProcessLogger(logger.info(_), logger.warn(_))) match {
      case 0 => ()
      case n =>
        logger.error(s"FAILED: $cmd")
        System.exit(n)
    }

  def readAndParseBloopFile(file: Path): Config.File = {
    val contents = Files.readString(file)
    parseBloopFile(contents)
  }

  def parseBloopFile(contents: String): Config.File =
    jsoniter_scala.core.readFromString(contents)(ConfigCodecs.codecFile)
}
