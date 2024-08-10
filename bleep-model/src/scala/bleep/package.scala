import java.nio.file.Path

package object bleep {
  implicit def bleepExceptionOps[Th, T](e: Either[Th, T]): BleepException.ExpectOps[Th, T] =
    new BleepException.ExpectOps(e)

  implicit class PathOps(private val path: Path) extends AnyVal {
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

  implicit class DiscardOps[T](private val t: T) extends AnyVal {
    // used to avoid unused warnings
    def discard(): Unit = ()
  }

}
