import java.nio.file.Path

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
}
