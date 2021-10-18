import java.nio.file.Path

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

  def cli(cmd: String): Unit =
    sys.process.Process(cmd).! match {
      case 0 => ()
      case n =>
        System.err.println(s"FAILED: $cmd")
        System.exit(n)
    }
}
