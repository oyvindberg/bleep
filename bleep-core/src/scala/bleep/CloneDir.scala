package bleep

import java.nio.file.{Files, Path, StandardCopyOption}

/** Recursive directory clone, by the fastest means the platform offers.
  *
  * macOS: `cp -Rc` (APFS clonefile — O(metadata), blocks shared copy-on-write). Linux: `cp -a --reflink=auto` (CoW on btrfs/XFS, plain copy elsewhere — that
  * fallback is cp's own documented semantics, not ours). Other platforms: a JVM recursive copy.
  *
  * The strategy is chosen by OS up front and failures throw — there is no try-one-then-the-other.
  */
object CloneDir {
  sealed trait Strategy
  object Strategy {
    case object MacCp extends Strategy
    case object LinuxCp extends Strategy
    case object JvmCopy extends Strategy

    val current: Strategy =
      OsArch.current.os match {
        case model.Os.Macos => MacCp
        case model.Os.Linux => LinuxCp
        case _              => JvmCopy
      }
  }

  /** Clone directory `from` to `to`. `to` must not exist; parent directories are created. */
  def clone(from: Path, to: Path): Unit =
    cloneWith(Strategy.current, from, to)

  def cloneWith(strategy: Strategy, from: Path, to: Path): Unit = {
    if (!Files.isDirectory(from)) throw new BleepException.Text(s"cannot clone $from: not a directory")
    if (Files.exists(to)) throw new BleepException.Text(s"cannot clone to $to: already exists")
    Files.createDirectories(to.getParent)

    strategy match {
      case Strategy.MacCp   => cp(List("cp", "-Rc", from.toString, to.toString))
      case Strategy.LinuxCp => cp(List("cp", "-a", "--reflink=auto", from.toString, to.toString))
      case Strategy.JvmCopy => jvmCopy(from, to)
    }
  }

  private def cp(cmd: List[String]): Unit = {
    import scala.jdk.CollectionConverters.*
    val proc = new ProcessBuilder(cmd.asJava).redirectErrorStream(true).start()
    val output = new String(proc.getInputStream.readAllBytes(), "UTF-8")
    val exit = proc.waitFor()
    if (exit != 0) throw new BleepException.Text(s"${cmd.mkString(" ")} failed with exit code $exit: $output")
  }

  private def jvmCopy(from: Path, to: Path): Unit = {
    import java.nio.file.{FileVisitResult, SimpleFileVisitor}
    import java.nio.file.attribute.BasicFileAttributes
    Files.walkFileTree(
      from,
      new SimpleFileVisitor[Path] {
        override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes): FileVisitResult = {
          Files.createDirectories(to.resolve(from.relativize(dir).toString))
          FileVisitResult.CONTINUE
        }
        override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
          Files.copy(file, to.resolve(from.relativize(file).toString), StandardCopyOption.COPY_ATTRIBUTES)
          FileVisitResult.CONTINUE
        }
      }
    )
    ()
  }
}
