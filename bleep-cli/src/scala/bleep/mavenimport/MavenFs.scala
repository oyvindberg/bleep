package bleep
package mavenimport

import io.circe.generic.semiauto.deriveCodec
import io.circe.Codec

import java.nio.file.{Files, Path}
import scala.collection.immutable.SortedMap
import scala.collection.mutable
import scala.jdk.CollectionConverters.IteratorHasAsScala

/** Everything the maven import reads from disk goes through here: the effective pom, the raw poms along a module's parent chain, which source directories exist
  * and have sources, and the files code generators left under `target/`.
  *
  * This keeps the import a function of what it read. `bleep import-maven` reads the real file system. The snapshot tests replay a recording of what an import
  * of a real build read, so they run without the build checked out, and fail loudly if the import starts reading something the recording has no answer for.
  */
trait MavenFs {
  def isDirectory(p: Path): Boolean
  def isRegularFile(p: Path): Boolean
  def exists(p: Path): Boolean

  /** `toRealPath` for a path which exists */
  def realPath(p: Path): Path
  def readString(p: Path): String

  /** The entries directly in `dir`, sorted */
  def list(dir: Path): List[Path]

  /** `dir` and everything below it, sorted */
  def walk(dir: Path): List[Path]
}

object MavenFs {
  object Real extends MavenFs {
    override def isDirectory(p: Path): Boolean = Files.isDirectory(p)
    override def isRegularFile(p: Path): Boolean = Files.isRegularFile(p)
    override def exists(p: Path): Boolean = Files.exists(p)
    override def realPath(p: Path): Path = p.toRealPath()
    override def readString(p: Path): String = Files.readString(p)
    override def list(dir: Path): List[Path] = {
      val stream = Files.list(dir)
      try stream.iterator().asScala.toList.sorted
      finally stream.close()
    }
    override def walk(dir: Path): List[Path] = {
      val stream = Files.walk(dir)
      try stream.iterator().asScala.toList.sorted
      finally stream.close()
    }
  }

  /** A path as a recording keys it: as a string, so it can be templated like any other text, and with `/` between its parts, so a recording made on one
    * operating system answers on all of them
    */
  def key(p: Path): String = p.toString.replace('\\', '/')

  /** The answers to every question an import asked, keyed by [[key]] */
  case class Recorded(
      isDirectory: SortedMap[String, Boolean],
      isRegularFile: SortedMap[String, Boolean],
      exists: SortedMap[String, Boolean],
      realPath: SortedMap[String, String],
      readString: SortedMap[String, String],
      list: SortedMap[String, List[String]],
      walk: SortedMap[String, List[String]]
  ) {
    def mapStrings(f: String => String): Recorded = {
      def keys[V](m: SortedMap[String, V]): SortedMap[String, V] = m.map { case (k, v) => (f(k), v) }
      Recorded(
        isDirectory = keys(isDirectory),
        isRegularFile = keys(isRegularFile),
        exists = keys(exists),
        realPath = realPath.map { case (k, v) => (f(k), f(v)) },
        readString = readString.map { case (k, v) => (f(k), f(v)) },
        list = list.map { case (k, v) => (f(k), v.map(f)) },
        walk = walk.map { case (k, v) => (f(k), v.map(f)) }
      )
    }
  }

  object Recorded {
    implicit val codec: Codec.AsObject[Recorded] = deriveCodec
  }

  /** Asks `underlying`, and remembers the answers */
  class Recording(underlying: MavenFs) extends MavenFs {
    private val isDirectoryAnswers = mutable.Map.empty[String, Boolean]
    private val isRegularFileAnswers = mutable.Map.empty[String, Boolean]
    private val existsAnswers = mutable.Map.empty[String, Boolean]
    private val realPathAnswers = mutable.Map.empty[String, String]
    private val readStringAnswers = mutable.Map.empty[String, String]
    private val listAnswers = mutable.Map.empty[String, List[String]]
    private val walkAnswers = mutable.Map.empty[String, List[String]]

    override def isDirectory(p: Path): Boolean = isDirectoryAnswers.getOrElseUpdate(key(p), underlying.isDirectory(p))
    override def isRegularFile(p: Path): Boolean = isRegularFileAnswers.getOrElseUpdate(key(p), underlying.isRegularFile(p))
    override def exists(p: Path): Boolean = existsAnswers.getOrElseUpdate(key(p), underlying.exists(p))
    override def realPath(p: Path): Path = Path.of(realPathAnswers.getOrElseUpdate(key(p), underlying.realPath(p).toString))
    override def readString(p: Path): String = readStringAnswers.getOrElseUpdate(key(p), underlying.readString(p))
    override def list(dir: Path): List[Path] = listAnswers.getOrElseUpdate(key(dir), underlying.list(dir).map(key)).map(Path.of(_))
    override def walk(dir: Path): List[Path] = walkAnswers.getOrElseUpdate(key(dir), underlying.walk(dir).map(key)).map(Path.of(_))

    def recorded: Recorded =
      Recorded(
        isDirectory = SortedMap.from(isDirectoryAnswers),
        isRegularFile = SortedMap.from(isRegularFileAnswers),
        exists = SortedMap.from(existsAnswers),
        realPath = SortedMap.from(realPathAnswers),
        readString = SortedMap.from(readStringAnswers),
        list = SortedMap.from(listAnswers),
        walk = SortedMap.from(walkAnswers)
      )
  }

  /** Answers from a recording. A question the recording has no answer for means the import reads something it did not read when the recording was made */
  class Replay(recorded0: Recorded) extends MavenFs {
    // a recording filled in with paths of this machine: on windows the build's own directory, `D:\a\bleep\bleep`, comes in with `\`. Paths only, what
    // files contain is left as it is
    private val recorded = {
      def slashes(path: String): String = path.replace('\\', '/')
      def keys[V](m: SortedMap[String, V]): SortedMap[String, V] = m.map { case (k, v) => (slashes(k), v) }
      Recorded(
        isDirectory = keys(recorded0.isDirectory),
        isRegularFile = keys(recorded0.isRegularFile),
        exists = keys(recorded0.exists),
        realPath = recorded0.realPath.map { case (k, v) => (slashes(k), slashes(v)) },
        readString = keys(recorded0.readString),
        list = recorded0.list.map { case (k, v) => (slashes(k), v.map(slashes)) },
        walk = recorded0.walk.map { case (k, v) => (slashes(k), v.map(slashes)) }
      )
    }

    private def answer[V](what: String, answers: SortedMap[String, V], p: Path): V =
      answers.getOrElse(
        key(p),
        throw new BleepException.Text(s"The maven import asked $what for $p, which the recording has no answer for. Regenerate the recording")
      )

    override def isDirectory(p: Path): Boolean = answer("isDirectory", recorded.isDirectory, p)
    override def isRegularFile(p: Path): Boolean = answer("isRegularFile", recorded.isRegularFile, p)
    override def exists(p: Path): Boolean = answer("exists", recorded.exists, p)
    override def realPath(p: Path): Path = Path.of(answer("realPath", recorded.realPath, p))
    override def readString(p: Path): String = answer("readString", recorded.readString, p)
    override def list(dir: Path): List[Path] = answer("list", recorded.list, dir).map(Path.of(_))
    override def walk(dir: Path): List[Path] = answer("walk", recorded.walk, dir).map(Path.of(_))
  }
}
