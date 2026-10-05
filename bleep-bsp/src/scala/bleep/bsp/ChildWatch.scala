package bleep.bsp

import bleep.machine.{ForkId, ForkRegistry}
import ryddig.Logger

import java.nio.file.Path
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.locks.LockSupport
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

/** Which child of the server belongs to which grant — the pure rule behind [[ChildWatch]] (design §5.3).
  *
  * Scala Native's toolchain spawns clang, clang++, lld, dsymutil and ar itself, through `scala.sys.process`, many at once and with no hook to hand the
  * processes out. They are children of the *server*, alongside test JVMs, konanc, node and other workspaces' links. What identifies them is verified against
  * the toolchain's source (0.5.x `LLVM.scala`): every command it runs names a path under the directory bleep hands it as `baseDir` — the `.ll`/`.c` input and
  * `.o` output of each compile, `@<workDir>/llvmLinkInfo` for the link, the build path for dsymutil and ar — and bleep gives every link its own such directory
  * (`<project target>/link-output/<platform>/native-work`). So the rule is: a child whose command line names a path under exactly one claimed directory belongs
  * to that claim.
  *
  * A child already registered as a fork's process is not a candidate (a test JVM, a konanc, a node). A child that names paths under two claims is a bug — two
  * links in one directory — and throws. A child that matches no claim is *unattributed*: it is in the machine's used memory, which every server reads, so no
  * room arithmetic is wrong; it is charged to no grant, which [[ChildWatch]] says out loud rather than dropping silently.
  */
object ChildAttribution {

  /** A direct child of the server: its pid, its command as the OS reports it, and the tokens of its command line (the arguments where the platform gives them,
    * else the whole line as one token; either way `contains` finds a path).
    */
  case class Child(pid: Long, command: Option[String], commandLine: List[String])

  /** A grant watching for its toolchain's children under `dir` (absolute, normalised). */
  case class Claim(fork: ForkId, dir: Path)

  case class Attributed(byFork: Map[ForkId, Set[Long]], unattributed: List[Child])

  def attribute(children: List[Child], known: Set[Long], claims: List[Claim]): Attributed = {
    val candidates = children.filterNot(c => known.contains(c.pid))
    val (matched, unmatched) = candidates.partition(c => claims.exists(matches(c, _)))
    val byFork = matched.map { child =>
      claims.filter(matches(child, _)) match {
        case List(one) => one.fork -> child.pid
        case several   =>
          throw new IllegalStateException(
            s"child process ${child.pid} (${child.command.getOrElse("?")}) names paths under ${several.size} links' directories: " +
              several.map(c => s"fork ${c.fork.value} at ${c.dir}").mkString(", ")
          )
      }
    }
    Attributed(byFork.groupMap(_._1)(_._2).view.mapValues(_.toSet).toMap, unmatched)
  }

  /** The claimed directory, followed by a separator, appears in some token: a path under it, not a sibling with a longer name. Both separators, because the
    * toolchain writes forward slashes into its link-info file on Windows while the paths it passes directly use the platform's.
    */
  private def matches(child: Child, claim: Claim): Boolean = {
    val dir = claim.dir.toString
    val needles = List(dir + java.io.File.separator, dir.replace('\\', '/') + "/")
    child.commandLine.exists(token => needles.exists(token.contains))
  }
}

/** Finds the processes a toolchain spawned and reports them to the grant they belong to (design §5.3).
  *
  * One per daemon. A Scala Native link claims its work directory for the time of the link; while any claim is open, a thread lists the server's children once a
  * second — the measurement cadence, not the tick's — and attributes each by [[ChildAttribution]]. Attributed children are reported through
  * [[GrantedFork.observed]], which is idempotent per pid, so the scan keeps no memory of what it has seen. With no claim open nothing runs: between links the
  * server's children are reported forks or the short-lived helpers bleep runs synchronously (`git`, `node --version`), and nothing is being measured against
  * them.
  *
  * Unattributed children are warned about once per command (pid and command line named), never silently dropped: they sit in the machine's used memory and are
  * charged to no grant, which is a fact worth a line in the log and, if it recurs, a bug report — but not a reason to charge them to a grant that did not start
  * them.
  */
final class ChildWatch(forks: ForkRegistry, logger: Logger) extends AutoCloseable {
  import ChildAttribution.*

  private val claims = new ConcurrentHashMap[ForkId, (GrantedFork, Path)]()
  private val warnedCommands = ConcurrentHashMap.newKeySet[String]()
  private val closed = new AtomicBoolean(false)
  private val started = new AtomicBoolean(false)
  private val thread: Thread = new Thread(() => loop(), ChildWatch.ThreadName)
  thread.setDaemon(true)

  /** Watch the server's children for `grant`'s toolchain, which names paths under `dir` in every command it runs. Closing the result ends the claim; processes
    * already reported stay the grant's until they are gone.
    */
  def claim(grant: GrantedFork, dir: Path): AutoCloseable = {
    if (closed.get()) throw new IllegalStateException("the child watch is closed")
    val key = dir.toAbsolutePath.normalize()
    if (claims.putIfAbsent(grant.id, (grant, key)) != null) throw new IllegalStateException(s"fork ${grant.id.value} already claims a directory")
    if (started.compareAndSet(false, true)) thread.start()
    LockSupport.unpark(thread)
    () => claims.remove(grant.id): Unit
  }

  /** How many claims are open, for tests. */
  def open: Int = claims.size()

  /** One scan: the server's children now, attributed and reported. Public for tests; the thread calls it once a second while a claim is open. */
  def scanOnce(): Unit =
    if (!claims.isEmpty) {
      val handles = ProcessHandle.current().children().iterator().asScala.toList
      val children = handles.map { h =>
        val info = h.info()
        val tokens = info.arguments().toScala.map(_.toList).getOrElse(Nil) ++ info.commandLine().toScala.toList
        h -> Child(h.pid(), info.command().toScala, tokens)
      }
      val known = forks.live.flatMap(_.pids()).toSet
      val snapshot = claims.asScala.toMap
      val result = attribute(children.map(_._2), known, snapshot.map { case (id, (_, dir)) => Claim(id, dir) }.toList)
      result.byFork.foreach { case (id, pids) =>
        val (grant, _) = snapshot(id)
        children.collect { case (h, c) if pids.contains(c.pid) => h }.foreach(grant.observed)
      }
      result.unattributed.foreach { child =>
        val command = child.command.map(c => Path.of(c).getFileName.toString).getOrElse("<unknown command>")
        if (warnedCommands.add(command))
          logger.warn(
            s"child process ${child.pid} ($command) is not attributed to any fork: it is in the machine's used memory but charged to no grant. " +
              s"Command line: ${child.commandLine.mkString(" ").take(300)}"
          )
      }
    }

  private def loop(): Unit =
    while (!closed.get())
      if (claims.isEmpty) LockSupport.park(this)
      else {
        scanOnce()
        LockSupport.parkNanos(this, ChildWatch.ScanIntervalMs * 1_000_000L)
      }

  override def close(): Unit =
    if (closed.compareAndSet(false, true)) {
      LockSupport.unpark(thread)
      if (thread.isAlive) thread.join()
    }
}

object ChildWatch {
  val ThreadName = "bleep-child-watch"

  /** The measurement cadence (design §5 rule 1, `Ticker.MeasureAfterMs`): a process is charged its grant's bound until a second old anyway, so finding it
    * sooner would change nothing.
    */
  val ScanIntervalMs: Long = 1000L
}
