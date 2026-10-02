package bleep

import scala.jdk.CollectionConverters._

/** A process and everything it has spawned, measured once: what `bleep server top` draws under each server.
  *
  * Sampled by the client, from outside, rather than reported by each daemon about itself. The daemons hogging a machine are precisely the old ones — left
  * running by a long-lived client pinned to an earlier bleep — and an old daemon can only report what its version knew to report. Looking from outside works on
  * every one of them, wedged ones included.
  *
  * The memory figure is [[ProcessMemory]]'s: `phys_footprint` on macOS (what Activity Monitor calls Memory), Pss on Linux.
  */
object ProcessTree {

  /** One process as sampled.
    *
    * @param parentPid
    *   `None` for the root of the sample. Descendants always carry it, and the tree is rebuilt from these links rather than shipped nested.
    * @param footprintMb
    *   [[ProcessMemory]]'s proportional figure. `None` when the platform cannot measure, or the process exited between being listed and being measured.
    * @param cpuTimeMs
    *   cumulative CPU time. A rate needs two samples, so the reader diffs consecutive ones; a single sample cannot say how busy anything is right now.
    */
  case class Sample(pid: Long, parentPid: Option[Long], label: String, footprintMb: Option[Long], cpuTimeMs: Option[Long], startedAtEpochMs: Option[Long])

  def sample(root: ProcessHandle, memory: ProcessMemory): List[Sample] = {
    val descendants = root.descendants().iterator().asScala.toList
    (root :: descendants).map { handle =>
      val info = handle.info()
      val parentPid = if (handle.pid() == root.pid()) None else Some(handle.parent().map[Long](_.pid()).orElse(root.pid()))
      Sample(
        pid = handle.pid(),
        parentPid = parentPid,
        label = describe(optional(info.command()), optional(info.arguments()).map(_.toList).getOrElse(Nil)),
        footprintMb = memory.footprintMb(handle.pid()),
        cpuTimeMs = memory.cpuTimeMs(handle.pid()),
        startedAtEpochMs = optional(info.startInstant()).map(_.toEpochMilli)
      )
    }
  }

  /** Whoever started a process, when that is not the init process. For a compile server this is the client that spawned it and is keeping it in use — the
    * answer to "why is this old server still here", when the answer is an editor or an MCP server started days ago.
    */
  case class Parent(pid: Long, label: String, startedAtEpochMs: Option[Long])

  def parentOf(handle: ProcessHandle): Option[Parent] = {
    val parent = optional(handle.parent())
    parent.filter(_.pid() > 1).map { p =>
      val info = p.info()
      Parent(
        p.pid(),
        describe(optional(info.command()), optional(info.arguments()).map(_.toList).getOrElse(Nil)),
        optional(info.startInstant()).map(_.toEpochMilli)
      )
    }
  }

  private def optional[A](o: java.util.Optional[A]): Option[A] = if (o.isPresent) Some(o.get()) else None

  /** The main classes bleep itself forks, by what they are for. Anything else is named by its own main class, which for sourcegen is the user's script. */
  private val KnownMainClasses: Map[String, String] = Map(
    bleep.bsp.BspRifleConfig.ServerMainClass -> "compile server",
    "bleep.testing.runner.ForkedTestRunner" -> "test JVM",
    "bleep.bsp.ScalaNativeTestFork" -> "scala-native test"
  )

  /** Options to `java` that consume the next argument, so it is not mistaken for the main class. */
  private val JavaOptionsWithValue: Set[String] =
    Set("-cp", "-classpath", "--class-path", "-p", "--module-path", "--add-modules", "--add-opens", "--add-exports", "--add-reads", "-m", "--module")

  /** A short name for a process, from its command line. A JVM is named by its main class, because "java" is every row of the tree; anything else by its
    * executable.
    */
  def describe(command: Option[String], arguments: List[String]): String = {
    val executable = command.map(c => java.nio.file.Paths.get(c).getFileName.toString).getOrElse("?")
    if (executable == "java" || executable == "java.exe") {
      def mainOf(args: List[String]): Option[String] = args match {
        case "-jar" :: jar :: _                                                 => Some(s"-jar ${java.nio.file.Paths.get(jar).getFileName}")
        case option :: _ :: rest if JavaOptionsWithValue(option)                => mainOf(rest)
        case option :: rest if option.startsWith("-") || option.startsWith("@") => mainOf(rest)
        case main :: _                                                          => Some(main)
        case Nil                                                                => None
      }
      mainOf(arguments) match {
        case Some(jar) if jar.startsWith("-jar ") => s"java $jar"
        case Some(main)                           => KnownMainClasses.getOrElse(main, s"java ${main.split('.').last}")
        case None                                 => "java"
      }
    } else
      // A subcommand says far more than the binary: `bleep mcp-server` and `bleep compile` are very different reasons to be running. A path or a file name
      // (`run.js`) is not a subcommand and would only add noise.
      arguments.headOption.filter(arg => !arg.startsWith("-") && !arg.contains('/') && !arg.contains('.')) match {
        case Some(subcommand) => s"$executable $subcommand"
        case None             => executable
      }
  }
}
