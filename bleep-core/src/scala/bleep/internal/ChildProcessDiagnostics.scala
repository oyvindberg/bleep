package bleep.internal

import java.io.PrintStream
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*
import scala.util.Try

/** Dumps thread stacks from this JVM and all child JVMs.
  *
  * Uses `ProcessHandle.current().descendants()` to find children — no manual registry needed. For Java children, runs `jstack` to get thread dumps. Groups
  * threads with identical stacks for compact output.
  */
object ChildProcessDiagnostics {

  /** Dump threads from this JVM and all descendant JVMs. */
  def dumpAll(out: PrintStream): Unit = {
    val timestamp = java.time.LocalDateTime.now().toString
    out.println()
    out.println(s"=== Thread Dump ($timestamp) ===")

    // Current JVM
    dumpCurrentJvm(out)

    // Child JVMs
    val descendants = ProcessHandle.current().descendants().iterator().asScala.toList
    val javaChildren = descendants.filter(isJavaProcess)

    if (javaChildren.nonEmpty) {
      val jstackPath = findJstack()
      javaChildren.foreach { ph =>
        dumpChildJvm(out, ph, jstackPath)
      }
    }

    val nonJava = descendants.filterNot(isJavaProcess)
    if (nonJava.nonEmpty) {
      out.println(s"--- Non-Java child processes (${nonJava.size}) ---")
      nonJava.foreach { ph =>
        val cmd = ph.info().command().toScala.getOrElse("unknown")
        out.println(s"  PID ${ph.pid()}: $cmd")
      }
      out.println()
    }

    out.println("=== End Thread Dump ===")
    out.println()
  }

  private def dumpCurrentJvm(out: PrintStream): Unit = {
    out.println()
    out.println(s"--- This JVM (PID ${ProcessHandle.current().pid()}) ---")
    val traces = Thread.getAllStackTraces.asScala.toList
    printGroupedThreads(out, traces.map { case (t, stack) => ThreadInfo(t.getName, t.getState.toString, stack.toList) })
  }

  private def dumpChildJvm(out: PrintStream, ph: ProcessHandle, jstackPath: Option[Path]): Unit = {
    val pid = ph.pid()
    val cmd = ph.info().command().toScala.getOrElse("java")
    out.println()
    out.println(s"--- Child JVM PID $pid ($cmd) ---")

    jstackPath match {
      case Some(jstack) =>
        val raw = runJstack(jstack, pid)
        raw match {
          case Some(output) =>
            val threads = parseJstackOutput(output)
            if (threads.nonEmpty) {
              printGroupedThreads(out, threads)
            } else {
              // Couldn't parse — print raw but truncated
              output.linesIterator.take(100).foreach(out.println)
              if (output.linesIterator.size > 100) out.println("  ... truncated")
            }
          case None =>
            out.println("  (jstack failed)")
        }
      case None =>
        // No jstack — try kill -QUIT and note it
        Try(Runtime.getRuntime.exec(Array("kill", "-QUIT", pid.toString)))
        out.println(s"  (sent SIGQUIT — dump appears in child's stderr)")
    }
  }

  case class ThreadInfo(name: String, state: String, stack: List[StackTraceElement])

  /** Group threads with identical stacks and print compactly. */
  private def printGroupedThreads(out: PrintStream, threads: List[ThreadInfo]): Unit = {
    // Separate interesting threads from boring ones
    val (interesting, boring) = threads.partition { t =>
      t.stack.nonEmpty &&
      !t.name.startsWith("Reference Handler") &&
      !t.name.startsWith("Finalizer") &&
      !t.name.startsWith("Signal Dispatcher") &&
      !t.name.startsWith("Common-Cleaner") &&
      !t.name.startsWith("Notification Thread") &&
      !t.name.startsWith("Attach Listener") &&
      !t.state.contains("WAITING") || t.stack.exists(f => !f.getClassName.startsWith("java.") && !f.getClassName.startsWith("jdk.") && !f.getClassName.startsWith("sun."))
    }

    // Group by stack trace
    val grouped = interesting.groupBy(t => t.stack.map(_.toString)).toList.sortBy(-_._2.size)

    grouped.foreach { case (_, threads) =>
      val names = threads.map(t => s"${t.name} [${t.state}]")
      if (threads.size == 1) {
        out.println(s"  Thread: ${names.head}")
      } else {
        out.println(s"  ${threads.size} threads:")
        names.take(5).foreach(n => out.println(s"    $n"))
        if (names.size > 5) out.println(s"    ... and ${names.size - 5} more")
      }
      // Print stack, shortening internal frames
      val stack = threads.head.stack
      printShortenedStack(out, stack)
      out.println()
    }

    if (boring.nonEmpty) {
      val boringByState = boring.groupBy(_.state).map { case (state, ts) => s"${ts.size} $state" }
      out.println(s"  (${boring.size} idle threads: ${boringByState.mkString(", ")})")
    }
  }

  /** Print a stack trace, collapsing runs of internal frames. */
  private def printShortenedStack(out: PrintStream, stack: List[StackTraceElement]): Unit = {
    val internalPrefixes = List("java.lang.Thread", "jdk.internal", "sun.misc", "java.util.concurrent.locks", "java.lang.Object.wait")
    var collapsedCount = 0

    stack.foreach { frame =>
      val s = frame.toString
      val isInternal = internalPrefixes.exists(s.startsWith)
      if (isInternal) {
        collapsedCount += 1
      } else {
        if (collapsedCount > 0) {
          out.println(s"    ... $collapsedCount internal frame(s)")
          collapsedCount = 0
        }
        out.println(s"    at $s")
      }
    }
    if (collapsedCount > 0) {
      out.println(s"    ... $collapsedCount internal frame(s)")
    }
  }

  /** Parse jstack output into ThreadInfo. */
  private def parseJstackOutput(output: String): List[ThreadInfo] = {
    val threads = List.newBuilder[ThreadInfo]
    val lines = output.linesIterator.toArray
    var i = 0
    while (i < lines.length) {
      val line = lines(i)
      // Thread header: "thread-name" #N daemon prio=N os_prio=N tid=... nid=... state
      if (line.startsWith("\"")) {
        val nameEnd = line.indexOf('"', 1)
        if (nameEnd > 0) {
          val name = line.substring(1, nameEnd)
          val state = if (line.contains("RUNNABLE")) "RUNNABLE"
          else if (line.contains("WAITING")) "WAITING"
          else if (line.contains("TIMED_WAITING")) "TIMED_WAITING"
          else if (line.contains("BLOCKED")) "BLOCKED"
          else "UNKNOWN"

          // Skip "java.lang.Thread.State:" line
          i += 1
          if (i < lines.length && lines(i).trim.startsWith("java.lang.Thread.State:")) i += 1

          // Collect stack frames
          val stack = List.newBuilder[StackTraceElement]
          while (i < lines.length && lines(i).trim.startsWith("at ")) {
            val frameStr = lines(i).trim.stripPrefix("at ")
            parseStackFrame(frameStr).foreach(stack += _)
            i += 1
            // Skip "- locked" / "- waiting" lines
            while (i < lines.length && lines(i).trim.startsWith("- ")) i += 1
          }

          threads += ThreadInfo(name, state, stack.result())
        } else {
          i += 1
        }
      } else {
        i += 1
      }
    }
    threads.result()
  }

  private def parseStackFrame(s: String): Option[StackTraceElement] = {
    // Format: "package.Class.method(File.java:123)" or "package.Class.method(Native Method)"
    val parenIdx = s.indexOf('(')
    if (parenIdx < 0) return None
    val methodPart = s.substring(0, parenIdx)
    val dotIdx = methodPart.lastIndexOf('.')
    if (dotIdx < 0) return None
    val className = methodPart.substring(0, dotIdx)
    val methodName = methodPart.substring(dotIdx + 1)
    val locationPart = s.substring(parenIdx + 1, s.length - 1) // strip parens
    val (fileName, lineNumber) = if (locationPart == "Native Method") {
      (null, -2)
    } else {
      val colonIdx = locationPart.indexOf(':')
      if (colonIdx > 0) (locationPart.substring(0, colonIdx), Try(locationPart.substring(colonIdx + 1).toInt).getOrElse(-1))
      else (locationPart, -1)
    }
    Some(new StackTraceElement(className, methodName, fileName, lineNumber))
  }

  private def isJavaProcess(ph: ProcessHandle): Boolean =
    ph.info().command().toScala.exists { cmd =>
      cmd.endsWith("/java") || cmd.endsWith("/java.exe") || cmd.contains("jdk") || cmd.contains("jre")
    }

  /** Find jstack relative to current JVM's java.home. */
  private def findJstack(): Option[Path] = {
    val javaHome = Paths.get(System.getProperty("java.home"))
    // java.home may be <jdk>/Contents/Home (macOS) or <jdk> (Linux/Windows)
    val candidates = List(
      javaHome.resolve("bin").resolve("jstack"),
      javaHome.resolve("../bin/jstack").normalize(),
      javaHome.resolve("../../bin/jstack").normalize()
    )
    candidates.find(Files.isExecutable)
  }

  private def runJstack(jstackPath: Path, pid: Long): Option[String] =
    Try {
      val pb = new ProcessBuilder(jstackPath.toString, pid.toString)
      pb.redirectErrorStream(true)
      val proc = pb.start()
      val output = new String(proc.getInputStream.readAllBytes())
      proc.waitFor(5, java.util.concurrent.TimeUnit.SECONDS)
      output
    }.toOption
}
