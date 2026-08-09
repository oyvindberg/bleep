package bleep

import bleep.internal.ChildProcessDiagnostics
import org.scalatest.funsuite.AnyFunSuite

import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.file.Paths

/** These fixes existed to make a thread dump useful, and every bug they fix was silent — an empty or incomplete dump looks exactly like a dump of a healthy
  * process. So the assertions here are about CONTENT, not about the call returning.
  */
class ChildProcessDiagnosticsTest extends AnyFunSuite {

  private def dump(jvmBinDirs: List[java.nio.file.Path], extraPids: List[Long]): String = {
    val bytes = new ByteArrayOutputStream()
    val out = new PrintStream(bytes, true, "UTF-8")
    ChildProcessDiagnostics.dumpAll(out, jvmBinDirs, extraPids)
    out.flush()
    bytes.toString("UTF-8")
  }

  test("dumps this JVM's own threads, with frames") {
    val content = dump(Nil, Nil)
    assert(content.contains("=== Thread Dump"))
    assert(content.contains(s"--- This JVM (PID ${ProcessHandle.current().pid()})"))
    // A header with no frames is the failure mode worth catching — this test's own stack must appear.
    assert(content.contains("ChildProcessDiagnosticsTest"), s"expected real frames, got:\n${content.take(400)}")
  }

  test("a process named in extraPids is dumped even though it is nobody's descendant") {
    // The bug this fixes: `dumpAll` walked only `descendants()`, so a shared compile server — spawned by some earlier
    // client and therefore not a child of this process — was the one thing missing from the dump.
    //
    // `ProcessHandle.current()` is not a descendant of itself and is filtered out, so use a real unrelated process: the
    // parent of this JVM. It is an ancestor, never a descendant, which is the property under test.
    val ancestor = ProcessHandle.current().parent()
    assume(ancestor.isPresent, "no parent process handle available on this platform")
    val ancestorPid = ancestor.get().pid()

    val without = dump(Nil, Nil)
    val with_ = dump(Nil, List(ancestorPid))

    assert(!without.contains(s"PID $ancestorPid"), s"the ancestor should not appear without extraPids — otherwise this test proves nothing")
    assert(with_.contains(s"PID $ancestorPid"), s"extraPids process $ancestorPid missing from:\n${with_.take(600)}")
  }

  test("self is never dumped twice via extraPids") {
    val selfPid = ProcessHandle.current().pid()
    val content = dump(Nil, List(selfPid))
    // "This JVM" is the one legitimate mention; it must not also show up as an "Other JVM".
    assert(!content.contains(s"--- Other JVM PID $selfPid"), "this process should not be dumped as if it were another")
  }

  test("a bogus jvm bin directory degrades to a stated reason, not a crash or a silent empty section") {
    val content = dump(List(Paths.get("/definitely/not/a/jvm/bin")), List(ProcessHandle.current().parent().map(_.pid()).orElse(0L)))
    assert(content.contains("=== End Thread Dump"), "the dump must still complete")
  }
}
