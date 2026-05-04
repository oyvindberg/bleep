package bleep.analysis

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.io.{BufferedReader, InputStreamReader}
import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit

/** End-to-end test for the thread-dump-on-timeout mechanism that runs at suite-idle in [[bleep.bsp.TestRunner]] / [[bleep.testing.JvmPool]].
  *
  * Strategy: spawn a real Java subprocess that hangs in a recognizable named thread, then shell out to `<javaHome>/bin/jstack <pid>` exactly like
  * `JvmPool.ManagedJvm.dumpThreads` does in production, and assert the captured lines look like a real HotSpot thread dump.
  *
  * If this test ever fails it means something broke in the chain: the JDK we run under no longer ships jstack, or jstack changed its output format, or our
  * drain logic regressed. All three cases are silent in production — the only signal would be "Suite idle timeout after 120s" with no accompanying frames
  * again, so keep this test alive.
  *
  * On Windows we self-skip — the production path there returns Nil if `jstack.exe` is missing, and the test is platform-noisy enough as-is.
  */
class JstackThreadDumpTest extends AnyFunSuite with Matchers {

  val isWindows: Boolean = System.getProperty("os.name").toLowerCase.contains("win")

  test("jstack against a hanging JVM produces a thread dump containing our named thread") {
    assume(!isWindows, "jstack subprocess output capture is verified on POSIX only")

    val javaSrc =
      """public class Hang {
        |  public static void main(String[] args) throws Exception {
        |    Thread.currentThread().setName("hanging-test-thread");
        |    Thread.sleep(java.util.concurrent.TimeUnit.MINUTES.toMillis(5));
        |  }
        |}
        |""".stripMargin

    val workDir = Files.createTempDirectory("threaddump-test")
    try {
      val srcFile = workDir.resolve("Hang.java")
      Files.writeString(srcFile, javaSrc)

      val javaHome = Path.of(System.getProperty("java.home"))
      val javac = javaHome.resolve("bin").resolve("javac").toString
      val java = javaHome.resolve("bin").resolve("java").toString
      val jstack = javaHome.resolve("bin").resolve("jstack")

      assume(Files.isExecutable(jstack), s"this JDK ($javaHome) doesn't ship jstack — production fallback returns Nil here")

      val compile = new ProcessBuilder(javac, srcFile.toString)
        .directory(workDir.toFile)
        .redirectErrorStream(true)
        .start()
      compile.waitFor(30, TimeUnit.SECONDS)
      compile.exitValue() shouldBe 0

      val pb = new ProcessBuilder(java, "-cp", workDir.toString, "Hang")
      pb.redirectErrorStream(true)
      val process = pb.start()

      try {
        // Give the JVM a moment to actually start sleeping.
        Thread.sleep(1500)
        process.isAlive shouldBe true

        val jstackProc = new ProcessBuilder(jstack.toString, process.pid().toString)
          .redirectErrorStream(true)
          .start()
        val reader = new BufferedReader(new InputStreamReader(jstackProc.getInputStream))
        val captured = scala.collection.mutable.ListBuffer.empty[String]
        val drainer = new Thread(() =>
          try {
            var line = reader.readLine()
            while (line != null) {
              captured.synchronized(captured += line)
              line = reader.readLine()
            }
          } catch { case _: Throwable => () }
        )
        drainer.setDaemon(true)
        drainer.start()
        jstackProc.waitFor(10, TimeUnit.SECONDS) shouldBe true
        jstackProc.exitValue() shouldBe 0
        drainer.join(1000)

        val capturedSnapshot = captured.synchronized(captured.toList)
        val joined = capturedSnapshot.mkString("\n")
        withClue(s"captured ${capturedSnapshot.size} jstack line(s):\n$joined\n") {
          capturedSnapshot should not be empty
          joined should include("Full thread dump")
          joined should include("hanging-test-thread")
        }
      } finally
        try process.destroyForcibly()
        catch { case _: Throwable => () }
    } finally
      try {
        import scala.jdk.StreamConverters.*
        Files
          .walk(workDir)
          .toScala(List)
          .reverse
          .foreach(p =>
            try Files.delete(p)
            catch { case _: Throwable => () }
          )
      } catch { case _: Throwable => () }
  }
}
