package bleep

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.Path

/** How bleep describes its own invocation to whoever asks — `bleepscript.Started` hands this to user scripts so they can run bleep again.
  *
  * There are two shapes and they are not interchangeable. A native-image bleep is one path. A bleep on a JVM is `java -cp <classpath> bleep.Main`, several
  * elements of which the first is `java`. The script API used to return `command` for both, so in the second case a script asking how to run bleep received the
  * path to `java` with the arguments that make it bleep silently discarded — and `java compile` is not a bleep invocation that fails, it is a different program
  * being asked for a class named `compile`.
  */
class BleepExecutableTest extends AnyFunSuite with Matchers {

  private val bin = Path.of("/usr/local/bin/bleep")
  private val java = Path.of("/opt/jvm/bin/java")
  private val javaArgs = List("-Xmx2g", "-cp", "/a/bleep-cli.jar:/b/bleep-core.jar", "bleep.Main")

  test("a binary is its own whole invocation, and can be named by one path") {
    BleepExecutable.CurrentBinary(bin).whole shouldBe List(bin.toString)
    BleepExecutable.CurrentBinary(bin).asSinglePath shouldBe bin
    BleepExecutable.DownloadedBinary(bin).whole shouldBe List(bin.toString)
    BleepExecutable.DownloadedBinary(bin).asSinglePath shouldBe bin
  }

  test("a JVM bleep keeps the arguments that make it bleep") {
    // The regression this guards: everything after the java path is what distinguishes bleep from any other JVM program, so dropping it does not degrade the
    // answer, it changes which program the caller runs.
    BleepExecutable.CurrentJava(java, javaArgs).whole shouldBe (java.toString :: javaArgs)
    BleepExecutable.DownloadedJava(java, javaArgs).whole shouldBe (java.toString :: javaArgs)
  }

  test("a JVM bleep refuses to be named by a single path, rather than answering with the java binary") {
    List(BleepExecutable.CurrentJava(java, javaArgs), BleepExecutable.DownloadedJava(java, javaArgs)).foreach { exe =>
      val thrown = intercept[BleepException.Text](exe.asSinglePath)
      // The message has to carry the real invocation: a caller that cannot use one path needs to see what to use instead.
      withClue(thrown.getMessage)(thrown.getMessage should include("bleep.Main"))
      withClue(thrown.getMessage)(thrown.getMessage should include(java.toString))
      // And it must not quietly be the java path, which is the shape of the bug.
      withClue(thrown.getMessage)(thrown.getMessage should not be java.toString)
    }
  }

  test("every case answers `whole`, so the script API always has something runnable to offer") {
    // `asSinglePath` is allowed to refuse; `whole` never is. If a new case ever gets added, this is where a missing invocation shows up.
    val all: List[BleepExecutable] = List(
      BleepExecutable.CurrentBinary(bin),
      BleepExecutable.DownloadedBinary(bin),
      BleepExecutable.InheritedBinary(bin),
      BleepExecutable.CurrentJava(java, javaArgs),
      BleepExecutable.DownloadedJava(java, javaArgs)
    )
    all.foreach(exe => withClue(exe.toString)(exe.whole should not be empty))
    all.foreach(exe => withClue(exe.toString)(exe.whole.head shouldBe exe.command.toString))
  }
}
