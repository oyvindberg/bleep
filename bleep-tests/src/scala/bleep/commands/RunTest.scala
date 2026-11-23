package bleep.commands

import bleep.IntegrationTestHarness

import java.io.{ByteArrayOutputStream, PrintStream}

/** `raw` hands the forked program raw stdout moments after [[Run.compileProject]] runs — its own protocol, not bleep's compile progress, must own that stream
  * from its first byte. See [[Run.compileStartedFor]].
  */
class RunTest extends IntegrationTestHarness {
  integrationTest("a raw run's own pre-compile logs to stderr, not the stdout the forked program is about to own") { ws =>
    ws.yaml(
      """projects:
        |  a:
        |    platform:
        |      name: jvm
        |      mainClass: test.Main
        |    scala:
        |      version: 3.4.2
        |""".stripMargin
    )
    ws.file(
      "a/src/scala/Main.scala",
      """package test
        |object Main {
        |  def main(args: Array[String]): Unit = ()
        |}""".stripMargin
    )
    val (started, _, _) = ws.start()

    assert(Run.compileStartedFor(started, raw = false) eq started)

    val savedOut = System.out
    val savedErr = System.err
    val capturedOut = new ByteArrayOutputStream()
    val capturedErr = new ByteArrayOutputStream()
    try {
      System.setOut(new PrintStream(capturedOut))
      System.setErr(new PrintStream(capturedErr))
      Run.compileStartedFor(started, raw = true).logger.info("compiling before raw run")
    } finally {
      System.setOut(savedOut)
      System.setErr(savedErr)
    }
    assert(capturedOut.size() == 0)
    assert(capturedErr.toString.contains("compiling before raw run"))
  }
}
