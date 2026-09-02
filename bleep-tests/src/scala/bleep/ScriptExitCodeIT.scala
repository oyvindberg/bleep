package bleep

/** A program bleep launched exiting non-zero is that program's answer, not a bleep failure to explain.
  *
  * What it used to get instead was `Failed external command 'run' with exit code 1 in <dir>: <the whole java launch command>` — the java path, the
  * `--add-opens` flags and a classpath of every jar on one line. A script that exits 1 to report a problem it has already described in two readable lines then
  * has that report pushed off the screen by bleep's own launch line, and the exit code it chose is replaced by bleep's flat 1.
  *
  * Run through [[JvmRunner.Forked]] on purpose. The harness swaps in [[JvmRunner.InProcess]] for every other test, which cannot host a `System.exit` at all —
  * it would take the test JVM down with it — so the codepath this covers is only reachable by asking for a real fork.
  */
class ScriptExitCodeIT extends IntegrationTestHarness {

  private val a = model.CrossProjectName(model.ProjectName("a"), None)

  private val Yaml =
    """projects:
      |  a:
      |    platform:
      |      name: jvm
      |      mainClass: test.Main
      |    scala:
      |      version: 3.4.2
      |""".stripMargin

  private def exitingMain(code: Int): String =
    s"""package test
       |object Main {
       |  def main(args: Array[String]): Unit = {
       |    println("the report the program wrote")
       |    System.exit($code)
       |  }
       |}""".stripMargin

  private def runForked(ws: Workspace): Either[BleepException, Unit] = {
    val (started, _, _) = ws.start()
    commands
      .Run(a, None, args = Nil, raw = false, watch = false, commands.CommonBuildOpts(commands.DisplayMode.NoTui, flamegraph = false, cancel = false))
      .run(started.withJvmRunner(JvmRunner.Forked))
  }

  integrationTest("a non-zero exit is reported in one line, and the code survives") { ws =>
    ws.yaml(Yaml)
    ws.file("a/src/scala/Main.scala", exitingMain(3))

    runForked(ws) match {
      case Right(())                                => fail("a program that exited 3 was reported as success")
      case Left(sub: BleepException.SubprocessExit) =>
        assert(sub.code == 3, s"the program chose 3, bleep reported ${sub.code}")
        // The whole point: what the reader is shown next to their program's own output. Two markers, because either one alone would have been satisfied by
        // some intermediate wording — a message with no classpath but still built from the launch command, or one that dropped the command and the code too.
        assert(!sub.message.contains(".jar"), s"the launch classpath is back in the message:\n${sub.message}")
        assert(!sub.message.contains("--add-opens"), s"the launch flags are back in the message:\n${sub.message}")
        assert(sub.message.contains("3"), s"the message does not say which code the program chose:\n${sub.message}")
        succeed
      case Left(other) =>
        fail(s"expected a SubprocessExit, got ${other.getClass.getName}: ${other.message}")
    }
  }

  integrationTest("a zero exit is still a success") { ws =>
    ws.yaml(Yaml)
    ws.file("a/src/scala/Main.scala", exitingMain(0))

    runForked(ws) match {
      case Right(())   => succeed
      case Left(other) => fail(s"a program that exited 0 was reported as failure: ${other.message}")
    }
  }
}
