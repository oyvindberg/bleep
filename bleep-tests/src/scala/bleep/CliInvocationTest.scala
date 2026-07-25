package bleep

import org.scalatest.funsuite.AnyFunSuite

import java.io.{ByteArrayOutputStream, PrintStream}

class CliInvocationTest extends AnyFunSuite {
  case class IoBuffer(stdOutBuffer: ByteArrayOutputStream, stdErrBuffer: ByteArrayOutputStream)

  // https://www.gnu.org/prep/standards/html_node/_002d_002dhelp.html

  test("'--help' output should go to stdout, nothing on stderr") {
    val captured = callMainSlurpingStdIo(Array("--help"))

    assert(
      captured.stdOutBuffer.toString.linesIterator
        .filter(_ match {
          case "Usage:" | "Options and flags:" | "Subcommands:" => true
          case _                                                => false
        })
        .toList
        .size == 3
    ).discard()

    assert(captured.stdErrBuffer.size == 0)
  }

  test("failed bleep invocation help output should go to stderr") {
    val captured = callMainSlurpingStdIo(Array("--this-option-does-not-exist"))

    assert(
      captured.stdErrBuffer.toString.linesIterator
        .filter(_ match {
          case "Usage:" | "Options and flags:" | "Subcommands:"  => true
          case "Unexpected option: --this-option-does-not-exist" => true
          case _                                                 => false
        })
        .toList
        .size == 3 + 1
    )
  }

  private val scriptNames = Set("myscript", "native-image")

  test("script invocation forwards plain trailing args unchanged") {
    assert(Main.insertScriptArgSeparator(List("myscript", "foo", "bar"), scriptNames) == List("myscript", "--", "foo", "bar"))
  }

  test("script invocation forwards `--`-prefixed args verbatim") {
    assert(Main.insertScriptArgSeparator(List("myscript", "--clients", "20"), scriptNames) == List("myscript", "--", "--clients", "20"))
  }

  test("a leading `--watch`/`-w` stays a bleep flag, the rest is forwarded raw") {
    assert(Main.insertScriptArgSeparator(List("myscript", "--watch", "--clients", "20"), scriptNames) == List("myscript", "--watch", "--", "--clients", "20"))
    assert(Main.insertScriptArgSeparator(List("myscript", "-w"), scriptNames) == List("myscript", "-w", "--"))
  }

  test("a user-supplied `--` separator is left untouched (no double insertion)") {
    assert(Main.insertScriptArgSeparator(List("myscript", "--", "--watch"), scriptNames) == List("myscript", "--", "--watch"))
  }

  test("`run <script>` also forwards `--`-prefixed args verbatim") {
    assert(Main.insertScriptArgSeparator(List("run", "myscript", "--clients", "20"), scriptNames) == List("run", "myscript", "--", "--clients", "20"))
  }

  test("built-in subcommands are not rewritten") {
    assert(Main.insertScriptArgSeparator(List("compile", "--watch", "myproject"), scriptNames) == List("compile", "--watch", "myproject"))
    assert(Main.insertScriptArgSeparator(List("run", "myproject", "--foo"), scriptNames) == List("run", "myproject", "--foo"))
  }

  def callMainSlurpingStdIo(arguments: Array[String]): IoBuffer = {
    val systemOut = System.out
    val systemErr = System.err

    val stdOutBuffer = ByteArrayOutputStream()
    val stdErrBuffer = ByteArrayOutputStream()
    val bufferedOut = PrintStream(stdOutBuffer)
    val bufferedErr = PrintStream(stdErrBuffer)

    System.setOut(bufferedOut)
    System.setErr(bufferedErr)

    // without `--dev`, `Main may try to boot another bleep version
    Main._main(Array("--dev") ++ arguments).discard()

    bufferedOut.close()
    bufferedErr.close()
    System.setOut(systemOut)
    System.setErr(systemErr)

    IoBuffer(stdOutBuffer, stdErrBuffer)
  }
}
