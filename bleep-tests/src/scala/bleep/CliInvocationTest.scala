package bleep

import org.scalatest.funsuite.AnyFunSuite

import java.io.BufferedOutputStream
import java.io.ByteArrayOutputStream
import java.io.DataOutputStream
import java.io.File
import java.io.FileDescriptor
import java.io.FileOutputStream
import java.io.PrintStream

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
    )

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

  def callMainSlurpingStdIo(arguments: Array[String]): IoBuffer = {
    val systemOut = System.out
    val systemErr = System.err

    val stdOutBuffer = ByteArrayOutputStream()
    val stdErrBuffer = ByteArrayOutputStream()
    val bufferedOut = PrintStream(stdOutBuffer)
    val bufferedErr = PrintStream(stdErrBuffer)

    System.setOut(bufferedOut)
    System.setErr(bufferedErr)

    Main._main(arguments)

    bufferedOut.close()
    bufferedErr.close()
    System.setOut(systemOut)
    System.setErr(systemErr)

    IoBuffer(stdOutBuffer, stdErrBuffer)
  }
}
