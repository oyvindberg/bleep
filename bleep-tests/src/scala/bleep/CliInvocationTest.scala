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

  test("'--help' lists the build's scripts in their own section, after bleep's subcommands") {
    // this repo's own build: `native-image` is one of its scripts
    val lines = callMainSlurpingStdIo(Array("--help")).stdOutBuffer.toString.linesIterator.toList
    val (beforeScripts, scriptsSection) = lines.span(line => !line.startsWith("Scripts"))
    assert(scriptsSection.exists(_.trim.startsWith("native-image ")))
    assert(!beforeScripts.exists(_.trim == "native-image"))
  }

  test("script descriptions line up, and a script without one shows what it runs") {
    def main(project: String, cls: String, description: Option[String]): model.JsonList[model.ScriptDef] =
      model.JsonList(
        List(model.ScriptDef.Main(model.CrossProjectName(model.ProjectName(project), None), cls, model.JsonSet.empty, model.JsonSet.empty, description))
      )
    val rendered = commands.ListScripts.render(
      List(
        model.ScriptName("a") -> main("scripts", "s.A", None),
        model.ScriptName("long-name") -> main("scripts", "s.B", Some("does b"))
      )
    )
    assert(rendered == List("a          (scripts/s.A)", "long-name  does b"))
  }

  test("a script's `description` survives a YAML round trip") {
    val yaml = "main: s.A\nproject: scripts\ndescription: does a\n"
    val parsed = bleep.yaml.parse(yaml).flatMap(_.as[model.ScriptDef]).toTry.get
    assert(parsed.asInstanceOf[model.ScriptDef.Main].description == Some("does a"))
    assert(parsed.asJson.as[model.ScriptDef].toTry.get == parsed)
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
