package bleep

import bleep.logging.{jsonEvents, Logger}
import io.circe.parser.decode
import sourcecode.{Enclosing, File, Line}

import java.nio.file.Path
import scala.sys.process.{BasicIO, Process, ProcessIO}

object cli {
  sealed trait WrittenLine
  object WrittenLine {
    case class StdErr(line: String) extends WrittenLine
    case class StdOut(line: String) extends WrittenLine
  }

  case class WrittenLines(combined: Array[WrittenLine]) {
    def stdout: Array[String] = combined.collect { case WrittenLine.StdOut(line) => line }
    def stderr: Array[String] = combined.collect { case WrittenLine.StdErr(line) => line }
  }

  sealed trait StdIn
  object StdIn {
    case object No extends StdIn
    case object Attach extends StdIn
    case class Provided(data: Array[Byte]) extends StdIn
  }

  case class CliLogger(logger: Logger)(implicit l: Line, f: File, e: Enclosing) {
    def apply(writtenLine: WrittenLine): Unit =
      writtenLine match {
        case WrittenLine.StdErr(line) =>
          logger.warn(line)(implicitly, l, f, e)
        case WrittenLine.StdOut(line) =>
          val maybeJsonEvent = if (line.startsWith("{")) decode[jsonEvents.JsonEvent](line).toOption else None
          maybeJsonEvent match {
            case None            => logger.info(line)(implicitly, l, f, e)
            case Some(jsonEvent) => jsonEvent.logTo(logger)
          }
      }

    def withAction(action: String): CliLogger =
      CliLogger(logger.withPath(action))
  }

  def apply(
      action: String,
      cwd: Path,
      cmd: List[String],
      cliLogger: CliLogger,
      stdIn: StdIn = StdIn.No,
      env: List[(String, String)] = Nil
  ): WrittenLines = {
    val builder = Process(cmd, cwd = Some(cwd.toFile), env: _*)
    val output = Array.newBuilder[WrittenLine]

    val cliLogger0 = cliLogger.withAction(s"[subprocess: $action]")
    val processIO = new ProcessIO(
      writeInput = os =>
        stdIn match {
          case StdIn.No     => ()
          case StdIn.Attach => BasicIO.connectToIn(os)
          case StdIn.Provided(data) =>
            os.write(data)
            os.close()
        },
      processOutput = BasicIO.processFully { line =>
        val stdOut = WrittenLine.StdOut(line)
        output += stdOut
        cliLogger0(stdOut)
      },
      processError = BasicIO.processFully { line =>
        val stdErr = WrittenLine.StdErr(line)
        output += stdErr
        cliLogger0(stdErr)
      },
      daemonizeThreads = false
    )

    val exitCode = builder.run(processIO).exitValue()

    exitCode match {
      case 0 => WrittenLines(output.result())
      case n =>
        cliLogger0.logger
          .withContext(action)
          .withContext(cwd)
          .withContext(cmd)
          .withContext(env)
          .debug(s"Failed external command with error code $n")
        throw new BleepException.Text(s"Failed external command '$action'")
    }
  }
}
