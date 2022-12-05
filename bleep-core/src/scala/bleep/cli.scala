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

  sealed trait In
  object In {
    case object No extends In
    case object Attach extends In
    case class Provided(data: Array[Byte]) extends In
  }

  sealed trait Out

  object Out {
    object Raw extends Out

    case class ViaLogger(logger: Logger)(implicit l: Line, f: File, e: Enclosing) extends Out {
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

      def withAction(action: String): ViaLogger =
        ViaLogger(logger.withPath(action))
    }
  }

  def apply(
      action: String,
      cwd: Path,
      cmd: List[String],
      logger: Logger,
      out: Out,
      in: In = In.No,
      env: List[(String, String)] = Nil
  ): WrittenLines = {
    val process = Process {
      val builder = new java.lang.ProcessBuilder(cmd: _*)
      builder.directory(cwd.toFile)
      builder.environment().clear()
      env.foreach { case (k, v) => builder.environment.put(k, v) }
      builder
    }

    val output = Array.newBuilder[WrittenLine]

    val processIO =
      out match {
        case Out.Raw =>
          BasicIO.standard(true)
        case viaLogger0: Out.ViaLogger =>
          val viaLogger = viaLogger0.withAction(s"[subprocess: $action]")
          new ProcessIO(
            writeInput = os =>
              in match {
                case In.No     => ()
                case In.Attach => BasicIO.connectToIn(os)
                case In.Provided(data) =>
                  os.write(data)
                  os.close()
              },
            processOutput = BasicIO.processFully { line =>
              val stdOut = WrittenLine.StdOut(line)
              output += stdOut
              viaLogger(stdOut)
            },
            processError = BasicIO.processFully { line =>
              val stdErr = WrittenLine.StdErr(line)
              output += stdErr
              viaLogger(stdErr)
            },
            daemonizeThreads = false
          )
      }

    val exitCode = process.run(processIO).exitValue()

    val ctxLogger = logger
      .withContext(action)
      .withContext(cwd)
      .withContext(cmd)
      .withContext(env)
      .withContext(exitCode)

    exitCode match {
      case 0 =>
        ctxLogger.debug("Command ran successfully")
        WrittenLines(output.result())
      case n =>
        ctxLogger.debug("Failed command details")
        throw new BleepException.Text(s"Failed external command '$action' with exit code $n. See log file for exact command")
    }
  }
}
