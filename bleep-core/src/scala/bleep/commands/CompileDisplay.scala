package bleep
package commands

import cats.effect._
import cats.syntax.all._
import ryddig.Logger

import scala.{Console => SConsole}

/** Simple display for compile-bsp that prints all events with timestamps. Used in --no-tui mode.
  */
trait CompileDisplay {
  def handle(event: CompileEvent): IO[Unit]
  def printSummary: IO[Unit]
}

/** Events that can occur during compilation */
sealed trait CompileEvent {
  def timestamp: Long
}

object CompileEvent {
  case class ProjectCompileStarted(project: String, timestamp: Long) extends CompileEvent
  case class ProjectCompileProgress(project: String, percent: Int, timestamp: Long) extends CompileEvent
  case class ProjectCompileFinished(project: String, success: Boolean, durationMs: Long, errors: List[String], timestamp: Long) extends CompileEvent
  case class ProjectSkipped(project: String, reason: String, timestamp: Long) extends CompileEvent
  case class DiagnosticEmitted(project: String, file: String, line: Int, severity: String, message: String, timestamp: Long) extends CompileEvent
  case class Error(message: String, details: Option[String], timestamp: Long) extends CompileEvent
}

object CompileDisplay {

  case class CompileSummary(
      projectsTotal: Int,
      projectsCompiled: Int,
      projectsFailed: Int,
      projectsSkipped: Int,
      errors: List[String],
      durationMs: Long
  )

  def create(logger: Logger): IO[CompileDisplay] =
    for {
      state <- Ref.of[IO, DisplayState](DisplayState.empty)
      startTime <- IO.realTime.map(_.toMillis)
    } yield new CompileDisplayImpl(state, startTime, logger)

  private case class DisplayState(
      projectsTotal: Int,
      projectsCompiled: Int,
      projectsFailed: Int,
      projectsSkipped: Int,
      currentlyCompiling: Set[String],
      errors: List[String],
      projectStartTimes: Map[String, Long]
  )

  private object DisplayState {
    def empty: DisplayState = DisplayState(
      projectsTotal = 0,
      projectsCompiled = 0,
      projectsFailed = 0,
      projectsSkipped = 0,
      currentlyCompiling = Set.empty,
      errors = Nil,
      projectStartTimes = Map.empty
    )
  }

  private class CompileDisplayImpl(
      state: Ref[IO, DisplayState],
      startTime: Long,
      logger: Logger
  ) extends CompileDisplay {

    private def formatElapsed(timestamp: Long): String = {
      val elapsed = timestamp - startTime
      f"[${elapsed / 1000.0}%6.2fs]"
    }

    private def log(msg: String): IO[Unit] = IO(logger.info(msg))
    private def logWarn(msg: String): IO[Unit] = IO(logger.warn(msg))
    private def logError(msg: String): IO[Unit] = IO(logger.error(msg))

    override def handle(event: CompileEvent): IO[Unit] = event match {
      case CompileEvent.ProjectCompileStarted(project, timestamp) =>
        for {
          _ <- state.update { s =>
            s.copy(
              projectsTotal = s.projectsTotal + 1,
              currentlyCompiling = s.currentlyCompiling + project,
              projectStartTimes = s.projectStartTimes + (project -> timestamp)
            )
          }
          _ <- log(s"${formatElapsed(timestamp)} ${SConsole.CYAN}COMPILE${SConsole.RESET} $project")
        } yield ()

      case CompileEvent.ProjectCompileProgress(project, percent, timestamp) =>
        log(s"${formatElapsed(timestamp)} ${SConsole.CYAN}PROGRESS${SConsole.RESET} $project: $percent%")

      case CompileEvent.ProjectCompileFinished(project, success, durationMs, errors, timestamp) =>
        for {
          _ <- state.update { s =>
            s.copy(
              currentlyCompiling = s.currentlyCompiling - project,
              projectsCompiled = s.projectsCompiled + (if (success) 1 else 0),
              projectsFailed = s.projectsFailed + (if (!success) 1 else 0),
              errors = if (success) s.errors else s.errors ++ errors,
              projectStartTimes = s.projectStartTimes - project
            )
          }
          status =
            if (success) {
              s"${SConsole.GREEN}COMPILED${SConsole.RESET}"
            } else {
              s"${SConsole.RED}FAILED${SConsole.RESET}"
            }
          _ <- log(s"${formatElapsed(timestamp)} $status $project (${durationMs}ms)")
          _ <-
            if (!success && errors.nonEmpty) {
              errors.take(5).traverse_(e => logError(s"           ${SConsole.RED}│${SConsole.RESET} $e")) >>
                (if (errors.size > 5) logError(s"           ${SConsole.RED}│${SConsole.RESET} ... and ${errors.size - 5} more errors") else IO.unit)
            } else IO.unit
        } yield ()

      case CompileEvent.ProjectSkipped(project, reason, timestamp) =>
        for {
          _ <- state.update(s => s.copy(projectsSkipped = s.projectsSkipped + 1))
          _ <- logWarn(s"${formatElapsed(timestamp)} ${SConsole.YELLOW}SKIPPED${SConsole.RESET} $project: $reason")
        } yield ()

      case CompileEvent.DiagnosticEmitted(project, file, line, severity, message, timestamp) =>
        val logFn = severity.toLowerCase match {
          case "error"   => logError _
          case "warning" => logWarn _
          case _         => log _
        }
        val sevColor = severity.toLowerCase match {
          case "error"   => SConsole.RED
          case "warning" => SConsole.YELLOW
          case _         => ""
        }
        val location = if (line > 0) s"$file:$line" else file
        logFn(s"${formatElapsed(timestamp)} $sevColor${severity.toUpperCase}${SConsole.RESET} [$project] $location: $message")

      case CompileEvent.Error(message, details, timestamp) =>
        for {
          _ <- state.update(s => s.copy(errors = s.errors :+ message))
          _ <- logError(s"${formatElapsed(timestamp)} ${SConsole.RED}ERROR${SConsole.RESET} $message")
          _ <- details match {
            case Some(d) => logError(s"           ${d.take(200)}")
            case None    => IO.unit
          }
        } yield ()
    }

    override def printSummary: IO[Unit] =
      for {
        s <- state.get
        now <- IO.realTime.map(_.toMillis)
        durationMs = now - startTime
        _ <- log("")
        _ <- log("=" * 60)
        _ <- log("Compilation Summary")
        _ <- log("=" * 60)
        _ <- log(s"Projects: ${s.projectsCompiled} compiled, ${s.projectsFailed} failed, ${s.projectsSkipped} skipped")
        _ <- log(f"Duration: ${durationMs / 1000.0}%.2fs")
        _ <-
          if (s.errors.nonEmpty) {
            log("") >>
              logError(s"${SConsole.RED}Errors:${SConsole.RESET}") >>
              s.errors.distinct.take(20).traverse_(e => logError(s"  • $e"))
          } else IO.unit
        _ <- log("=" * 60)
      } yield ()
  }
}
