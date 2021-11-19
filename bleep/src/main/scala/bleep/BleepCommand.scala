package bleep

import cats.effect.{ExitCode, IO}

trait BleepCommand {
  def runWithEnv(runIo: Started => IO[ExitCode]): IO[ExitCode] =
    bootstrap.fromCwd match {
      case Left(th) => IO.raiseError(th)
      case Right(started) =>
        started.activeProject.foreach(p => started.logger.info(s"active project: ${p.value}"))
        runIo(started)
    }

  def run(): IO[ExitCode]
}
