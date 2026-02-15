package bleep

import bleep.internal.{logException, TransitiveProjects}
import ryddig.Throwables

/** Extracted watch-mode logic shared by commands that extend BleepBuildCommand.
  *
  * Runs `runOnce` immediately, then watches source and build files for changes and re-runs on each change. Blocks until stdin input signals stop.
  */
object WatchMode {
  def run(
      started: Started,
      watchableProjects: Started => TransitiveProjects
  )(runOnce: Started => Either[BleepException, Unit]): Either[BleepException, Unit] = {
    runOnce(started).discard()

    var currentStarted = started

    val codeWatcher = BleepFileWatching.projects(currentStarted, watchableProjects(currentStarted)) { _ =>
      runOnce(currentStarted) match {
        case Left(ex)  => currentStarted.logger.error(Throwables.messagesFrom(ex).mkString(": "))
        case Right(()) => ()
      }
    }

    val buildWatcher = BleepFileWatching.build(started.pre) { _ =>
      started.reloadFromDisk() match {
        case Left(ex) =>
          logException("build changed, but it didn't work :(", started.logger, ex)
          codeWatcher.updateMapping(Map.empty)
        case Right(None) =>
          ()
        case Right(Some(newStarted)) =>
          currentStarted = newStarted
          codeWatcher.updateMapping(BleepFileWatching.projectPathsMapping(currentStarted, watchableProjects(currentStarted)))
      }
    }

    started.logger.info("Running in watch mode")
    codeWatcher.combine(buildWatcher).run(FileWatching.StopWhen.OnStdInput)
    Right(())
  }
}
