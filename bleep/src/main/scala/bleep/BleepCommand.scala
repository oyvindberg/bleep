package bleep

import cats.data.NonEmptyList

trait BleepCommand {
  def chosenProjects(started: Started, maybeFromCommandLine: Option[NonEmptyList[model.ProjectName]]): List[model.ProjectName] =
    maybeFromCommandLine match {
      case Some(fromCommandLine) => fromCommandLine.toList
      case None =>
        started.activeProjectFromPath match {
          case Some(value) => List(value)
          case None        => started.bloopFiles.keys.toList
        }
    }

  def run(): Unit
}
