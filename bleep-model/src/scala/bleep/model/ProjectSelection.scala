package bleep.model

import scala.collection.mutable

sealed trait ProjectSelection

object ProjectSelection {
  case object None extends ProjectSelection

  case class Include(projectNames: Array[CrossProjectName]) extends ProjectSelection

  case class Exclude(projectNames: Array[CrossProjectName]) extends ProjectSelection

  implicit class SelectionOps(private val selections: Array[ProjectSelection]) extends AnyVal {
    def selection: Array[CrossProjectName] = {
      val ret = mutable.Set.empty[CrossProjectName]
      selections.iterator.foreach {
        case ProjectSelection.None                  => ()
        case ProjectSelection.Include(projectNames) => ret ++= projectNames
        case ProjectSelection.Exclude(projectNames) => ret --= projectNames
      }
      ret.toArray
    }

  }
}
