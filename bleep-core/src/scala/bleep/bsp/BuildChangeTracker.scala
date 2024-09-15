package bleep
package bsp

import bleep.rewrites.{BuildRewrite, keepSelectedProjects}
import bloop.config.Config
import ch.epfl.scala.bsp4j

import java.util.concurrent.atomic.AtomicReference
import scala.collection.SortedMap
import scala.jdk.CollectionConverters.*

trait BuildChangeTracker {
  def ensureBloopUpToDate(): Either[BleepException, Started]
  def current: Either[BleepException, Started]
}

object BuildChangeTracker {
  def make(bleepConfig: model.BleepConfig, pre: Prebootstrapped, buildClient: bsp4j.BuildClient): BuildChangeTracker = {
    val initialState: State =
      rewriteFor(pre.buildPaths).flatMap(bspRewrites => load(bleepConfig, pre, bspRewrites)) match {
        case Left(th)       => State.No(bleepConfig, pre, th)
        case Right(started) => State.Yes(started)
      }

    new Impl(new AtomicReference[State](initialState), buildClient)
  }

  // an `Either` with enough state to bootstrap again after it failed
  sealed trait State {
    def pre: Prebootstrapped
    def bleepConfig: model.BleepConfig

    def toEither: Either[BleepException, Started] =
      this match {
        case State.Yes(started)             => Right(started)
        case State.No(_, _, bleepException) => Left(bleepException)
      }
  }

  object State {
    case class Yes(started: Started) extends State {
      override def pre: Prebootstrapped = started.pre
      override def bleepConfig: model.BleepConfig = started.config
    }

    case class No(bleepConfig: model.BleepConfig, pre: Prebootstrapped, bleepException: BleepException) extends State
  }

  private class Impl(atomicState: AtomicReference[State], buildClient: bsp4j.BuildClient) extends BuildChangeTracker {
    def ensureBloopUpToDate(): Either[BleepException, Started] =
      atomicState.updateAndGet { currentState =>
        val reloaded: Either[BleepException, Option[Started]] =
          rewriteFor(currentState.pre.buildPaths).flatMap { buildRewrites =>
            currentState match {
              case State.No(bleepConfig, pre, _) =>
                pre.reloadFromDisk().flatMap(newPre => load(bleepConfig, newPre.getOrElse(pre), buildRewrites).map(Some.apply))

              case State.Yes(started) =>
                started.reloadFromDisk(buildRewrites)
            }
          }
        val newState = reloaded match {
          case Left(bleepException) =>
            State.No(currentState.bleepConfig, currentState.pre, bleepException)
          case Right(None) =>
            currentState.pre.logger.debug(s"Build changed superficially, not reloading")
            currentState
          case Right(Some(newStarted)) =>
            State.Yes(newStarted)
        }

        computeBuildTargetChanges(currentState, newState) match {
          case Some(changes) =>
            newState.pre.logger.info(s"Notifying IDE of ${changes.getChanges.size} changes in build targets")
            newState.pre.logger.debug(changes.toString)
            buildClient.onBuildTargetDidChange(changes)
          case None =>
            ()
        }

        newState
      }.toEither

    override def current: Either[BleepException, Started] = atomicState.get().toEither
  }

  def rewriteFor(buildPaths: BuildPaths): Either[BleepException, List[BuildRewrite]] =
    BspProjectSelection.load(buildPaths).map {
      case Some(selectedProjectGlobs) => List(keepSelectedProjects(selectedProjectGlobs))
      case None                       => Nil
    }

  private def load(bleepConfig: model.BleepConfig, pre: Prebootstrapped, bspRewrites: List[BuildRewrite]): Either[BleepException, Started] =
    bootstrap.from(pre, GenBloopFiles.SyncToDisk, bspRewrites, bleepConfig, CoursierResolver.Factory.default)

  private def computeBuildTargetChanges(before: State, after: State): Option[bsp4j.DidChangeBuildTarget] = {
    implicit val ordering: Ordering[bsp4j.BuildTargetIdentifier] = Ordering.by(_.getUri)

    def projectsFor(s: State): SortedMap[bsp4j.BuildTargetIdentifier, Lazy[Config.File]] =
      s match {
        case State.No(_, _, _)  => SortedMap.empty
        case State.Yes(started) => started.bloopFiles.map { case (crossName, p) => BleepCommandRemote.buildTarget(started.buildPaths, crossName) -> p }
      }

    val beforeProjects = projectsFor(before)
    val afterProjects = projectsFor(after)

    def event(id: bsp4j.BuildTargetIdentifier, change: bsp4j.BuildTargetEventKind) = {
      val ret = new bsp4j.BuildTargetEvent(id)
      ret.setKind(change)
      Some(ret)
    }

    val changes: List[bsp4j.BuildTargetEvent] =
      (beforeProjects.keySet ++ afterProjects.keySet).toList.flatMap { id =>
        (beforeProjects.get(id), afterProjects.get(id)) match {
          case (Some(before), Some(after)) => if (before.forceGet == after.forceGet) None else event(id, bsp4j.BuildTargetEventKind.CHANGED)
          case (None, Some(_))             => event(id, bsp4j.BuildTargetEventKind.CREATED)
          case (Some(_), None)             => event(id, bsp4j.BuildTargetEventKind.DELETED)
          case (None, None)                => None
        }
      }

    if (changes.nonEmpty) Some(new bsp4j.DidChangeBuildTarget(changes.asJava)) else None
  }
}
