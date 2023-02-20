package bleep
package bsp

import bloop.config.Config
import ch.epfl.scala.bsp4j

import java.util.concurrent.atomic.AtomicReference
import scala.collection.SortedMap
import scala.jdk.CollectionConverters._

trait BuildChangeTracker {
  def ensureBloopUpToDate(): Either[BleepException, Started]
  def currentBuildError: Option[BleepException]
}

object BuildChangeTracker {
  def make(pre: Prebootstrapped, buildClient: bsp4j.BuildClient): BuildChangeTracker =
    new Impl(new AtomicReference[State](State(pre, load(pre))), buildClient)

  case class State(pre: Prebootstrapped, maybeStarted: Either[BleepException, Started])

  private class Impl(atomicState: AtomicReference[State], buildClient: bsp4j.BuildClient) extends BuildChangeTracker {
    def ensureBloopUpToDate(): Either[BleepException, Started] =
      atomicState.updateAndGet { currentState =>
        currentState.pre.reloadFromDisk() match {
          case Left(err) =>
            State(currentState.pre, Left(err))
          case Right(None) =>
            currentState.pre.logger.info(s"Build changed superficially, not reloading")
            currentState
          case Right(Some(newPre)) =>
            val newState = State(newPre, load(newPre))
            computeBuildTargetChanges(currentState, newState) match {
              case Some(changes) =>
                newPre.logger.info(s"Notifying IDE of ${changes.getChanges.size} changes in build targets")
                newPre.logger.debug(changes.toString)
                buildClient.onBuildTargetDidChange(changes)
              case None =>
                ()
            }
            newState
        }
      }.maybeStarted

    override def currentBuildError: Option[BleepException] = atomicState.get().maybeStarted.left.toOption
  }

  private def load(pre: Prebootstrapped): Either[BleepException, Started] =
    for {
      config <- BleepConfigOps.loadOrDefault(pre.userPaths)
      projectSelection <- BspProjectSelection.load(pre.buildPaths)
      bspRewrites = projectSelection match {
        case Some(selectedProjectGlobs) => List(rewrites.keepSelectedProjects(selectedProjectGlobs))
        case None                       => Nil
      }
      started <- bootstrap.from(pre, GenBloopFiles.SyncToDisk, bspRewrites, config, CoursierResolver.Factory.default)
    } yield started

  private def computeBuildTargetChanges(before: State, after: State): Option[bsp4j.DidChangeBuildTarget] = {
    implicit val ordering: Ordering[bsp4j.BuildTargetIdentifier] = Ordering.by(_.getUri)

    def projectsFor(s: State): SortedMap[bsp4j.BuildTargetIdentifier, Config.Project] =
      s.maybeStarted match {
        case Left(_)        => SortedMap.empty
        case Right(started) => started.bloopProjects.map { case (crossName, p) => BleepCommandRemote.buildTarget(started.buildPaths, crossName) -> p }
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
          case (Some(before), Some(after)) => if (before == after) None else event(id, bsp4j.BuildTargetEventKind.CHANGED)
          case (None, Some(_))             => event(id, bsp4j.BuildTargetEventKind.CREATED)
          case (Some(_), None)             => event(id, bsp4j.BuildTargetEventKind.DELETED)
          case (None, None)                => None
        }
      }

    if (changes.nonEmpty) Some(new bsp4j.DidChangeBuildTarget(changes.asJava)) else None
  }
}
