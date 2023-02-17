package bleep
package bsp

import bloop.config.Config
import ch.epfl.scala.bsp4j

import java.util.concurrent.atomic.AtomicReference
import scala.collection.SortedMap
import scala.jdk.CollectionConverters._

trait BuildChangeTracker {
  def ensureBloopUpToDate(): Either[BleepException, Started]
}

object BuildChangeTracker {
  def make(pre: Prebootstrapped, buildClient: bsp4j.BuildClient): BuildChangeTracker =
    new Impl(new AtomicReference[State](State(pre, load(pre))), buildClient)

  case class State(pre: Prebootstrapped, maybeStarted: Either[BleepException, Started]) {
    val bloopProjects: SortedMap[model.CrossProjectName, Config.Project] =
      maybeStarted match {
        case Left(_)        => SortedMap.empty
        case Right(started) => started.bloopProjects
      }
  }

  private class Impl(atomicState: AtomicReference[State], buildClient: bsp4j.BuildClient) extends BuildChangeTracker {
    def ensureBloopUpToDate(): Either[BleepException, Started] =
      atomicState.updateAndGet { currentState =>
        currentState.pre.reloadFromDisk() match {
          case Left(err) =>
            State(currentState.pre, Left(err))
          case Right(None) =>
            currentState
          case Right(Some(newPre)) =>
            val newState = State(newPre, load(newPre))
            val changes = computeBuildTargetChanges(currentState, newState)
            newPre.logger.info("Notifying client of changes in build targets")
            newPre.logger.info(changes.toString())
            buildClient.onBuildTargetDidChange(changes)
            newState
        }
      }.maybeStarted
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

  private def id(proj: Config.Project): bsp4j.BuildTargetIdentifier = {
    val id = proj.directory.toUri.toASCIIString.stripSuffix("/") + "/?id=" + proj.name
    // Applying the same format as bloop. There might be a better way to do this.
    val amended = id.replace("file:///", "file:/")
    new bsp4j.BuildTargetIdentifier(amended)
  }

  private def computeBuildTargetChanges(before: State, after: State): bsp4j.DidChangeBuildTarget = {
    val changedAndDeleted = before.bloopProjects.flatMap { case (cross, beforeProj) =>
      after.bloopProjects.get(cross) match {
        case Some(afterProj) if beforeProj == afterProj =>
          None
        case Some(_) =>
          Some {
            val event = new bsp4j.BuildTargetEvent(id(beforeProj))
            event.setKind(bsp4j.BuildTargetEventKind.CHANGED)
            event
          }
        case None =>
          Some {
            val event = new bsp4j.BuildTargetEvent(id(beforeProj))
            event.setKind(bsp4j.BuildTargetEventKind.DELETED)
            event
          }
      }
    }
    val added = after.bloopProjects.flatMap { case (cross, afterProj) =>
      before.bloopProjects.get(cross) match {
        case None =>
          Some {
            val event = new bsp4j.BuildTargetEvent(id(afterProj))
            event.setKind(bsp4j.BuildTargetEventKind.CREATED)
            event
          }
        case _ => None
      }
    }
    new bsp4j.DidChangeBuildTarget((added ++ changedAndDeleted).toList.asJava)
  }
}
