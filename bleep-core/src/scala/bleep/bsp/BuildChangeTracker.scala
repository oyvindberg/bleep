package bleep.bsp

import bleep.BleepException
import bleep.Started
import bleep.Prebootstrapped
import bleep.rewrites.BuildRewrite
import bleep.GenBloopFiles
import bleep.CoursierResolver
import java.util.concurrent.atomic.AtomicReference
import bleep.BleepConfigOps
import ch.epfl.scala.bsp4j.DidChangeBuildTarget
import ch.epfl.scala.bsp4j.BuildTargetEvent
import ch.epfl.scala.bsp4j.BuildTargetIdentifier
import ch.epfl.scala.bsp4j.BuildTargetEventKind
import scala.jdk.CollectionConverters._
import ch.epfl.scala.bsp4j.BuildClient
import bloop.config.Config
import scala.collection.SortedMap
import bleep.model.CrossProjectName

trait BuildChangeTracker {
  def ensureBloopUpToDate(): Either[BleepException, Started]
}

object BuildChangeTracker {

  def make(pre: Prebootstrapped, buildClient: BuildClient): BuildChangeTracker =
    new Impl(new AtomicReference[State](load(pre)), buildClient)

  case class State(pre: Prebootstrapped, maybeStarted: Either[BleepException, Started], bloopProjects: SortedMap[CrossProjectName, Config.Project])
  private class Impl(atomicState: AtomicReference[State], buildClient: BuildClient) extends BuildChangeTracker {

    def ensureBloopUpToDate(): Either[BleepException, Started] =
      atomicState.updateAndGet { currentState =>
        import currentState.pre
        if (pre.isOutdated()) {
          val newState = load(pre)
          val changes = computeBuildTargetChanges(currentState, newState)
          pre.logger.info("Notifying client of changes in build targets")
          pre.logger.info(changes.toString())
          buildClient.onBuildTargetDidChange(changes)
          newState
        } else currentState
      }.maybeStarted
  }

  private def load(pre: Prebootstrapped): State = {
    val config = BleepConfigOps.loadOrDefault(pre.userPaths).orThrow
    val maybeStarted = BspProjectSelection.load(pre.buildPaths).flatMap { maybeSelectedProjectGlobs =>
      val bspRewrites: List[BuildRewrite] = List(
        maybeSelectedProjectGlobs match {
          case Some(selectedProjectGlobs) => List(bleep.rewrites.keepSelectedProjects(selectedProjectGlobs))
          case None                       => Nil
        }
      ).flatten

      pre.reloadFromDisk().flatMap { newPre =>
        val maybeStarted = bleep.bootstrap.from(newPre, GenBloopFiles.SyncToDisk, bspRewrites, config, CoursierResolver.Factory.default)
        maybeStarted.map(newPre -> _)
      }
    }
    maybeStarted match {
      case Left(e)                  => State(pre, Left(e), SortedMap.empty)
      case Right((newPre, started)) => State(newPre, Right(started), started.bloopProjects)
    }
  }

  private def id(proj: Config.Project): BuildTargetIdentifier = {
    val id = proj.directory.toUri.toASCIIString.stripSuffix("/") + "/?id=" + proj.name
    // Applying the same format as bloop. There might be a better way to do this.
    val amended = id.replace("file:///", "file:/")
    new BuildTargetIdentifier(amended)
  }

  private def computeBuildTargetChanges(before: State, after: State): DidChangeBuildTarget = {
    val changedAndDeleted = before.bloopProjects.flatMap { case (cross, beforeProj) =>
      after.bloopProjects.get(cross) match {
        case Some(afterProj) if beforeProj == afterProj =>
          None
        case Some(_) =>
          Some {
            val event = new BuildTargetEvent(id(beforeProj))
            event.setKind(BuildTargetEventKind.CHANGED)
            event
          }
        case None =>
          Some {
            val event = new BuildTargetEvent(id(beforeProj))
            event.setKind(BuildTargetEventKind.DELETED)
            event
          }
      }
    }
    val added = after.bloopProjects.flatMap { case (cross, afterProj) =>
      before.bloopProjects.get(cross) match {
        case None =>
          Some {
            val event = new BuildTargetEvent(id(afterProj))
            event.setKind(BuildTargetEventKind.CREATED)
            event
          }
        case _ => None
      }
    }
    new DidChangeBuildTarget((added ++ changedAndDeleted).toList.asJava)
  }
}
