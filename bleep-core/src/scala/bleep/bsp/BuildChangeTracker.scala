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

trait BuildChangeTracker {
  def ensureBloopUpToDate(): Either[BleepException, Started]
  def apply[A](f: => A): A
}

object BuildChangeTracker {

  def make(pre: Prebootstrapped, buildClient: BuildClient): BuildChangeTracker =
    new Impl(new AtomicReference[State](load(pre)), buildClient)

  case class State(pre: Prebootstrapped, maybeStarted: Either[BleepException, Started])
  private class Impl(atomicState: AtomicReference[State], buildClient: BuildClient) extends BuildChangeTracker {
    def apply[A](f: => A): A = {
      ensureBloopUpToDate()
      f
    }

    def ensureBloopUpToDate(): Either[BleepException, Started] =
      atomicState.updateAndGet { state =>
        import state.pre
        if (pre.isOutdated()) {
          val result = load(pre)
          state.maybeStarted.foreach { before =>
            result.maybeStarted.foreach { after =>
              val changes = computeBuildTargetChanges(before, after)
              buildClient.onBuildTargetDidChange(changes)
            }
          }
          result
        } else state
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
      case Left(e)                  => State(pre, Left(e))
      case Right((newPre, started)) => State(newPre, Right(started))
    }
  }

  private def computeBuildTargetChanges(before: Started, after: Started): DidChangeBuildTarget = {
    val changedAndDeleted = before.bloopProjects.flatMap { case (cross, beforeProj) =>
      after.bloopProjects.get(cross) match {
        case Some(afterProj) if beforeProj == afterProj =>
          None
        case Some(_) =>
          Some {
            val event = new BuildTargetEvent(new BuildTargetIdentifier(cross.value))
            event.setKind(BuildTargetEventKind.CHANGED)
            event
          }
        case None =>
          Some {
            val event = new BuildTargetEvent(new BuildTargetIdentifier(cross.value))
            event.setKind(BuildTargetEventKind.DELETED)
            event
          }
      }
    }
    val added = after.bloopProjects.flatMap { case (cross, _) =>
      before.bloopProjects.get(cross) match {
        case None =>
          Some {
            val event = new BuildTargetEvent(new BuildTargetIdentifier(cross.value))
            event.setKind(BuildTargetEventKind.CREATED)
            event
          }
        case _ => None
      }
    }
    new DidChangeBuildTarget((added ++ changedAndDeleted).toList.asJava)
  }
}
