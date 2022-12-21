package bleep

import bleep.logging.Logger
import com.swoval.files.FileTreeViews.Observer
import com.swoval.files.{PathWatcher, PathWatchers}
import com.swoval.functional.Either

import java.nio.file.{Files, Path}
import scala.collection.compat._
import scala.collection.mutable

object FileWatching {
  def projects(started: Started, projects: Array[model.CrossProjectName])(onChange: Set[model.CrossProjectName] => Unit): Unit = {
    val bySourceFolder: Map[Path, Seq[model.CrossProjectName]] = {
      val withTransitiveDeps: Array[model.CrossProjectName] =
        (projects ++ projects.flatMap(x => started.build.transitiveDependenciesFor(x).keys)).distinct

      val sourceProjectPairs: Array[(Path, model.CrossProjectName)] =
        withTransitiveDeps.flatMap { name =>
          val bloopProject = started.bloopProjects(name)
          (bloopProject.sources ++ bloopProject.resources.getOrElse(Nil)).map(path => (path, name))
        }

      sourceProjectPairs.toSeq.groupMap { case (p, _) => p } { case (_, name) => name }
    }

    FileWatching[model.CrossProjectName](started.logger, bySourceFolder)(onChange)
  }

  def apply[K](logger: Logger, mapping: Map[Path, Seq[K]])(onChange: Set[K] => Unit): Unit = {
    val w = new State[K](logger, onChange)
    // setup initial paths
    w.updateMapping(mapping)
    logger.info("Running in watch mode")
    while (!Console.in.ready() && !w.isShutdown) {
      w.step()
      Thread.sleep(10)
    }
    w.close()
  }

  private class State[K](logger: Logger, onChange: Set[K] => Unit) {
    val watcher: PathWatcher[PathWatchers.Event] = PathWatchers.get( /* followLinks = */ true)
    val changedKeys = mutable.Set.empty[K]
    var isShutdown = false
    var mapping: Map[Path, Seq[K]] = Map.empty

    watcher.addObserver {
      new Observer[PathWatchers.Event] {
        def onError(t: Throwable): Unit = {
          logger.error(s"Got error while listening watching files", t)
          close()
        }

        def onNext(event: PathWatchers.Event): Unit = {
          val path = event.getTypedPath.getPath
          if (path.getFileName.toString.endsWith("~")) {
            logger.withContext(path).debug("Ignoring change in temporary file")
          } else {
            logger.debug(event.toString)
            var registeredPath = path
            while (!mapping.contains(registeredPath)) registeredPath = registeredPath.getParent
            val keys = mapping(registeredPath)
            synchronized(changedKeys ++= keys)
          }
        }
      }
    }

    def step(): Unit = {
      val changes = consumeChanges()
      changes match {
        case empty if empty.isEmpty => ()
        case nonEmpty               => onChange(nonEmpty)
      }
    }

    def consumeChanges(): Set[K] = {
      val changes = synchronized {
        val res = changedKeys.toSet
        changedKeys.clear()
        res
      }
      if (changes.nonEmpty) logger.info(s"Changed: $changes")
      changes
    }

    def updateMapping(newMapping0: Map[Path, Seq[K]]): Unit = {
      val newMapping = newMapping0.filter { case (path, _) => Files.exists(path) }
      // unregister removed paths
      (mapping.keys.toSet -- newMapping.keys).foreach(watcher.unregister)

      newMapping.foreach { case (path, keys) =>
        val maybeRegistered = watcher.register(path, Int.MaxValue)

        if (maybeRegistered.isLeft) {
          val th = Either.leftProjection(maybeRegistered).getValue
          throw new BleepException.Cause(th, s"Couldn't register $path for $keys for file watching ")
        } else {
          if (maybeRegistered.get()) {
            logger.withContext(path).debug("registered")
          }
        }
      }
      mapping = newMapping
    }

    def close(): Unit = {
      isShutdown = true
      watcher.close()
    }
  }
}
