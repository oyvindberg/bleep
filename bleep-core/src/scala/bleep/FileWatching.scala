package bleep

import com.swoval.files.FileTreeViews.Observer
import com.swoval.files.{PathWatcher, PathWatchers}
import com.swoval.functional.Either
import ryddig.Logger

import java.nio.file.{Files, Path}
import scala.collection.mutable

object FileWatching {
  trait StopWhen {
    def shouldContinue(): Boolean
  }

  object StopWhen {
    case object OnStdInput extends StopWhen {
      override def shouldContinue(): Boolean = !Console.in.ready()
    }
    case object Never extends StopWhen {
      override def shouldContinue(): Boolean = true
    }
    case object Immediately extends StopWhen {
      override def shouldContinue(): Boolean = false
    }
  }

  def apply[K](logger: Logger, mapping: Map[Path, Seq[K]])(onChange: Set[K] => Unit): TypedWatcher[K] = {
    val state = new TypedWatcher[K](logger, onChange)

    // setup initial paths
    state.updateMapping(mapping)

    state.addObserver(
      new Observer[PathWatchers.Event] {
        def onError(t: Throwable): Unit = {
          logger.error(s"Got error while listening watching files. We'll stop watching now.", t)
          state.close()
        }

        def onNext(event: PathWatchers.Event): Unit = {
          logger.debug(event.toString)
          val path = event.getTypedPath.getPath
          if (path.getFileName.toString.endsWith("~")) {
            logger.withContext("path", path).debug("Ignoring change in temporary file")
          } else {
            // Find the keys attached to the given path.
            // In order to do that, we need to traverse the file system towards the root until we find a registered directory
            // if we reach the root, `getParent` will return `null`, and it means we got a change for a file outside the registered directories.
            // That again can happen since we register a listener to the parent folder when we want to listen to files or to non-existing directories.
            var registeredPath = path
            while (registeredPath != null && !mapping.contains(registeredPath)) registeredPath = registeredPath.getParent
            if (registeredPath != null) {
              val keys = mapping(registeredPath)
              synchronized {
                state.changedKeys ++= keys
                ()
              }
            }
          }
        }
      }
    )

    state
  }

  trait Watcher {
    private[FileWatching] def isShutdown: Boolean
    private[FileWatching] def close(): Unit
    private[FileWatching] def step(): Unit

    final def run(stopWhen: StopWhen, waitMillis: Long = 10): Unit = {
      def continue = stopWhen.shouldContinue() && !isShutdown
      while (continue) {
        step()
        Thread.sleep(waitMillis)
      }
      close()
    }

    final def combine(other: Watcher): Watcher = new Combined(this, other)
  }

  private class Combined(w1: Watcher, w2: Watcher) extends Watcher {
    override def isShutdown: Boolean =
      w1.isShutdown || w2.isShutdown

    override def close(): Unit = {
      w1.close()
      w2.close()
    }

    override def step(): Unit = {
      w1.step()
      w2.step()
    }
  }

  class TypedWatcher[K](logger: Logger, onChange: Set[K] => Unit) extends Watcher {
    private[FileWatching] val watcher: PathWatcher[PathWatchers.Event] = PathWatchers.get( /* followLinks = */ true)
    private[FileWatching] val changedKeys = mutable.Set.empty[K]
    private[FileWatching] var isShutdown = false
    private[FileWatching] var mapping: Map[Path, Seq[K]] = Map.empty

    def addObserver(observer: Observer[PathWatchers.Event]): Unit =
      watcher.addObserver(observer).discard()

    // todo: imperfect, but good enough for now
    // - path.getParent may clobber `path` if that was its own mapping, and we may lose events under that (depending on order)
    // - unregister will leave directories-for-files for the same reason.
    def updateMapping(newMapping: Map[Path, Seq[K]]): Unit = {
      val newMapping1 = newMapping.filter { case (path, _) => path.toFile.exists() }
      val newMapping2 = newMapping1.map { case (path, keys) =>
        val maybeRegistered =
          if (Files.isRegularFile(path)) {
            watcher.register(path.getParent, 0)
          } else {
            watcher.register(path, Int.MaxValue)
          }

        if (maybeRegistered.isLeft) {
          val th = Either.leftProjection(maybeRegistered).getValue
          throw new BleepException.Cause(th, s"Couldn't register $path for $keys for file watching ")
        }

        (path, keys)
      }

      // unregister removed paths
      val unregister = mapping.keys.toSet -- newMapping2.keys
      unregister.foreach { path =>
        watcher.unregister(path)
        logger.withContext("path", path).debug("unregistered")
      }

      mapping = newMapping2
    }

    override def step(): Unit = {
      val consumeChanges: Set[K] = {
        val changes = synchronized {
          val res = changedKeys.toSet
          changedKeys.clear()
          res
        }
        if (changes.nonEmpty) logger.info(s"Changed: $changes")
        changes
      }

      consumeChanges match {
        case empty if empty.isEmpty => ()
        case nonEmpty               => onChange(nonEmpty)
      }
    }

    override def close(): Unit = {
      isShutdown = true
      watcher.close()
    }
  }
}
