package bleep.bsp

import bleep.model
import cats.effect.IO
import cats.effect.std.Mutex

import java.nio.file.Path
import java.util.concurrent.ConcurrentHashMap

/** Serializes KSP runs for one project across all BSP connections in this daemon.
  *
  * KSP writes into `BuildPaths.generatedSourcesDir`, which is deliberately SHARED across build variants (see the doc comment on `BuildPaths`), and the
  * cancellation path deletes those output directories. Two connections — typically an IDE on the `bsp` variant and a CLI on `normal` — running KSP for the same
  * project therefore write, and can delete, the same tree.
  *
  * This must be daemon-scoped to do anything. It was previously an instance field of `MultiWorkspaceBspServer`, which is constructed per connection, so it
  * serialized a project against itself within one connection and nothing else.
  *
  * Keyed by (workspace, project) because the generated-sources directory is workspace-relative and one daemon serves many workspaces.
  *
  * `cats.effect.std.Mutex` rather than `ReentrantLock` so acquire/release composes inside the IO chain.
  */
class KspMutexes {
  private val mutexes = new ConcurrentHashMap[(Path, model.CrossProjectName), Mutex[IO]]()

  def forProject(workspace: Path, project: model.CrossProjectName): IO[Mutex[IO]] = {
    val key = (workspace, project)
    Option(mutexes.get(key)) match {
      case Some(m) => IO.pure(m)
      case None    =>
        Mutex[IO].flatMap { fresh =>
          IO(Option(mutexes.putIfAbsent(key, fresh)).getOrElse(fresh))
        }
    }
  }
}
