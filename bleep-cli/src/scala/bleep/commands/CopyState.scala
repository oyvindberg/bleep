package bleep
package commands

import bleep.bsp.protocol.{CopyStateRequest, CopyStateResponse}
import bleep.bsp.{BspRifle, ServerAdminClient, SetupBleepBsp}
import cats.effect.unsafe.implicits.global

/** Copy compiled state from a sibling worktree into this one, so the first build starts from the parent's incremental baseline instead of cold.
  *
  * The copy runs in the compile daemon under the same per-project locks compiles take — that is the whole point of routing it there: state is never copied
  * mid-compile, no matter which client is compiling the source worktree. See [[BleepServerAdmin.CopyStateMethod]].
  */
case class CopyState(from: String) extends BleepBuildCommand {

  override def run(started: Started): Either[BleepException, Unit] =
    exec(started).map { response =>
      val size = CopyState.humanBytes(response.bytesCopied)
      started.logger.info(s"copied $size of compiled state for ${response.projects.length} projects from $from in ${response.durationMs}ms")
      started.logger.info(response.projects.mkString(", "))
    }

  def exec(started: Started): Either[BleepException, CopyStateResponse] = {
    val fromDir = started.buildPaths.cwd.resolve(from).normalize()
    started.bspServerClasspathSource match {
      case _: bsp.BspServerClasspathSource.InProcess =>
        Left(new BleepException.Text("copy-state requires the shared compile server"))
      case bsp.BspServerClasspathSource.FromCoursier(resolver) =>
        SetupBleepBsp(
          compileServerMode = started.config.compileServerModeOrDefault,
          config = started.config,
          resolvedJvm = started.resolvedJvm.forceGet,
          userPaths = started.pre.userPaths,
          resolver = resolver,
          logger = started.logger,
          javaSemanticdbVersion = SetupBleepBsp.DefaultJavaSemanticdbVersion
        ).flatMap { bspConfig =>
          BspRifle.ensureRunning(bspConfig, started.logger).unsafeRunSync()
          val request = CopyStateRequest(
            from = fromDir.toString,
            to = started.buildPaths.buildDir.toString,
            variant = Some(model.BuildVariant.Normal.name)
          )
          ServerAdminClient.copyState(bspConfig.address.socketDir, request) match {
            case Left(err)       => Left(new BleepException.Text(err.message))
            case Right(response) => Right(response)
          }
        }
    }
  }
}

object CopyState {

  /** human-readable byte count: `842 B`, `12.3 KB`, `1.8 GB` */
  def humanBytes(bytes: Long): String = {
    val abs = math.abs(bytes)
    if (abs < 1024L) s"$bytes B"
    else if (abs < 1024L * 1024) f"${bytes / 1024.0}%.1f KB"
    else if (abs < 1024L * 1024 * 1024) f"${bytes / (1024.0 * 1024)}%.1f MB"
    else f"${bytes / (1024.0 * 1024 * 1024)}%.1f GB"
  }
}
