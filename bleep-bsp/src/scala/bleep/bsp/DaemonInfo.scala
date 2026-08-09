package bleep.bsp

import bleep.model

import java.nio.file.Path

/** Daemon-wide facts and capabilities, handed to each connection.
  *
  * This exists so the `bleep/status` and `bleep/shutdown` handlers can reach daemon-level state without any of it becoming a global. `MultiWorkspaceBspServer`
  * is constructed per connection; everything here is created once in `BspServerDaemon.runWithLock` and passed down the call chain.
  *
  * @param bootedConfig
  *   the config this daemon read at startup. Deliberately a snapshot, not a re-read: it bounds state shared across every client, and re-reading per connection
  *   would let the newest client silently redefine it for the others. `bleep server config show` compares it against disk to show drift.
  * @param requestDaemonShutdown
  *   closes the server socket, which unblocks the accept loop and lets normal cleanup run — lock released, pid/socket removed, metrics flushed. Distinct from
  *   `build/shutdown`, which only ends the calling connection.
  */
case class DaemonInfo(
    startedAtEpochMs: Long,
    pid: Long,
    socketDir: Path,
    bleepVersion: String,
    bootedConfig: model.BspServerConfig,
    connectionRegistry: ConnectionRegistry,
    requestDaemonShutdown: () => Unit
)

object DaemonInfo {

  /** For servers running inside the caller's own JVM — the in-process client and the test harness.
    *
    * `bleep/status` still answers truthfully about this process, which is what makes the endpoint testable without forking. `bleep/shutdown` is a no-op because
    * the caller owns the lifecycle: there is no accept loop to unblock, no lock to release, and no socket files to remove.
    */
  def inProcess(bootedConfig: model.BspServerConfig): DaemonInfo =
    DaemonInfo(
      startedAtEpochMs = System.currentTimeMillis(),
      pid = ProcessHandle.current().pid(),
      socketDir = java.nio.file.Paths.get(""),
      bleepVersion = model.BleepVersion.current.value,
      bootedConfig = bootedConfig,
      connectionRegistry = new ConnectionRegistry(() => System.currentTimeMillis()),
      requestDaemonShutdown = () => ()
    )
}
