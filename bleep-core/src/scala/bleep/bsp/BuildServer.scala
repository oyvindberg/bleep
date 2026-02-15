package bleep.bsp

import ch.epfl.scala.bsp4j

/** Combined BuildServer trait that extends all BSP interfaces.
  *
  * This replaces bloop.rifle.BuildServer with a bleep-native version. Used by BspServerBuilder to create a typed proxy for BSP communication.
  */
trait BuildServer extends bsp4j.BuildServer with bsp4j.ScalaBuildServer with bsp4j.JavaBuildServer with bsp4j.JvmBuildServer {

  /** Send notification to cancel blocking work on the active workspace.
    *
    * When a second client connects and the workspace is busy, the server sends WorkspaceBusy events. The client can call this to cancel the blocking build so
    * the waiting connection can proceed.
    */
  @org.eclipse.lsp4j.jsonrpc.services.JsonNotification("bleep/cancelBlockingWork")
  def cancelBlockingWork(): Unit
}
