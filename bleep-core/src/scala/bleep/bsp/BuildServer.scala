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

  /** Hand the server a freshly resolved build, replacing the one sent at initialize.
    *
    * The client owns the build and watches its files; this is how a long-lived session (an IDE, an MCP server) keeps the daemon in step after an edit. The
    * parameter is a `BspBuildData.Payload` as a Gson element so lsp4j passes the JSON through untouched — same reason `initializeSession` sets `data` that way.
    *
    * Method name must match `BleepBspProtocol.BuildChanged`; annotations need a literal.
    */
  @org.eclipse.lsp4j.jsonrpc.services.JsonNotification("bleep/buildChanged")
  def buildChanged(payload: com.google.gson.JsonElement): Unit
}
