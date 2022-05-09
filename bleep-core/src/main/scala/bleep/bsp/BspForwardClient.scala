package bleep.bsp

import ch.epfl.scala.bsp4j

class BspForwardClient(var forwardToOpt: Option[bsp4j.BuildClient]) extends bsp4j.BuildClient {
  override def onBuildLogMessage(params: bsp4j.LogMessageParams): Unit =
    forwardToOpt.foreach(_.onBuildLogMessage(params))
  override def onBuildPublishDiagnostics(params: bsp4j.PublishDiagnosticsParams): Unit =
    forwardToOpt.foreach(_.onBuildPublishDiagnostics(params))
  override def onBuildShowMessage(params: bsp4j.ShowMessageParams): Unit =
    forwardToOpt.foreach(_.onBuildShowMessage(params))
  override def onBuildTargetDidChange(params: bsp4j.DidChangeBuildTarget): Unit =
    forwardToOpt.foreach(_.onBuildTargetDidChange(params))
  override def onBuildTaskFinish(params: bsp4j.TaskFinishParams): Unit =
    forwardToOpt.foreach(_.onBuildTaskFinish(params))
  override def onBuildTaskProgress(params: bsp4j.TaskProgressParams): Unit =
    forwardToOpt.foreach(_.onBuildTaskProgress(params))
  override def onBuildTaskStart(params: bsp4j.TaskStartParams): Unit =
    forwardToOpt.foreach(_.onBuildTaskStart(params))
}
