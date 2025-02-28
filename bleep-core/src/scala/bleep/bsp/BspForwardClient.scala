package bleep.bsp

import bleep.internal.BspClientDisplayProgress
import ch.epfl.scala.bsp4j
import ryddig.Logger

class BspForwardClient(var forwardToOpt: Option[bsp4j.BuildClient], logger: Logger) extends bsp4j.BuildClient {
  override def onBuildLogMessage(params: bsp4j.LogMessageParams): Unit =
    logger.withOptContext("originId", Option(params.getOriginId)).log(BspClientDisplayProgress.logLevelFor(params.getType), params.getMessage)
  override def onBuildPublishDiagnostics(params: bsp4j.PublishDiagnosticsParams): Unit =
    forwardToOpt.foreach(_.onBuildPublishDiagnostics(params))
  override def onBuildShowMessage(params: bsp4j.ShowMessageParams): Unit =
    logger.withOptContext("originId", Option(params.getOriginId)).log(BspClientDisplayProgress.logLevelFor(params.getType), params.getMessage)
  override def onBuildTargetDidChange(params: bsp4j.DidChangeBuildTarget): Unit =
    forwardToOpt.foreach(_.onBuildTargetDidChange(params))
  override def onBuildTaskFinish(params: bsp4j.TaskFinishParams): Unit =
    forwardToOpt.foreach(_.onBuildTaskFinish(params))
  override def onBuildTaskProgress(params: bsp4j.TaskProgressParams): Unit =
    forwardToOpt.foreach(_.onBuildTaskProgress(params))
  override def onBuildTaskStart(params: bsp4j.TaskStartParams): Unit =
    forwardToOpt.foreach(_.onBuildTaskStart(params))
}
