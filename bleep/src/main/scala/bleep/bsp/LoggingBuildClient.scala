package bleep.bsp

import bleep.internal.stderr
import ch.epfl.scala.bsp4j

trait LoggingBuildClient extends bsp4j.BuildClient {
  protected def underlying: bsp4j.BuildClient
  override def onBuildLogMessage(params: bsp4j.LogMessageParams): Unit =
    underlying.onBuildLogMessage(stderr.log(params))
  override def onBuildPublishDiagnostics(params: bsp4j.PublishDiagnosticsParams): Unit =
    underlying.onBuildPublishDiagnostics(stderr.log(params))
  override def onBuildShowMessage(params: bsp4j.ShowMessageParams): Unit =
    underlying.onBuildShowMessage(stderr.log(params))
  override def onBuildTargetDidChange(params: bsp4j.DidChangeBuildTarget): Unit =
    underlying.onBuildTargetDidChange(stderr.log(params))
  override def onBuildTaskFinish(params: bsp4j.TaskFinishParams): Unit =
    underlying.onBuildTaskFinish(stderr.log(params))
  override def onBuildTaskProgress(params: bsp4j.TaskProgressParams): Unit =
    underlying.onBuildTaskProgress(stderr.log(params))
  override def onBuildTaskStart(params: bsp4j.TaskStartParams): Unit =
    underlying.onBuildTaskStart(stderr.log(params))
}
