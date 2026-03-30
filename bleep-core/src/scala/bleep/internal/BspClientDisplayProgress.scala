package bleep
package internal

import ch.epfl.scala.bsp4j
import ch.epfl.scala.bsp4j.{BuildTargetIdentifier, MessageType}
import fansi.{Bold, Str}
import ryddig.{LogLevel, Logger}

import scala.collection.mutable

// a bsp client which will display compilation diagnostics and progress to a logger
object BspClientDisplayProgress {

  /** Convert lsp4j Either[String, Integer] to Option[String] for logging */
  private[internal] def eitherToString(eid: org.eclipse.lsp4j.jsonrpc.messages.Either[String, Integer]): Option[String] =
    Option(eid).map(e => if (e.isLeft) e.getLeft else e.getRight.toString)

  def apply(logger: Logger): BspClientDisplayProgress =
    new BspClientDisplayProgress(logger.withPath("BSP"), mutable.ListBuffer.empty)

  def logLevelFor(messageType: MessageType): LogLevel =
    messageType match {
      case bsp4j.MessageType.ERROR   => LogLevel.error
      case bsp4j.MessageType.WARNING => LogLevel.warn
      case bsp4j.MessageType.INFO    => LogLevel.info
      case bsp4j.MessageType.LOG     => LogLevel.debug
    }
}

class BspClientDisplayProgress(
    logger: Logger,
    var failed: mutable.ListBuffer[bsp4j.BuildTargetIdentifier]
) extends bsp4j.BuildClient {
  def extract(anyRef: AnyRef): Option[BuildTargetIdentifier] =
    anyRef match {
      case obj: com.google.gson.JsonObject =>
        obj.get("target") match {
          case target: com.google.gson.JsonObject =>
            target.get("uri") match {
              case str: com.google.gson.JsonPrimitive => Some(new BuildTargetIdentifier(str.getAsString))
              case _                                  => None
            }
          case _ => None
        }
      case _ => None
    }

  def renderBuildTarget(buildTargetId: BuildTargetIdentifier): Str =
    Bold.On(Str(buildTargetId.getUri.split("=").last))

  override def onBuildShowMessage(params: bsp4j.ShowMessageParams): Unit =
    logger.withOptContext("originId", Option(params.getOriginId)).log(BspClientDisplayProgress.logLevelFor(params.getType), params.getMessage)

  override def onBuildLogMessage(params: bsp4j.LogMessageParams): Unit =
    logger.withOptContext("originId", Option(params.getOriginId)).log(BspClientDisplayProgress.logLevelFor(params.getType), params.getMessage)

  override def onBuildTaskStart(params: bsp4j.TaskStartParams): Unit = ()

  override def onBuildTaskProgress(params: bsp4j.TaskProgressParams): Unit = ()

  override def onBuildTaskFinish(params: bsp4j.TaskFinishParams): Unit =
    extract(params.getData).foreach { id =>
      params.getStatus match {
        case bsp4j.StatusCode.OK        => ()
        case bsp4j.StatusCode.ERROR     => failed += id
        case bsp4j.StatusCode.CANCELLED => ()
      }
    }

  override def onBuildPublishDiagnostics(params: bsp4j.PublishDiagnosticsParams): Unit =
    params.getDiagnostics.forEach { d =>
      val logLevel = Option(d.getSeverity) match {
        case Some(bsp4j.DiagnosticSeverity.ERROR)       => LogLevel.error
        case Some(bsp4j.DiagnosticSeverity.WARNING)     => LogLevel.warn
        case Some(bsp4j.DiagnosticSeverity.INFORMATION) => LogLevel.info
        case Some(bsp4j.DiagnosticSeverity.HINT)        => LogLevel.info
        case None                                       => LogLevel.info
      }

      val location = Str.join(
        List(
          params.getTextDocument.getUri,
          ":",
          (d.getRange.getStart.getLine + 1).toString,
          ":",
          d.getRange.getStart.getCharacter.toString,
          " until ",
          (d.getRange.getEnd.getLine + 1).toString,
          ":",
          d.getRange.getEnd.getCharacter.toString
        )
      )

      logger
        .withOptContext("code", BspClientDisplayProgress.eitherToString(d.getCode))
        .withContext("location", location)
        .log(logLevel, Str(renderBuildTarget(params.getBuildTarget), Str(" "), Str("\n" + d.getMessage + "\n")))
    }

  override def onBuildTargetDidChange(params: bsp4j.DidChangeBuildTarget): Unit = logger.info(s"Build target changed: $params")
  override def onRunPrintStdout(params: bsp4j.PrintParams): Unit = logger.info(params.getMessage)
  override def onRunPrintStderr(params: bsp4j.PrintParams): Unit = logger.warn(params.getMessage)
}
