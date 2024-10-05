package bleep
package internal

import ch.epfl.scala.bsp4j
import ch.epfl.scala.bsp4j.{BuildTargetIdentifier, MessageType}
import fansi.{Bold, Str}
import ryddig.{LogLevel, Logger, LoggerFn, TypedLogger}

import scala.collection.mutable

// a bsp client which will display compilation diagnostics and progress to a logger
object BspClientDisplayProgress {
  def apply(logger: Logger): BspClientDisplayProgress =
    new BspClientDisplayProgress(logger.withPath("BSP"), mutable.SortedMap.empty(Ordering.by(_.getUri)), mutable.ListBuffer.empty)

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
    active: mutable.SortedMap[bsp4j.BuildTargetIdentifier, Option[bsp4j.TaskProgressParams]],
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

  implicit class LoggerOps(logger: Logger) {
    def progressMonitor: Option[LoggerFn] =
      logger match {
        case logger: TypedLogger[?] => logger.progressMonitor
      }
  }

  val DisplayN = 4
  var lastProgress = Option.empty[String]
  def render(): Unit =
    logger.progressMonitor.foreach { pm =>
      val byMostProgress = active.toList.sortBy(_._2.fold(0L)(-_.getProgress))
      val rest = byMostProgress.drop(DisplayN)

      val progress = byMostProgress
        .take(DisplayN)
        .map { case (buildTargetId, maybeProgress) =>
          val percentage: String =
            maybeProgress match {
              case Some(progress) =>
                val percentage = progress.getProgress.toDouble / progress.getTotal * 100
                s"${percentage.toInt}%"
              case None => "started"
            }
          Str.join(List(renderBuildTarget(buildTargetId), ": ", percentage))
        }
        .mkString("Compiling ", ", ", if (rest.isEmpty) "" else s" +${rest.size}")

      // avoid duplicate lines. very visible on web-based terminals which don't erase lines
      if (lastProgress.contains(progress)) ()
      else {
        lastProgress = Some(progress)
        pm.info(progress)
      }
    }

  def renderBuildTarget(buildTargetId: BuildTargetIdentifier): Str =
    Bold.On(Str(buildTargetId.getUri.split("=").last))

  override def onBuildShowMessage(params: bsp4j.ShowMessageParams): Unit =
    logger.withOptContext("originId", Option(params.getOriginId)).log(BspClientDisplayProgress.logLevelFor(params.getType), params.getMessage)

  override def onBuildLogMessage(params: bsp4j.LogMessageParams): Unit =
    logger.withOptContext("originId", Option(params.getOriginId)).log(BspClientDisplayProgress.logLevelFor(params.getType), params.getMessage)

  override def onBuildTaskStart(params: bsp4j.TaskStartParams): Unit =
    extract(params.getData).foreach { id =>
      active(id) = None
      render()
    }

  override def onBuildTaskProgress(params: bsp4j.TaskProgressParams): Unit =
    extract(params.getData).foreach { id =>
      active(id) = Some(params)
      render()
    }

  override def onBuildTaskFinish(params: bsp4j.TaskFinishParams): Unit =
    extract(params.getData).foreach { id =>
      active.remove(id).discard()
      params.getStatus match {
        case bsp4j.StatusCode.OK        => ()
        case bsp4j.StatusCode.ERROR     => failed += id
        case bsp4j.StatusCode.CANCELLED => ()
      }
      render()
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
        .withOptContext("code", Option(d.getCode))
        .withContext("location", location)
        .log(logLevel, Str(renderBuildTarget(params.getBuildTarget), Str(" "), Str(d.getMessage)))
    }

  override def onBuildTargetDidChange(params: bsp4j.DidChangeBuildTarget): Unit = println(params)
}
