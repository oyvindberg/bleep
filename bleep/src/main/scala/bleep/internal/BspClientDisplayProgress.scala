package bleep.internal

import bleep.logging.{LogLevel, Logger}
import ch.epfl.scala.bsp4j
import ch.epfl.scala.bsp4j.BuildTargetIdentifier
import fansi.{Bold, Str}

import scala.collection.mutable

// a bsp client which will display compilation diagnostics and progress to a logger
object BspClientDisplayProgress {
  def apply(logger: Logger) = new BspClientDisplayProgress(logger, mutable.SortedMap.empty(Ordering.by(_.getUri)))
}

class BspClientDisplayProgress(logger: Logger, active: mutable.SortedMap[bsp4j.BuildTargetIdentifier, bsp4j.TaskProgressParams]) extends bsp4j.BuildClient {
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

  def render(): Unit =
    logger.progressMonitor.foreach { pm =>
      val progress = active.toList
        .map { case (task, progress) =>
          val percentage: String = {
            val percentage = progress.getProgress.toDouble / progress.getTotal * 100
            s"${percentage.toInt}%"
          }
          Str.join(Bold.On(Str(task.getUri.split("=").last)), ": ", percentage)
        }
        .mkString("Compiling ", ", ", "")
      pm.info(progress)
    }

  override def onBuildShowMessage(params: bsp4j.ShowMessageParams): Unit =
    logger.withContext("type", params.getType.name).withContext("originId", params.getOriginId).info(s"show message: ${params.getMessage}")

  override def onBuildLogMessage(params: bsp4j.LogMessageParams): Unit =
    logger.withContext("type", params.getType.name()).withOptContext("originId", Option(params.getOriginId)).info(params.getMessage)

  override def onBuildTaskStart(params: bsp4j.TaskStartParams): Unit =
    ()

  override def onBuildTaskProgress(params: bsp4j.TaskProgressParams): Unit =
    extract(params.getData).foreach { id =>
      active(id) = params
      render()
    }
  override def onBuildTaskFinish(params: bsp4j.TaskFinishParams): Unit =
    extract(params.getData).foreach { id =>
      active.remove(id)
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
        params.getTextDocument.getUri,
        ":",
        d.getRange.getStart.getLine.toString,
        ":",
        d.getRange.getStart.getCharacter.toString,
        " until ",
        d.getRange.getEnd.getLine.toString,
        ":",
        d.getRange.getEnd.getCharacter.toString
      )

      logger.withContext(location).withOptContext("code", Option(d.getCode)).apply(logLevel, d.getMessage)
    }

  override def onBuildTargetDidChange(params: bsp4j.DidChangeBuildTarget): Unit = println(params)
}
