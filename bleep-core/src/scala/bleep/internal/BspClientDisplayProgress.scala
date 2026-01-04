package bleep
package internal

import ch.epfl.scala.bsp4j
import ch.epfl.scala.bsp4j.{BuildTargetIdentifier, MessageType}
import fansi.{Bold, Str}
import ryddig.{LogLevel, Logger, LoggerFn, TypedLogger}

import scala.collection.mutable

// a bsp client which will display compilation diagnostics and progress to a logger
object BspClientDisplayProgress {
  def apply(logger: Logger): BspClientDisplayProgress = {
    // TerminalSizeCache constructor registers a SIGWINCH handler which doesn't exist on Windows.
    // Check for Windows first to avoid even loading the class which triggers static initialization.
    val isWindows = System.getProperty("os.name", "").toLowerCase.contains("win")
    val terminalSizeCache: Option[io.github.alexarchambault.nativeterm.TerminalSizeCache] =
      if (isWindows) None
      else {
        try Some(new io.github.alexarchambault.nativeterm.TerminalSizeCache())
        catch { case _: Throwable => None }
      }
    new BspClientDisplayProgress(logger.withPath("BSP"), mutable.SortedMap.empty(Ordering.by(_.getUri)), mutable.ListBuffer.empty, terminalSizeCache)
  }

  def logLevelFor(messageType: MessageType): LogLevel =
    messageType match {
      case bsp4j.MessageType.ERROR   => LogLevel.error
      case bsp4j.MessageType.WARNING => LogLevel.warn
      case bsp4j.MessageType.INFO    => LogLevel.info
      case bsp4j.MessageType.LOG     => LogLevel.debug
    }

  /** Renders progress items to fit within the given terminal width
    * @param progressItems
    *   List of progress items to render
    * @param remainingCount
    *   Number of items not shown
    * @param terminalWidth
    *   Terminal width in columns
    * @return
    *   Rendered progress string
    */
  def renderProgress(progressItems: List[fansi.Str], remainingCount: Int, terminalWidth: Int): String = {
    val prefix = "Compiling "
    val suffix = if (remainingCount == 0) "" else s" +$remainingCount"
    val separator = ", "

    val fixedWidth = prefix.length + suffix.length
    val availableWidth = (terminalWidth - fixedWidth).max(20) // Ensure at least 20 chars for content

    // Build progress string that fits terminal width
    val items = new StringBuilder
    var currentWidth = 0
    var first = true
    var itemCount = 0

    for (item <- progressItems) {
      val itemStr = item.render
      val itemWidth = if (first) itemStr.length else separator.length + itemStr.length

      if (currentWidth + itemWidth <= availableWidth) {
        if (!first) items.append(separator)
        items.append(itemStr)
        currentWidth += itemWidth
        first = false
        itemCount += 1
      }
    }

    // If we couldn't fit all items, adjust the suffix
    val actualSuffix = if (itemCount < progressItems.length) {
      val hiddenCount = progressItems.length - itemCount + remainingCount
      s" +$hiddenCount"
    } else {
      suffix
    }

    prefix + items.toString + actualSuffix
  }
}

class BspClientDisplayProgress(
    logger: Logger,
    active: mutable.SortedMap[bsp4j.BuildTargetIdentifier, Option[bsp4j.TaskProgressParams]],
    var failed: mutable.ListBuffer[bsp4j.BuildTargetIdentifier],
    terminalSizeCache: Option[io.github.alexarchambault.nativeterm.TerminalSizeCache]
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

      val progressItems = byMostProgress
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

      // Calculate available width for progress items
      val termWidth =
        terminalSizeCache match {
          case Some(cache) =>
            try {
              val size = cache.getSize()
              if (size != null && size.getWidth() > 0) size.getWidth() else 80
            } catch {
              case _: Exception => 80
            }
          case None => 80
        }

      val progress = BspClientDisplayProgress.renderProgress(progressItems, rest.size, termWidth)

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
