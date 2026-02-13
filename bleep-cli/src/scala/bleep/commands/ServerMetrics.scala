package bleep
package commands

import bleep.internal.FileUtils
import com.google.gson.{JsonObject, JsonParser}
import ryddig.Logger

import java.nio.file.{Files, Path}
import java.util.Locale
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters._
import scala.jdk.StreamConverters._

case class ServerMetrics(logger: Logger, userPaths: UserPaths, pid: Option[Long]) extends BleepCommand {

  override def run(): Either[BleepException, Unit] =
    findMetricsFile(userPaths.bspSocketDir) match {
      case None =>
        val msg = pid match {
          case Some(p) => s"No metrics found for PID $p. Check that the server wrote metrics.jsonl."
          case None    => "No metrics found. Run a compilation first to generate BSP server metrics."
        }
        Left(new BleepException.Text(msg))
      case Some(metricsPath) =>
        val events = parseMetrics(metricsPath)
        val html = generateHtml(events)
        val tempFile = Files.createTempFile("bleep-metrics-", ".html")
        Files.writeString(tempFile, html)
        java.awt.Desktop.getDesktop.browse(tempFile.toUri)
        logger.info(s"Dashboard opened: $tempFile")
        logger.info(s"Metrics source: $metricsPath")
        Right(())
    }

  private def findMetricsFile(socketDir: Path): Option[Path] = {
    if (!FileUtils.exists(socketDir)) return None
    pid match {
      case Some(targetPid) =>
        // Find socket dir whose pid file matches the given PID
        Files.list(socketDir).toScala(List).flatMap { dir =>
          val pidFile = dir.resolve("pid")
          val mf = dir.resolve("metrics.jsonl")
          if (FileUtils.exists(pidFile) && FileUtils.exists(mf)) {
            val filePid = Files.readString(pidFile).trim.toLong
            if (filePid == targetPid) Some(mf) else None
          } else None
        }.headOption
      case None =>
        // Find most recently modified metrics.jsonl
        val candidates = Files.list(socketDir).toScala(List).flatMap { dir =>
          val mf = dir.resolve("metrics.jsonl")
          if (FileUtils.exists(mf)) Some(mf) else None
        }
        if (candidates.isEmpty) None
        else Some(candidates.maxBy(f => Files.getLastModifiedTime(f).toMillis))
    }
  }

  private class Events {
    val jvm: ArrayBuffer[JsonObject] = ArrayBuffer.empty
    val compileStart: ArrayBuffer[JsonObject] = ArrayBuffer.empty
    val compileEnd: ArrayBuffer[JsonObject] = ArrayBuffer.empty
    val buildStart: ArrayBuffer[JsonObject] = ArrayBuffer.empty
    val buildEnd: ArrayBuffer[JsonObject] = ArrayBuffer.empty
    val cacheEvict: ArrayBuffer[JsonObject] = ArrayBuffer.empty
    val cleanCache: ArrayBuffer[JsonObject] = ArrayBuffer.empty
    val connectionOpen: ArrayBuffer[JsonObject] = ArrayBuffer.empty
    val connectionClose: ArrayBuffer[JsonObject] = ArrayBuffer.empty
    val sourcegenStart: ArrayBuffer[JsonObject] = ArrayBuffer.empty
    val sourcegenEnd: ArrayBuffer[JsonObject] = ArrayBuffer.empty
    val summary: ArrayBuffer[JsonObject] = ArrayBuffer.empty
  }

  private def parseMetrics(path: Path): Events = {
    val events = new Events
    Files.readAllLines(path).asScala.foreach { line =>
      val trimmed = line.trim
      if (trimmed.nonEmpty) {
        val obj = JsonParser.parseString(trimmed).getAsJsonObject
        val eventType = obj.get("type").getAsString
        eventType match {
          case "jvm"              => events.jvm += obj
          case "compile_start"    => events.compileStart += obj
          case "compile_end"      => events.compileEnd += obj
          case "build_start"      => events.buildStart += obj
          case "build_end"        => events.buildEnd += obj
          case "cache_evict"      => events.cacheEvict += obj
          case "clean_cache"      => events.cleanCache += obj
          case "connection_open"  => events.connectionOpen += obj
          case "connection_close" => events.connectionClose += obj
          case "sourcegen_start"  => events.sourcegenStart += obj
          case "sourcegen_end"    => events.sourcegenEnd += obj
          case "summary"          => events.summary += obj
          case _                  => ()
        }
      }
    }
    events
  }

  // ---- HTML generation ----

  private def generateHtml(events: Events): String = {
    val allTs = ArrayBuffer.empty[Long]
    val collectTs: JsonObject => Unit = obj => if (obj.has("ts")) allTs += obj.get("ts").getAsLong
    events.jvm.foreach(collectTs)
    events.compileStart.foreach(collectTs)
    events.compileEnd.foreach(collectTs)
    events.buildStart.foreach(collectTs)
    events.buildEnd.foreach(collectTs)
    events.cacheEvict.foreach(collectTs)
    events.cleanCache.foreach(collectTs)
    events.connectionOpen.foreach(collectTs)
    events.connectionClose.foreach(collectTs)
    events.sourcegenStart.foreach(collectTs)
    events.sourcegenEnd.foreach(collectTs)
    events.summary.foreach(collectTs)

    val t0 = if (allTs.isEmpty) 0L else allTs.min
    def relS(tsMs: Long): Double = (tsMs - t0) / 1000.0

    val chartCards = ArrayBuffer.empty[String]
    val plotCalls = ArrayBuffer.empty[String]

    val palette = Array("#3b82f6", "#ef4444", "#22c55e", "#f59e0b", "#8b5cf6", "#ec4899")

    def baseLayout(xTitle: String, yTitle: String): String =
      s"""{"margin":{"t":8,"r":16,"b":44,"l":60},"showlegend":true,"legend":{"orientation":"h","y":1.15,"x":0.5,"xanchor":"center","font":{"size":11}},"xaxis":{"title":{"text":"$xTitle","font":{"size":12}},"gridcolor":"#f0f0f0","zeroline":false},"yaxis":{"title":{"text":"$yTitle","font":{"size":12}},"gridcolor":"#f0f0f0","zeroline":false},"plot_bgcolor":"white","paper_bgcolor":"white","font":{"family":"Inter,system-ui,sans-serif","size":11,"color":"#374151"},"hoverlabel":{"font":{"family":"Inter,system-ui,sans-serif"}}}"""

    def addChart(id: String, title: String, traces: ArrayBuffer[String], layout: String, fullWidth: Boolean, heightPx: Int): Unit = {
      val colClass = if (fullWidth) " lg:col-span-2" else ""
      chartCards += s"""<div class="bg-white rounded-xl shadow-sm border border-gray-100 p-5$colClass"><h3 class="text-sm font-semibold text-gray-500 mb-3 uppercase tracking-wider">$title</h3><div id="$id" style="height:${heightPx}px"></div></div>"""
      plotCalls += s"""Plotly.newPlot('$id',[${traces.mkString(",")}],$layout,{responsive:true,displayModeBar:false});"""
    }

    // ---- 1. Heap Memory ----
    if (events.jvm.nonEmpty) {
      val t = ArrayBuffer.empty[String]
      val xArr = fmtDoubles(events.jvm.map(e => relS(e.get("ts").getAsLong)))
      t += scatterTrace(xArr, fmtLongs(events.jvm.map(_.get("heap_used_mb").getAsLong)), "Used", "#3b82f6", "solid", "none", "lines")
      t += scatterTrace(xArr, fmtLongs(events.jvm.map(_.get("heap_committed_mb").getAsLong)), "Committed", "#f59e0b", "dash", "none", "lines")
      t += scatterTrace(xArr, fmtLongs(events.jvm.map(_.get("heap_max_mb").getAsLong)), "Max", "#ef4444", "dot", "none", "lines")
      addChart("heap", "Heap Memory (MB)", t, baseLayout("Time (s)", "MB"), false, 280)
    }

    // ---- 2. GC Activity ----
    if (events.jvm.nonEmpty) {
      val t = ArrayBuffer.empty[String]
      val xArr = fmtDoubles(events.jvm.map(e => relS(e.get("ts").getAsLong)))
      val gcCounts = events.jvm.map { e =>
        var total = 0L
        e.getAsJsonArray("gc").forEach(g => total += g.getAsJsonObject.get("count").getAsLong)
        total
      }
      val gcTimes = events.jvm.map { e =>
        var total = 0L
        e.getAsJsonArray("gc").forEach(g => total += g.getAsJsonObject.get("time_ms").getAsLong)
        total
      }
      t += scatterTrace(xArr, fmtLongs(gcCounts), "Count", "#8b5cf6", "solid", "none", "lines")
      t += scatterTrace(xArr, fmtLongs(gcTimes), "Time (ms)", "#22c55e", "solid", "none", "lines")
      addChart("gc", "GC Activity (Cumulative)", t, baseLayout("Time (s)", "Count / ms"), false, 280)
    }

    // ---- 3. Thread Count ----
    if (events.jvm.nonEmpty) {
      val t = ArrayBuffer.empty[String]
      val xArr = fmtDoubles(events.jvm.map(e => relS(e.get("ts").getAsLong)))
      t += scatterTrace(xArr, fmtLongs(events.jvm.map(_.get("threads").getAsLong)), "Live", "#14b8a6", "solid", "none", "lines")
      t += scatterTrace(xArr, fmtLongs(events.jvm.map(_.get("peak_threads").getAsLong)), "Peak", "#ef4444", "dot", "none", "lines")
      t += scatterTrace(xArr, fmtLongs(events.jvm.map(_.get("daemon_threads").getAsLong)), "Daemon", "#6b7280", "dash", "none", "lines")
      addChart("threads", "Thread Count", t, baseLayout("Time (s)", "Threads"), false, 280)
    }

    // ---- 4. CPU Load ----
    if (events.jvm.nonEmpty) {
      val t = ArrayBuffer.empty[String]
      val xArr = fmtDoubles(events.jvm.map(e => relS(e.get("ts").getAsLong)))
      t += scatterTrace(xArr, fmtDoubles(events.jvm.map(_.get("cpu_process").getAsDouble)), "Process", "#3b82f6", "solid", "none", "lines")
      t += scatterTrace(xArr, fmtDoubles(events.jvm.map(_.get("cpu_system").getAsDouble)), "System", "#f59e0b", "solid", "none", "lines")
      addChart("cpu", "CPU Load", t, baseLayout("Time (s)", "Load (0-1)"), false, 280)
    }

    // ---- 5. Concurrent Compilations ----
    if (events.jvm.nonEmpty) {
      val t = ArrayBuffer.empty[String]
      val xArr = fmtDoubles(events.jvm.map(e => relS(e.get("ts").getAsLong)))
      t += scatterTrace(xArr, fmtLongs(events.jvm.map(_.get("concurrent_compiles").getAsLong)), "Concurrent", "#6366f1", "solid", "tozeroy", "lines")
      addChart("concurrent", "Concurrent Compilations", t, baseLayout("Time (s)", "Count"), false, 280)
    }

    // ---- 6. Per-Project Compile Duration ----
    if (events.compileEnd.nonEmpty) {
      val t = ArrayBuffer.empty[String]
      val workspaces = events.compileEnd.map(e => getStr(e, "workspace")).distinct.sorted
      workspaces.zipWithIndex.foreach { case (ws, i) =>
        val wsCompiles = events.compileEnd.filter(e => getStr(e, "workspace") == ws)
        val wsLabel = pathName(ws)
        val color = palette(i % palette.length)
        t += s"""{"type":"scatter","mode":"markers","x":${fmtDoubles(wsCompiles.map(e => relS(e.get("ts").getAsLong)))},"y":${fmtDoubles(wsCompiles.map(_.get("duration_ms").getAsLong / 1000.0))},"text":${fmtStrings(wsCompiles.map(e => getStr(e, "project")))},"name":"${escJson(wsLabel)}","marker":{"color":"$color","size":5,"opacity":0.7},"hovertemplate":"%{text}<br>%{y:.1f}s<extra></extra>"}"""
      }
      addChart("compile-scatter", "Per-Project Compile Duration", t, baseLayout("Time (s)", "Duration (s)"), false, 280)
    }

    // ---- 7. Build Duration Over Time ----
    if (events.buildEnd.nonEmpty) {
      val t = ArrayBuffer.empty[String]
      val workspaces = events.buildEnd.map(e => getStr(e, "workspace")).distinct.sorted
      workspaces.zipWithIndex.foreach { case (ws, i) =>
        val wsBuilds = events.buildEnd.filter(e => getStr(e, "workspace") == ws).sortBy(_.get("ts").getAsLong)
        val wsLabel = pathName(ws)
        val color = palette(i % palette.length)
        t += scatterTrace(
          fmtDoubles(wsBuilds.map(e => relS(e.get("ts").getAsLong))),
          fmtDoubles(wsBuilds.map(_.get("duration_ms").getAsLong / 1000.0)),
          wsLabel, color, "solid", "none", "lines+markers"
        )
      }
      addChart("build-dur", "Build Duration Over Time", t, baseLayout("Time (s)", "Duration (s)"), false, 280)
    }

    // ---- 8. Cache & Connection Events ----
    locally {
      val xs = ArrayBuffer.empty[Double]
      val labels = ArrayBuffer.empty[String]
      val colors = ArrayBuffer.empty[String]

      events.cacheEvict.foreach { e =>
        xs += relS(e.get("ts").getAsLong)
        labels += s"evict: ${getStr(e, "cache")}"
        colors += "#ef4444"
      }
      events.cleanCache.foreach { e =>
        xs += relS(e.get("ts").getAsLong)
        labels += s"clean: ${getStr(e, "project")}"
        colors += "#f59e0b"
      }
      events.connectionOpen.foreach { e =>
        xs += relS(e.get("ts").getAsLong)
        labels += s"open #${e.get("conn_id").getAsInt}"
        colors += "#22c55e"
      }
      events.connectionClose.foreach { e =>
        xs += relS(e.get("ts").getAsLong)
        labels += s"close #${e.get("conn_id").getAsInt}"
        colors += "#6b7280"
      }

      if (xs.nonEmpty) {
        val t = ArrayBuffer.empty[String]
        t += s"""{"type":"scatter","mode":"markers","x":${fmtDoubles(xs)},"y":${fmtLongs(Seq.fill(xs.size)(1L))},"text":${fmtStrings(labels)},"marker":{"color":${fmtStrings(colors)},"size":12,"symbol":"diamond"},"name":"Events","hovertemplate":"%{text}<br>t=%{x:.1f}s<extra></extra>","showlegend":false}"""
        addChart("events", "Cache & Connection Events", t, baseLayout("Time (s)", ""), false, 200)
      }
    }

    // ---- 9. Compilation Timeline (full width) ----
    if (events.compileStart.nonEmpty && events.compileEnd.nonEmpty) {
      val startMap = scala.collection.mutable.Map.empty[(String, String), ArrayBuffer[JsonObject]]
      events.compileStart.foreach { e =>
        val key = (getStr(e, "project"), getStr(e, "workspace"))
        startMap.getOrElseUpdate(key, ArrayBuffer.empty) += e
      }

      val barData = ArrayBuffer.empty[(String, String, Long, Boolean)]
      events.compileEnd.foreach { e =>
        val key = (getStr(e, "project"), getStr(e, "workspace"))
        startMap.get(key).foreach { starts =>
          if (starts.nonEmpty) {
            starts.remove(0): Unit
            barData += ((getStr(e, "project"), pathName(getStr(e, "workspace")), e.get("duration_ms").getAsLong, e.get("success").getAsBoolean))
          }
        }
      }

      if (barData.nonEmpty) {
        val recent = barData.takeRight(200)
        val barHeight = math.min(2000, math.max(350, recent.size * 16))
        val t = ArrayBuffer.empty[String]
        t += s"""{"type":"bar","orientation":"h","x":${fmtDoubles(recent.map(_._3 / 1000.0))},"y":${fmtStrings(recent.map(b => s"${b._1} (${b._2})"))},"name":"Compile Tasks","marker":{"color":${fmtStrings(recent.map(b => if (b._4) "#22c55e" else "#ef4444"))},"opacity":0.85,"line":{"width":0}},"hovertemplate":"%{y}<br>%{x:.1f}s<extra></extra>"}"""
        val tlLayout = s"""{"margin":{"t":8,"r":16,"b":44,"l":16},"showlegend":false,"xaxis":{"title":{"text":"Duration (s)","font":{"size":12}},"gridcolor":"#f0f0f0","zeroline":false,"side":"top"},"yaxis":{"automargin":true,"tickfont":{"size":10},"autorange":"reversed"},"plot_bgcolor":"white","paper_bgcolor":"white","font":{"family":"Inter,system-ui,sans-serif","size":11,"color":"#374151"},"bargap":0.15}"""

        chartCards += s"""<div class="bg-white rounded-xl shadow-sm border border-gray-100 p-5 lg:col-span-2"><h3 class="text-sm font-semibold text-gray-500 mb-3 uppercase tracking-wider">Compilation Timeline</h3><div id="timeline" style="height:${barHeight}px"></div></div>"""
        plotCalls += s"""Plotly.newPlot('timeline',[${t.mkString(",")}],$tlLayout,{responsive:true,displayModeBar:false});"""
      }
    }

    // ---- Summary statistics ----
    val totalCompiles = events.compileEnd.size
    val successfulCompiles = events.compileEnd.count(_.get("success").getAsBoolean)
    val failedCompiles = totalCompiles - successfulCompiles
    val avgCompileMs = if (totalCompiles > 0) events.compileEnd.map(_.get("duration_ms").getAsLong).sum.toDouble / totalCompiles else 0.0
    val maxConcurrent = if (events.jvm.nonEmpty) events.jvm.map(_.get("concurrent_compiles").getAsInt).max else 0
    val maxHeap = if (events.jvm.nonEmpty) events.jvm.map(_.get("heap_used_mb").getAsLong).max else 0L
    val totalGcMs = if (events.jvm.nonEmpty) events.jvm.map { e =>
      var total = 0L
      e.getAsJsonArray("gc").forEach(g => total += g.getAsJsonObject.get("time_ms").getAsLong)
      total
    }.max
    else 0L
    val totalBuilds = events.buildEnd.size
    val avgBuildMs = if (totalBuilds > 0) events.buildEnd.map(_.get("duration_ms").getAsLong).sum.toDouble / totalBuilds else 0.0

    val summaryMaxConcurrent = if (events.summary.nonEmpty) events.summary.head.get("max_concurrent_compiles").getAsInt else maxConcurrent
    val summaryMaxHeap = if (events.summary.nonEmpty) events.summary.head.get("max_heap_used_mb").getAsLong else maxHeap

    val successPct = if (totalCompiles > 0) f"${successfulCompiles.toDouble / totalCompiles * 100}%.0f" else "0"
    val timestamp = java.time.LocalDateTime.now().toString.take(16).replace('T', ' ')

    def stat(label: String, value: String, accent: String): String =
      s"""<div class="bg-white rounded-xl shadow-sm border border-gray-100 overflow-hidden">
<div style="height:3px;background:$accent"></div>
<div class="px-4 py-3">
<div class="text-xs font-medium text-gray-400 uppercase tracking-wider">$label</div>
<div class="text-xl font-bold text-gray-900 mt-1">$value</div>
</div></div>"""

    val statsHtml = s"""<div class="grid grid-cols-2 sm:grid-cols-4 gap-4 mb-6">
${stat("Total Builds", totalBuilds.toString, "#3b82f6")}
${stat("Total Compiles", totalCompiles.toString, "#6366f1")}
${stat("Success Rate", s"$successPct%", if (failedCompiles > 0) "#ef4444" else "#22c55e")}
${stat("Avg Compile", f"${avgCompileMs / 1000.0}%.1f s", "#f59e0b")}
${stat("Avg Build", f"${avgBuildMs / 1000.0}%.1f s", "#f59e0b")}
${stat("Max Concurrent", summaryMaxConcurrent.toString, "#8b5cf6")}
${stat("Max Heap", s"$summaryMaxHeap MB", "#ec4899")}
${stat("Total GC", f"${totalGcMs / 1000.0}%.1f s", "#14b8a6")}
</div>"""

    s"""<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<title>BSP Server Metrics</title>
<script src="https://cdn.plot.ly/plotly-2.35.2.min.js"></script>
<script src="https://cdn.tailwindcss.com"></script>
<style>
body { font-family: Inter, system-ui, -apple-system, sans-serif; }
.js-plotly-plot .plotly .modebar { display: none !important; }
</style>
</head>
<body class="bg-gray-50 min-h-screen antialiased">
<header class="bg-white border-b border-gray-200">
<div class="max-w-7xl mx-auto px-6 py-5 flex items-center justify-between">
<div>
<h1 class="text-2xl font-bold text-gray-900">BSP Server Metrics</h1>
<p class="text-sm text-gray-400 mt-0.5">Performance dashboard</p>
</div>
<div class="text-xs text-gray-400">$timestamp</div>
</div>
</header>
<main class="max-w-7xl mx-auto px-6 py-6">
$statsHtml
<div class="grid grid-cols-1 lg:grid-cols-2 gap-6">
${chartCards.mkString("\n")}
</div>
</main>
<script>
${plotCalls.mkString("\n")}
</script>
</body>
</html>"""
  }

  // ---- Helpers ----

  private def getStr(obj: JsonObject, field: String): String =
    if (obj.has(field)) obj.get(field).getAsString else ""

  private def pathName(path: String): String = {
    val lastSlash = math.max(path.lastIndexOf('/'), path.lastIndexOf('\\'))
    if (lastSlash >= 0) path.substring(lastSlash + 1) else path
  }

  private def escJson(s: String): String =
    s.replace("\\", "\\\\")
      .replace("\"", "\\\"")
      .replace("\n", "\\n")
      .replace("\r", "\\r")
      .replace("<", "\\u003c")

  private def fmtDoubles(values: Iterable[Double]): String =
    values.map(v => String.format(Locale.US, "%.3f", v: java.lang.Double)).mkString("[", ",", "]")

  private def fmtLongs(values: Iterable[Long]): String =
    values.mkString("[", ",", "]")

  private def fmtStrings(values: Iterable[String]): String =
    values.map(s => s""""${escJson(s)}"""").mkString("[", ",", "]")

  private def scatterTrace(x: String, y: String, name: String, color: String, dash: String, fill: String, mode: String): String = {
    val fillPart = if (fill == "none") "" else s""""fill":"$fill","""
    val lineDash = if (dash == "solid") "" else s""","dash":"$dash""""
    s"""{"type":"scatter","mode":"$mode","x":$x,"y":$y,"name":"${escJson(name)}",${fillPart}"line":{"color":"$color"$lineDash}}"""
  }
}
