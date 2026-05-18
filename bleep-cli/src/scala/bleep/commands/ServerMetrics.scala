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

case class ServerMetrics(
    logger: Logger,
    userPaths: UserPaths,
    invocationMetricsRoot: Option[Path],
    pid: Option[Long],
    htmlOutput: Option[Path],
    jsonlOutput: Option[Path]
) extends BleepCommand {

  override def run(): Either[BleepException, Unit] = {
    val bspMetricsPaths = findBspMetricsFiles(userPaths.bspSocketDir)
    val invocationMetricsPaths = invocationMetricsRoot.toList.flatMap(findInvocationMetricsFiles)

    if (bspMetricsPaths.isEmpty && invocationMetricsPaths.isEmpty) {
      val msg = pid match {
        case Some(p) => s"No metrics found for PID $p. Check that the server wrote metrics.jsonl."
        case None    => "No metrics found. Run a compilation first to generate BSP server or per-invocation metrics."
      }
      Left(new BleepException.Text(msg))
    } else {
      val allPaths = bspMetricsPaths ++ invocationMetricsPaths
      logger.info(s"Reading ${allPaths.size} metrics file(s): ${allPaths.map(_.toString).mkString(", ")}")

      // Raw JSONL output: concatenate everything in timestamp order. Append-only semantics still hold across the merge — the JSONL files are independent log
      // streams from different processes (BSP daemon, CLI invocations). Output ordering matches the natural read of each file with files concatenated by
      // last-modified time so older runs come first.
      jsonlOutput.foreach { dst =>
        Option(dst.getParent).foreach(Files.createDirectories(_))
        val sorted = allPaths.sortBy(p => Files.getLastModifiedTime(p).toMillis)
        val w = Files.newBufferedWriter(dst)
        try
          sorted.foreach { p =>
            w.write(s"# source: $p\n")
            Files.readAllLines(p).forEach { line => w.write(line); w.newLine() }
          }
        finally w.close()
        logger.info(s"Wrote raw metrics JSONL: $dst")
      }

      val anyOutputRequested = htmlOutput.isDefined || jsonlOutput.isDefined
      if (htmlOutput.isDefined || !anyOutputRequested) {
        // Parse each file into its own Events, then merge. We tag every JsonObject with a synthetic `_scope` field (the source-file directory name) so the
        // span-graph builder can pair `_start`/`_end` events scope-by-scope — events from one BSP daemon run shouldn't end up parented under spans from a
        // separate CLI invocation just because their timestamps overlap.
        val merged = new Events
        allPaths.foreach { p =>
          val scope = scopeOf(p)
          val parsed = parseMetrics(p)
          tagScope(parsed, scope)
          mergeInto(merged, parsed)
        }

        // Pick the html destination (temp file if not provided), then write the trace files next to it. Same basename, different suffix — `.trace.json`
        // (Chrome Trace, drag into ui.perfetto.dev) and `.otlp.json` (OpenTelemetry, opens in Jaeger/Tempo/otel-desktop-viewer/etc).
        val htmlDst = htmlOutput.getOrElse(Files.createTempFile("bleep-metrics-", ".html"))
        Option(htmlDst.getParent).foreach(Files.createDirectories(_))
        val htmlName = htmlDst.getFileName.toString
        val baseName = if (htmlName.endsWith(".html")) htmlName.dropRight(5) else htmlName
        val parentDir = Option(htmlDst.getParent).getOrElse(Path.of("."))
        val traceChrome = parentDir.resolve(s"$baseName.trace.json")
        val traceOtlp = parentDir.resolve(s"$baseName.otlp.json")

        val graph = buildSpanGraph(merged)
        writeChromeTrace(graph, traceChrome)
        writeOtlpJson(graph, traceOtlp)
        logger.info(s"Wrote span trace (Chrome Trace, drop into ui.perfetto.dev): $traceChrome")
        logger.info(s"Wrote span trace (OTLP/JSON, otel-desktop-viewer / Jaeger / Tempo): $traceOtlp")

        val html = generateHtml(merged, Some(traceChrome), Some(traceOtlp))
        Files.writeString(htmlDst, html)
        logger.info(s"Wrote dashboard HTML: $htmlDst")

        if (!anyOutputRequested) {
          val os = System.getProperty("os.name", "").toLowerCase(Locale.ROOT)
          val openCmd =
            if (os.contains("mac")) Array("open", htmlDst.toString)
            else if (os.contains("win")) Array("cmd", "/c", "start", htmlDst.toString)
            else Array("xdg-open", htmlDst.toString)
          Runtime.getRuntime.exec(openCmd)
          logger.info(s"Dashboard opened: $htmlDst")
        }
      }

      Right(())
    }
  }

  /** Enumerate every `<metricsRoot>/<timestamp>-<pid>/metrics.jsonl` — one per past invocation. Ordered newest-first by mtime so the dashboard's "recent
    * invocations" view leads with the most relevant data.
    */
  private def findInvocationMetricsFiles(root: Path): List[Path] = {
    if (!FileUtils.exists(root)) return Nil
    Files
      .list(root)
      .toScala(List)
      .flatMap { dir =>
        val mf = dir.resolve("metrics.jsonl")
        if (FileUtils.exists(mf)) Some(mf) else None
      }
      .sortBy(f => -Files.getLastModifiedTime(f).toMillis)
  }

  /** Derive a short scope label from a metrics file path. BSP daemon files live under `<socketDir>/<hash>/metrics.jsonl` — we surface those as `bsp-<hash>`.
    * CLI invocation files live under `<workspace>/.bleep/metrics/<timestamp>-<pid>/metrics.jsonl` — we just use the directory name. The label is the natural
    * grouping unit for spans: everything from one file lives in one scope and shouldn't be parented under spans from a different file.
    */
  private def scopeOf(path: Path): String = {
    val parent = Option(path.getParent).map(_.getFileName.toString).getOrElse("unknown")
    val grandparent = Option(path.getParent).flatMap(p => Option(p.getParent)).map(_.getFileName.toString).getOrElse("")
    if (grandparent == "socket") s"bsp-$parent" else parent
  }

  /** Stamp every JsonObject in `events` with `_scope: <name>` so the span renderer can group them. The synthetic field uses an underscore prefix to avoid
    * colliding with any real protocol field; nothing on the writer side ever emits a `_scope`.
    */
  private def tagScope(events: Events, scope: String): Unit = {
    def tag(buf: ArrayBuffer[JsonObject]): Unit = buf.foreach(_.addProperty("_scope", scope))
    tag(events.jvm)
    tag(events.compileStart)
    tag(events.compileEnd)
    tag(events.buildStart)
    tag(events.buildEnd)
    tag(events.cacheEvict)
    tag(events.cleanCache)
    tag(events.connectionOpen)
    tag(events.connectionClose)
    tag(events.sourcegenStart)
    tag(events.sourcegenEnd)
    tag(events.summary)
    tag(events.oomPressure)
    tag(events.oomCrash)
    tag(events.suiteStart)
    tag(events.suiteEnd)
    tag(events.testEnd)
    tag(events.subprocessStart)
    tag(events.subprocessEnd)
    tag(events.projectTestStart)
    tag(events.projectTestEnd)
    tag(events.requestStart)
    tag(events.requestEnd)
    tag(events.processEvents)
    tag(events.invocationEnd)
  }

  /** Append `src`'s buffers into `dst`. Cheap because all the buffers hold `JsonObject`s by reference. */
  private def mergeInto(dst: Events, src: Events): Unit = {
    dst.jvm ++= src.jvm
    dst.compileStart ++= src.compileStart
    dst.compileEnd ++= src.compileEnd
    dst.buildStart ++= src.buildStart
    dst.buildEnd ++= src.buildEnd
    dst.cacheEvict ++= src.cacheEvict
    dst.cleanCache ++= src.cleanCache
    dst.connectionOpen ++= src.connectionOpen
    dst.connectionClose ++= src.connectionClose
    dst.sourcegenStart ++= src.sourcegenStart
    dst.sourcegenEnd ++= src.sourcegenEnd
    dst.summary ++= src.summary
    dst.oomPressure ++= src.oomPressure
    dst.oomCrash ++= src.oomCrash
    dst.suiteStart ++= src.suiteStart
    dst.suiteEnd ++= src.suiteEnd
    dst.testEnd ++= src.testEnd
    dst.subprocessStart ++= src.subprocessStart
    dst.subprocessEnd ++= src.subprocessEnd
    dst.projectTestStart ++= src.projectTestStart
    dst.projectTestEnd ++= src.projectTestEnd
    dst.requestStart ++= src.requestStart
    dst.requestEnd ++= src.requestEnd
    dst.processEvents ++= src.processEvents
    dst.invocationEnd ++= src.invocationEnd
  }

  /** Collect every BSP daemon's `metrics.jsonl` under `socketDir`. With `pid` set we filter to just that daemon (one match by `pid` file); with `pid` empty we
    * return everything — there's one socket dir per workspace, and tests in one workspace will only show up by reading that workspace's daemon file (the scope
    * tagging keeps them visually partitioned in the span tree).
    */
  private def findBspMetricsFiles(socketDir: Path): List[Path] = {
    if (!FileUtils.exists(socketDir)) return Nil
    val allDaemons = Files.list(socketDir).toScala(List).flatMap { dir =>
      val mf = dir.resolve("metrics.jsonl")
      if (FileUtils.exists(mf)) Some((dir, mf)) else None
    }
    pid match {
      case Some(targetPid) =>
        allDaemons.flatMap { case (dir, mf) =>
          val pidFile = dir.resolve("pid")
          if (FileUtils.exists(pidFile) && Files.readString(pidFile).trim.toLong == targetPid) Some(mf) else None
        }
      case None =>
        allDaemons.map(_._2).sortBy(f => Files.getLastModifiedTime(f).toMillis)
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
    val oomPressure: ArrayBuffer[JsonObject] = ArrayBuffer.empty
    val oomCrash: ArrayBuffer[JsonObject] = ArrayBuffer.empty
    val suiteStart: ArrayBuffer[JsonObject] = ArrayBuffer.empty
    val suiteEnd: ArrayBuffer[JsonObject] = ArrayBuffer.empty
    val testEnd: ArrayBuffer[JsonObject] = ArrayBuffer.empty
    val subprocessStart: ArrayBuffer[JsonObject] = ArrayBuffer.empty
    val subprocessEnd: ArrayBuffer[JsonObject] = ArrayBuffer.empty
    val projectTestStart: ArrayBuffer[JsonObject] = ArrayBuffer.empty
    val projectTestEnd: ArrayBuffer[JsonObject] = ArrayBuffer.empty
    val requestStart: ArrayBuffer[JsonObject] = ArrayBuffer.empty
    val requestEnd: ArrayBuffer[JsonObject] = ArrayBuffer.empty
    val processEvents: ArrayBuffer[JsonObject] = ArrayBuffer.empty
    val invocationEnd: ArrayBuffer[JsonObject] = ArrayBuffer.empty
  }

  private def parseMetrics(path: Path): Events = {
    val events = new Events
    Files.readAllLines(path).asScala.foreach { line =>
      val trimmed = line.trim
      if (trimmed.nonEmpty) {
        val obj = JsonParser.parseString(trimmed).getAsJsonObject
        val eventType = obj.get("type").getAsString
        eventType match {
          case "jvm"                => events.jvm += obj
          case "compile_start"      => events.compileStart += obj
          case "compile_end"        => events.compileEnd += obj
          case "build_start"        => events.buildStart += obj
          case "build_end"          => events.buildEnd += obj
          case "cache_evict"        => events.cacheEvict += obj
          case "clean_cache"        => events.cleanCache += obj
          case "connection_open"    => events.connectionOpen += obj
          case "connection_close"   => events.connectionClose += obj
          case "sourcegen_start"    => events.sourcegenStart += obj
          case "sourcegen_end"      => events.sourcegenEnd += obj
          case "summary"            => events.summary += obj
          case "oom_pressure"       => events.oomPressure += obj
          case "oom_crash"          => events.oomCrash += obj
          case "suite_start"        => events.suiteStart += obj
          case "suite_end"          => events.suiteEnd += obj
          case "test_end"           => events.testEnd += obj
          case "subprocess_start"   => events.subprocessStart += obj
          case "subprocess_end"     => events.subprocessEnd += obj
          case "project_test_start" => events.projectTestStart += obj
          case "project_test_end"   => events.projectTestEnd += obj
          case "request_start"      => events.requestStart += obj
          case "request_end"        => events.requestEnd += obj
          case "process"            => events.processEvents += obj
          case "invocation_end"     => events.invocationEnd += obj
          case _                    => ()
        }
      }
    }
    events
  }

  // ---- Span graph + trace exporters ----

  /** One paired start/end from the metrics stream. `originId` is the BSP request id when known (set on `request_*`, `project_test_*`, and inherited for other
    * events emitted during the request — currently inferred via time-containment within the same scope). Carrying it on `Span` lets the OTLP writer group all
    * spans of one bleep command under a single `traceId`, so Jaeger / Tempo / otel-desktop-viewer render the full request tree as one cohesive trace.
    */
  private case class Span(
      scope: String,
      kind: String,
      label: String,
      startMs: Long,
      endMs: Long,
      success: Option[Boolean],
      peakRssMb: Option[Long],
      cmd: Option[String],
      originId: Option[String]
  )

  /** Spans + the inferred parent index for each span (-1 = root). Index is into `spans`. The renderer side reads this as a tree. */
  private case class SpanGraph(spans: Array[Span], parentIdx: Array[Int])

  /** Build the unified span graph from the merged event buffers. Pairs `_start` with `_end` events scope-by-scope (so spans from different metrics files never
    * pair with each other), then infers parent-child relationships from time containment within scope, and prepends a synthetic per-scope "root" span so every
    * scope has a top-level entry in the resulting trace.
    */
  private def buildSpanGraph(events: Events): SpanGraph = {
    val spans = ArrayBuffer.empty[Span]

    val noBool: JsonObject => Option[Boolean] = _ => None
    val noLong: JsonObject => Option[Long] = _ => None
    val noStr: JsonObject => Option[String] = _ => None
    val readSuccess: JsonObject => Option[Boolean] = e => if (e.has("success")) Some(e.get("success").getAsBoolean) else None
    def scopeOfObj(o: JsonObject): String = if (o.has("_scope")) o.get("_scope").getAsString else "unknown"

    def pairSpans[K](
        starts: ArrayBuffer[JsonObject],
        ends: ArrayBuffer[JsonObject],
        startKey: JsonObject => K,
        endKey: JsonObject => K,
        toLabel: JsonObject => String,
        kind: String,
        success: JsonObject => Option[Boolean],
        peakRssMb: JsonObject => Option[Long],
        cmd: JsonObject => Option[String],
        originId: JsonObject => Option[String]
    ): Unit = {
      val pending = scala.collection.mutable.Map.empty[(String, K), scala.collection.mutable.Queue[JsonObject]]
      starts.foreach { s =>
        val k = (scopeOfObj(s), startKey(s))
        pending.getOrElseUpdate(k, scala.collection.mutable.Queue.empty[JsonObject]).enqueue(s)
      }
      ends.foreach { e =>
        val k = (scopeOfObj(e), endKey(e))
        pending.get(k).flatMap(q => if (q.nonEmpty) Some(q.dequeue()) else None) match {
          case Some(s) =>
            spans += Span(scopeOfObj(s), kind, toLabel(s), s.get("ts").getAsLong, e.get("ts").getAsLong, success(e), peakRssMb(e), cmd(s), originId(s))
          case None => ()
        }
      }
    }

    val readOriginId: JsonObject => Option[String] = o => if (o.has("origin_id")) Some(getStr(o, "origin_id")).filter(_.nonEmpty) else None

    // Request span: the BSP-request boundary, the root of one bleep-command trace. Keyed by `origin_id` so the start/end pair only matches across two events
    // from the same request even if multiple requests are interleaved.
    pairSpans(
      events.requestStart,
      events.requestEnd,
      startKey = o => getStr(o, "origin_id"),
      endKey = o => getStr(o, "origin_id"),
      toLabel = o => s"${getStr(o, "method")}${if (o.has("projects")) " " + o.get("projects").toString else ""}",
      kind = "request",
      success = readSuccess,
      peakRssMb = noLong,
      cmd = noStr,
      originId = readOriginId
    )
    pairSpans(
      events.buildStart,
      events.buildEnd,
      startKey = o => getStr(o, "workspace"),
      endKey = o => getStr(o, "workspace"),
      toLabel = o => s"build (${pathName(getStr(o, "workspace"))})",
      kind = "build",
      success = readSuccess,
      peakRssMb = noLong,
      cmd = noStr,
      originId = readOriginId
    )
    pairSpans(
      events.compileStart,
      events.compileEnd,
      startKey = o => (getStr(o, "project"), getStr(o, "workspace")),
      endKey = o => (getStr(o, "project"), getStr(o, "workspace")),
      toLabel = o => s"compile ${getStr(o, "project")}",
      kind = "compile",
      success = readSuccess,
      peakRssMb = noLong,
      cmd = noStr,
      originId = readOriginId
    )
    pairSpans(
      events.projectTestStart,
      events.projectTestEnd,
      startKey = o => (getStr(o, "project"), getStr(o, "workspace"), getStr(o, "origin_id")),
      endKey = o => (getStr(o, "project"), getStr(o, "workspace"), getStr(o, "origin_id")),
      toLabel = o => s"project_test ${getStr(o, "project")}",
      kind = "project_test",
      success = readSuccess,
      peakRssMb = noLong,
      cmd = noStr,
      originId = readOriginId
    )
    pairSpans(
      events.suiteStart,
      events.suiteEnd,
      startKey = o => (getStr(o, "project"), getStr(o, "suite")),
      endKey = o => (getStr(o, "project"), getStr(o, "suite")),
      toLabel = o => s"suite ${getStr(o, "suite")}",
      kind = "suite",
      success = readSuccess,
      peakRssMb = noLong,
      cmd = noStr,
      originId = readOriginId
    )
    pairSpans(
      events.sourcegenStart,
      events.sourcegenEnd,
      startKey = o => getStr(o, "script"),
      endKey = o => getStr(o, "script"),
      toLabel = o => s"sourcegen ${getStr(o, "script")}",
      kind = "sourcegen",
      success = readSuccess,
      peakRssMb = noLong,
      cmd = noStr,
      originId = readOriginId
    )
    pairSpans(
      events.subprocessStart,
      events.subprocessEnd,
      startKey = o => o.get("pid").getAsLong,
      endKey = o => o.get("pid").getAsLong,
      toLabel = s => {
        val cmdStr = getStr(s, "cmd")
        val name = if (cmdStr.contains("/")) cmdStr.substring(cmdStr.lastIndexOf('/') + 1) else cmdStr
        s"${getStr(s, "kind")} ($name, pid=${s.get("pid").getAsLong})"
      },
      kind = "subprocess",
      success = noBool,
      peakRssMb = e => if (e.has("peak_rss_mb")) Some(e.get("peak_rss_mb").getAsLong) else None,
      cmd = s => Some(getStr(s, "cmd")),
      originId = readOriginId
    )

    val real = spans.toList.sortBy(_.startMs)
    // One synthetic root per scope, covering the union of its children's times — gives every scope a stable top-level entry in the resulting trace.
    val scopeRoots = real.groupBy(_.scope).toList.map { case (scope, ss) =>
      Span(
        scope,
        kind = "scope",
        label = scope,
        startMs = ss.map(_.startMs).min,
        endMs = ss.map(_.endMs).max,
        success = None,
        peakRssMb = None,
        cmd = None,
        originId = None
      )
    }
    val all0 = (scopeRoots ::: real).sortBy(_.startMs).toArray
    val parentIdx = Array.fill(all0.length)(-1)
    var i = 0
    while (i < all0.length) {
      val s = all0(i); var best = -1; var bestDur = Long.MaxValue; var j = 0
      while (j < all0.length) {
        if (j != i) {
          val p = all0(j)
          val contains = p.scope == s.scope && p.startMs <= s.startMs && s.endMs <= p.endMs && (p.endMs - p.startMs) > (s.endMs - s.startMs)
          val dur = p.endMs - p.startMs
          if (contains && dur < bestDur) { best = j; bestDur = dur }
        }
        j += 1
      }
      parentIdx(i) = best
      i += 1
    }
    // Propagate originId down the tree: any span without its own originId inherits from the nearest ancestor that has one. This is how compile/sourcegen/suite/
    // subprocess spans pick up the BSP-request originId without explicit plumbing through every emit site — the request_start span sits at the top of their
    // containment subtree, so once that one has originId set, descendants do too. After this pass, all spans in a request-rooted subtree share originId.
    val all = all0.clone()
    var k = 0
    while (k < all.length) {
      if (all(k).originId.isEmpty) {
        var anc = parentIdx(k)
        while (anc >= 0 && all(anc).originId.isEmpty) anc = parentIdx(anc)
        if (anc >= 0) all(k) = all(k).copy(originId = all(anc).originId)
      }
      k += 1
    }
    SpanGraph(all, parentIdx)
  }

  /** Write the merged span graph as a Chrome Trace Event Format JSON file. Drops into `chrome://tracing` and `ui.perfetto.dev` with zero install.
    *
    * pid assignment is **per trace** — i.e. per `originId` for BSP-request work, per scope otherwise. Every span belonging to one `bleep` command lands on the
    * same pid in the viewer, so the BSP request span, the compile/sourcegen children, the test-runner subprocesses, and the suite spans all stack into a single
    * nested tree on one lane. tid=0 within each pid, so Perfetto uses time-overlap to nest the spans. Phase `X` = complete event with `ts` (start, µs) and
    * `dur` (duration, µs).
    *
    * pid label uses the request label when available (e.g. `test [bleep-tests]`) so the viewer's process header reads as the command, not as a scope hash.
    */
  private def writeChromeTrace(graph: SpanGraph, path: Path): Unit = {
    def traceKey(s: Span): String = s.originId match { case Some(id) if id.nonEmpty => s"origin:$id"; case _ => s"scope:${s.scope}" }

    val tracesInOrder = graph.spans.toList.zipWithIndex.map { case (s, i) => (traceKey(s), i) }.distinctBy(_._1).map(_._1)
    val pidByTrace = tracesInOrder.zipWithIndex.toMap.view.mapValues(_ + 1).toMap

    // Label each pid with the most informative span in that trace: the `request` span when present, otherwise the scope. So in Perfetto you see
    // `test [bleep-tests] (request, e4102302…)` rather than a bare hash.
    val labelByTrace: Map[String, String] = tracesInOrder.map { tk =>
      val members = graph.spans.filter(traceKey(_) == tk)
      val req = members.find(_.kind == "request")
      val label = req match {
        case Some(r) => s"${r.label} (request${r.originId.fold("")(id => s", ${id.take(8)}…")})"
        case None    => members.headOption.map(_.scope).getOrElse(tk)
      }
      tk -> label
    }.toMap

    val events = ArrayBuffer.empty[String]
    tracesInOrder.foreach { tk =>
      events += s"""{"name":"process_name","ph":"M","pid":${pidByTrace(tk)},"tid":0,"args":{"name":"${escJson(labelByTrace(tk))}"}}"""
    }
    graph.spans.foreach { s =>
      val pid = pidByTrace(traceKey(s))
      val startUs = s.startMs * 1000
      val durUs = math.max(0L, (s.endMs - s.startMs) * 1000)
      val args = scala.collection.mutable.ArrayBuffer.empty[String]
      args += s""""kind":"${s.kind}""""
      args += s""""scope":"${escJson(s.scope)}""""
      s.originId.foreach(id => args += s""""origin_id":"${escJson(id)}"""")
      s.success.foreach(b => args += s""""success":${if (b) "true" else "false"}""")
      s.peakRssMb.foreach(m => args += s""""peak_rss_mb":$m""")
      s.cmd.foreach(c => args += s""""cmd":"${escJson(c)}"""")
      val argsJson = s""","args":{${args.mkString(",")}}"""
      events += s"""{"name":"${escJson(s.label)}","cat":"${s.kind}","ph":"X","ts":$startUs,"dur":$durUs,"pid":$pid,"tid":0$argsJson}"""
    }
    val json = s"""{"traceEvents":[${events.mkString(",")}],"displayTimeUnit":"ms"}"""
    Option(path.getParent).foreach(Files.createDirectories(_))
    Files.writeString(path, json): Unit
  }

  /** Write the merged span graph as OTLP/JSON. One `resourceSpans` group per scope (so each shows up as a distinct service in OTel viewers). `traceId` is a
    * stable 16-byte hash of the scope name (hex-encoded, 32 chars); `spanId` is a sequential 8-byte id within the trace (hex, 16 chars). `parentSpanId` is
    * looked up from the inferred-parent index when set. Time fields are nanoseconds, encoded as **strings** because JSON numbers don't preserve precision.
    */
  private def writeOtlpJson(graph: SpanGraph, path: Path): Unit = {
    // Trace grouping: when a span has `originId` (BSP request id propagated from the CLI), use that as the trace key so every span in one bleep command shares
    // a `traceId` and renders as one cohesive trace in Jaeger/Tempo/etc. Spans without originId (CLI process events, idle BSP daemon work outside a request)
    // fall back to grouping by scope. The resulting `traceId` is a stable hex 16-byte hash of the key.
    def traceIdOf(key: String): String = {
      val digest = java.security.MessageDigest.getInstance("SHA-256").digest(key.getBytes(java.nio.charset.StandardCharsets.UTF_8))
      digest.take(16).map(b => f"$b%02x").mkString
    }
    def spanIdOf(n: Long): String = f"$n%016x"
    def traceKey(s: Span): String = s.originId match { case Some(id) if id.nonEmpty => s"origin:$id"; case _ => s"scope:${s.scope}" }

    // SpanId is per-trace; counter resets per traceKey so the parentSpanId lookups stay within the right trace.
    val spanIdByIdx = Array.fill(graph.spans.length)("")
    val counter = scala.collection.mutable.Map.empty[String, Long]
    var i = 0
    while (i < graph.spans.length) {
      val tk = traceKey(graph.spans(i))
      val n = counter.getOrElse(tk, 0L) + 1
      counter(tk) = n
      spanIdByIdx(i) = spanIdOf(n)
      i += 1
    }

    // One resourceSpans block per trace (= per bleep request, or per scope for un-attributed spans). service.name = "bleep" always; service.instance.id is the
    // scope (BSP daemon hash / CLI invocation dir) so multi-tracker views in Jaeger group by it.
    val resourceBlocks = graph.spans.indices.groupBy(idx => traceKey(graph.spans(idx))).toList.map { case (tk, idxs) =>
      val firstScope = graph.spans(idxs.head).scope
      val resourceAttrs =
        s"""{"key":"service.name","value":{"stringValue":"bleep"}},{"key":"service.instance.id","value":{"stringValue":"${escJson(firstScope)}"}}"""
      val tid = traceIdOf(tk)
      val spans = idxs.toList.sortBy(graph.spans(_).startMs).map { idx =>
        val s = graph.spans(idx)
        val startNs = s.startMs * 1000000L
        val endNs = s.endMs * 1000000L
        // Only emit parentSpanId when the parent lives in the same trace — otherwise span ids from a different trace would dangle.
        val parentField = {
          val pi = graph.parentIdx(idx)
          if (pi >= 0 && traceKey(graph.spans(pi)) == tk) s""","parentSpanId":"${spanIdByIdx(pi)}"""" else ""
        }
        val attrs = scala.collection.mutable.ArrayBuffer.empty[String]
        attrs += s"""{"key":"kind","value":{"stringValue":"${s.kind}"}}"""
        attrs += s"""{"key":"scope","value":{"stringValue":"${escJson(s.scope)}"}}"""
        s.originId.foreach(id => attrs += s"""{"key":"origin_id","value":{"stringValue":"${escJson(id)}"}}""")
        s.peakRssMb.foreach(m => attrs += s"""{"key":"peak_rss_mb","value":{"intValue":"$m"}}""")
        s.cmd.foreach(c => attrs += s"""{"key":"cmd","value":{"stringValue":"${escJson(c)}"}}""")
        val statusCode = s.success match { case Some(true) => 1; case Some(false) => 2; case None => 0 }
        s"""{"traceId":"$tid","spanId":"${spanIdByIdx(idx)}"$parentField,"name":"${escJson(
            s.label
          )}","kind":1,"startTimeUnixNano":"$startNs","endTimeUnixNano":"$endNs","attributes":[${attrs.mkString(",")}],"status":{"code":$statusCode}}"""
      }
      s"""{"resource":{"attributes":[$resourceAttrs]},"scopeSpans":[{"scope":{"name":"bleep"},"spans":[${spans.mkString(",")}]}]}"""
    }
    val json = s"""{"resourceSpans":[${resourceBlocks.mkString(",")}]}"""
    Option(path.getParent).foreach(Files.createDirectories(_))
    Files.writeString(path, json): Unit
  }

  // ---- HTML generation ----

  private def generateHtml(events: Events, traceChromePath: Option[Path], traceOtlpPath: Option[Path]): String = {
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
    events.suiteStart.foreach(collectTs)
    events.suiteEnd.foreach(collectTs)
    events.testEnd.foreach(collectTs)

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
      // Add 95% threshold line
      val heapMaxVal = events.jvm.head.get("heap_max_mb").getAsLong
      val threshold95 = (heapMaxVal * 0.95).toLong
      t += s"""{"type":"scatter","mode":"lines","x":$xArr,"y":${fmtLongs(
          Seq.fill(events.jvm.size)(threshold95)
        )},"name":"95% threshold","line":{"color":"#ef4444","dash":"dash","width":1},"showlegend":false}"""
      addChart("heap", "Heap Memory (MB)", t, baseLayout("Time (s)", "MB"), false, 280)
    }

    // ---- 2. GC Activity ----
    // Show per-interval GC pause count (actual STW events) and cumulative pause time.
    // For ZGC pauses are sub-ms; for G1/Parallel they're the main overhead signal.
    if (events.jvm.size >= 2) {
      val t = ArrayBuffer.empty[String]
      val timestamps = events.jvm.map(_.get("ts").getAsLong)
      val cumulativePauseCount = events.jvm.map { e =>
        var total = 0L
        e.getAsJsonArray("gc").forEach { g =>
          val obj = g.getAsJsonObject
          if (obj.get("name").getAsString.toLowerCase(Locale.ROOT).contains("pause"))
            total += obj.get("count").getAsLong
        }
        total
      }
      val cumulativePauseMs = events.jvm.map { e =>
        var total = 0L
        e.getAsJsonArray("gc").forEach { g =>
          val obj = g.getAsJsonObject
          if (obj.get("name").getAsString.toLowerCase(Locale.ROOT).contains("pause"))
            total += obj.get("time_ms").getAsLong
        }
        total
      }
      // Deltas per interval
      val xs = ArrayBuffer.empty[Double]
      val pauseCounts = ArrayBuffer.empty[Long]
      val pauseMs = ArrayBuffer.empty[Long]
      var i = 1
      while (i < timestamps.length) {
        xs += relS(timestamps(i))
        pauseCounts += (cumulativePauseCount(i) - cumulativePauseCount(i - 1))
        pauseMs += (cumulativePauseMs(i) - cumulativePauseMs(i - 1))
        i += 1
      }
      val xArr = fmtDoubles(xs)
      t += s"""{"type":"bar","x":$xArr,"y":${fmtLongs(pauseCounts)},"name":"Pauses/interval","marker":{"color":"#8b5cf6","opacity":0.7},"yaxis":"y"}"""
      t += s"""{"type":"scatter","mode":"lines","x":$xArr,"y":${fmtLongs(
          pauseMs
        )},"name":"Pause ms/interval","line":{"color":"#ef4444","width":2},"yaxis":"y2"}"""
      val gcLayout =
        s"""{"margin":{"t":8,"r":60,"b":44,"l":60},"showlegend":true,"legend":{"orientation":"h","y":1.15,"x":0.5,"xanchor":"center","font":{"size":11}},"xaxis":{"title":{"text":"Time (s)","font":{"size":12}},"gridcolor":"#f0f0f0","zeroline":false},"yaxis":{"title":{"text":"Pause count","font":{"size":12}},"gridcolor":"#f0f0f0","zeroline":false},"yaxis2":{"title":{"text":"Pause ms","font":{"size":12}},"overlaying":"y","side":"right","gridcolor":"#f0f0f0","zeroline":false},"plot_bgcolor":"white","paper_bgcolor":"white","font":{"family":"Inter,system-ui,sans-serif","size":11,"color":"#374151"},"bargap":0.1}"""
      addChart("gc", "GC Pauses", t, gcLayout, false, 280)
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

    // ---- 6. Build Duration Over Time ----
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
          wsLabel,
          color,
          "solid",
          "none",
          "lines+markers"
        )
      }
      addChart("build-dur", "Build Duration Over Time", t, baseLayout("Time (s)", "Duration (s)"), false, 280)
    }

    // ---- 7. Compilation Timeline (full width) ----
    // Y-axis: (workspace, project) sorted by workspace then project name
    // X-axis: time. Each compile is a horizontal bar at its start time with width = duration.
    // Multiple bars per row when the same project was compiled multiple times.
    if (events.compileStart.nonEmpty && events.compileEnd.nonEmpty) {
      // Match starts to ends
      val startMap = scala.collection.mutable.Map.empty[(String, String), ArrayBuffer[Long]]
      events.compileStart.foreach { e =>
        val key = (getStr(e, "project"), getStr(e, "workspace"))
        startMap.getOrElseUpdate(key, ArrayBuffer.empty) += e.get("ts").getAsLong
      }

      case class Span(project: String, workspace: String, startS: Double, durationS: Double, success: Boolean)
      val spans = ArrayBuffer.empty[Span]
      events.compileEnd.foreach { e =>
        val key = (getStr(e, "project"), getStr(e, "workspace"))
        startMap.get(key).foreach { starts =>
          if (starts.nonEmpty) {
            val startTs = starts.remove(0)
            val durMs = e.get("duration_ms").getAsLong
            spans += Span(getStr(e, "project"), getStr(e, "workspace"), relS(startTs), durMs / 1000.0, e.get("success").getAsBoolean)
          }
        }
      }

      if (spans.nonEmpty) {
        // Build sorted list of (workspace, project) rows
        val rowKeys = spans.map(s => (pathName(s.workspace), s.project)).distinct.sortBy(r => (r._1, r._2))
        val rowLabels = rowKeys.map { case (ws, proj) => s"$ws / $proj" }
        val rowIndex = rowKeys.zipWithIndex.toMap

        val timelineHeight = math.max(400, rowKeys.size * 14 + 80)

        // Build one shape per span
        val shapes = spans.map { span =>
          val row = rowIndex((pathName(span.workspace), span.project))
          val color = if (span.success) "#22c55e" else "#ef4444"
          s"""{"type":"rect","x0":${span.startS},"x1":${span.startS + span.durationS},"y0":${row - 0.4},"y1":${row + 0.4},"fillcolor":"$color","opacity":0.8,"line":{"width":0}}"""
        }

        // Invisible scatter for hover info
        val t = ArrayBuffer.empty[String]
        t += s"""{"type":"scatter","mode":"markers","x":${fmtDoubles(spans.map(s => s.startS + s.durationS / 2))},"y":${fmtDoubles(
            spans.map(s => rowIndex((pathName(s.workspace), s.project)).toDouble)
          )},"text":${fmtStrings(spans.map(s => s"${s.project} (${pathName(s.workspace)})"))},"customdata":${fmtDoubles(
            spans.map(_.durationS)
          )},"marker":{"color":"rgba(0,0,0,0)","size":6},"hovertemplate":"%{text}<br>%{customdata:.1f}s<extra></extra>","showlegend":false}"""

        val tlLayout =
          s"""{"margin":{"t":8,"r":16,"b":44,"l":16},"showlegend":false,"xaxis":{"title":{"text":"Time (s)","font":{"size":12}},"gridcolor":"#f0f0f0","zeroline":false},"yaxis":{"automargin":true,"tickvals":${fmtDoubles(
              rowKeys.indices.map(_.toDouble)
            )},"ticktext":${fmtStrings(
              rowLabels
            )},"tickfont":{"size":9},"autorange":"reversed","gridcolor":"#f8f8f8"},"plot_bgcolor":"white","paper_bgcolor":"white","font":{"family":"Inter,system-ui,sans-serif","size":11,"color":"#374151"},"shapes":[${shapes
              .mkString(",")}]}"""

        chartCards += s"""<div class="bg-white rounded-xl shadow-sm border border-gray-100 p-5 lg:col-span-2"><h3 class="text-sm font-semibold text-gray-500 mb-3 uppercase tracking-wider">Compilation Timeline (${rowKeys.size} projects)</h3><div id="timeline" style="height:${timelineHeight}px"></div></div>"""
        plotCalls += s"""Plotly.newPlot('timeline',[${t.mkString(",")}],$tlLayout,{responsive:true,displayModeBar:false});"""
      }
    }

    // ---- Span Trace card (links to standard-format trace files) ----
    // We dropped the custom HTML span tree in favour of writing standard trace formats and pointing the user at off-the-shelf viewers. Chrome Trace Event
    // Format drops straight into ui.perfetto.dev with zero install; OTLP/JSON works with Jaeger / Grafana Tempo / otel-desktop-viewer / Honeycomb / DataDog.
    (traceChromePath, traceOtlpPath) match {
      case (None, None) => ()
      case _            =>
        val chromeLine = traceChromePath match {
          case Some(p) =>
            s"""<div style="margin-bottom:10px"><div style="font-size:12px;color:#475569;font-weight:600;margin-bottom:2px">Chrome Trace Event Format</div><div style="font-family:ui-monospace,SFMono-Regular,Menlo,monospace;font-size:11px;color:#1e3a8a;word-break:break-all">${escJson(
                p.toString
              )}</div><div style="font-size:11px;color:#64748b;margin-top:4px">Drop into <a href="https://ui.perfetto.dev" target="_blank" rel="noopener" style="color:#3b82f6;text-decoration:underline">ui.perfetto.dev</a> — no install. Or open <code>chrome://tracing</code>.</div></div>"""
          case None => ""
        }
        val otlpLine = traceOtlpPath match {
          case Some(p) =>
            s"""<div><div style="font-size:12px;color:#475569;font-weight:600;margin-bottom:2px">OpenTelemetry (OTLP/JSON)</div><div style="font-family:ui-monospace,SFMono-Regular,Menlo,monospace;font-size:11px;color:#1e3a8a;word-break:break-all">${escJson(
                p.toString
              )}</div><div style="font-size:11px;color:#64748b;margin-top:4px">Open with <a href="https://github.com/CtrlSpice/otel-desktop-viewer" target="_blank" rel="noopener" style="color:#3b82f6;text-decoration:underline">otel-desktop-viewer</a>, Jaeger, Grafana Tempo, Honeycomb, or DataDog.</div></div>"""
          case None => ""
        }
        chartCards += s"""<div class="bg-white rounded-xl shadow-sm border border-gray-100 p-5 lg:col-span-2"><h3 class="text-sm font-semibold text-gray-500 mb-3 uppercase tracking-wider">Span Trace</h3>$chromeLine$otlpLine</div>"""
    }

    // ---- Summary statistics ----
    val totalCompiles = events.compileEnd.size
    val successfulCompiles = events.compileEnd.count(_.get("success").getAsBoolean)
    val failedCompiles = totalCompiles - successfulCompiles
    val avgCompileMs = if (totalCompiles > 0) events.compileEnd.map(_.get("duration_ms").getAsLong).sum.toDouble / totalCompiles else 0.0
    val maxConcurrent = if (events.jvm.nonEmpty) events.jvm.map(_.get("concurrent_compiles").getAsInt).max else 0
    val maxHeap = if (events.jvm.nonEmpty) events.jvm.map(_.get("heap_used_mb").getAsLong).max else 0L
    val heapMax = if (events.jvm.nonEmpty) events.jvm.head.get("heap_max_mb").getAsLong else 0L
    val completedBuilds = events.buildEnd.size
    val avgBuildMs = if (completedBuilds > 0) events.buildEnd.map(_.get("duration_ms").getAsLong).sum.toDouble / completedBuilds else 0.0

    // OOM detection from server-side events
    val oomPressureCount = events.oomPressure.size
    val oomCrashCount = events.oomCrash.size
    val oomDetected = oomPressureCount > 0 || oomCrashCount > 0
    // Also detect from JVM samples as fallback (heap_used >= 95% of max)
    val oomSamplesFromJvm = if (events.jvm.nonEmpty && heapMax > 0) {
      events.jvm.count { e =>
        val used = e.get("heap_used_mb").getAsLong
        val max = e.get("heap_max_mb").getAsLong
        max > 0 && used.toDouble / max >= 0.95
      }
    } else 0
    val oomFromJvm = oomSamplesFromJvm > 0
    val anyOom = oomDetected || oomFromJvm
    val crashedBuilds = events.buildStart.size - events.buildEnd.size

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

    val oomWarning = if (anyOom) {
      val crashNote =
        if (oomCrashCount > 0) s" <strong>OutOfMemoryError recorded $oomCrashCount time(s).</strong>"
        else if (crashedBuilds > 0) s" Server crashed with $crashedBuilds build(s) in progress."
        else ""
      val pressureNote =
        if (oomPressureCount > 0) s" Server detected heap &ge;95% $oomPressureCount time(s)."
        else if (oomFromJvm) s" Heap was at &ge;95% of max for $oomSamplesFromJvm/${events.jvm.size} samples."
        else ""
      // Show timestamps of OOM events
      val oomTimes = (events.oomPressure.map(e => relS(e.get("ts").getAsLong)) ++ events.oomCrash.map(e => relS(e.get("ts").getAsLong))).sorted
      val timesNote = if (oomTimes.nonEmpty) s" OOM events at: ${oomTimes.map(t => f"${t}%.0fs").mkString(", ")}." else ""
      s"""<div class="bg-red-50 border border-red-200 rounded-xl p-4 mb-6">
<div class="flex items-start gap-3">
<div class="text-red-600 text-xl font-bold">!</div>
<div>
<div class="font-semibold text-red-800">Memory Pressure Detected</div>
<div class="text-sm text-red-700 mt-1">Heap max: $heapMax MB.$pressureNote$crashNote$timesNote Increase <code class="bg-red-100 px-1 rounded">-Xmx</code> or reduce concurrent workspaces.</div>
</div>
</div>
</div>"""
    } else ""

    val oomLabel = if (oomCrashCount > 0) s"$oomCrashCount CRASH" else if (oomPressureCount > 0) s"$oomPressureCount events" else "None"

    // Test-level summary
    val totalSuites = events.suiteEnd.size
    val failedSuites = events.suiteEnd.count(e => e.has("success") && !e.get("success").getAsBoolean)
    val totalTests = events.testEnd.size
    val testStatuses = events.testEnd.groupBy(e => getStr(e, "status").toLowerCase(Locale.ROOT)).view.mapValues(_.size).toMap
    val passedTests = testStatuses.getOrElse("passed", 0)
    val failedTests = testStatuses.getOrElse("failed", 0) + testStatuses.getOrElse("error", 0)
    val cancelledTests = testStatuses.getOrElse("canceled", 0) + testStatuses.getOrElse("skipped", 0) + testStatuses.getOrElse("ignored", 0)
    val avgSuiteMs = if (totalSuites > 0) events.suiteEnd.map(_.get("duration_ms").getAsLong).sum.toDouble / totalSuites else 0.0
    val avgTestMs = if (totalTests > 0) events.testEnd.map(_.get("duration_ms").getAsLong).sum.toDouble / totalTests else 0.0
    val slowestSuiteMs = if (totalSuites > 0) events.suiteEnd.map(_.get("duration_ms").getAsLong).max else 0L
    val slowestTestMs = if (totalTests > 0) events.testEnd.map(_.get("duration_ms").getAsLong).max else 0L

    val testStatsHtml =
      if (totalSuites == 0 && totalTests == 0) ""
      else s"""<div class="grid grid-cols-2 sm:grid-cols-4 gap-4 mb-6">
${stat("Suites", s"$totalSuites${if (failedSuites > 0) s" ($failedSuites failed)" else ""}", if (failedSuites > 0) "#ef4444" else "#22c55e")}
${stat("Tests", s"$totalTests passed:$passedTests failed:$failedTests skip:$cancelledTests", if (failedTests > 0) "#ef4444" else "#22c55e")}
${stat("Avg Suite", f"${avgSuiteMs / 1000.0}%.2f s", "#f59e0b")}
${stat("Avg Test", f"${avgTestMs}%.0f ms", "#f59e0b")}
${stat("Slowest Suite", f"${slowestSuiteMs / 1000.0}%.2f s", "#8b5cf6")}
${stat("Slowest Test", f"${slowestTestMs / 1000.0}%.2f s", "#8b5cf6")}
</div>"""

    val statsHtml = s"""$oomWarning<div class="grid grid-cols-2 sm:grid-cols-4 gap-4 mb-6">
${stat("Builds", s"$completedBuilds / ${events.buildStart.size}", if (crashedBuilds > 0) "#ef4444" else "#3b82f6")}
${stat("Compiles", totalCompiles.toString, "#6366f1")}
${stat("Success Rate", s"$successPct%", if (failedCompiles > 0) "#ef4444" else "#22c55e")}
${stat("Avg Compile", f"${avgCompileMs / 1000.0}%.1f s", "#f59e0b")}
${stat("Avg Build", f"${avgBuildMs / 1000.0}%.1f s", "#f59e0b")}
${stat("Max Concurrent", summaryMaxConcurrent.toString, "#8b5cf6")}
${stat("Heap", s"$summaryMaxHeap / $heapMax MB", if (anyOom) "#ef4444" else "#ec4899")}
${stat("OOM", oomLabel, if (anyOom) "#ef4444" else "#22c55e")}
</div>$testStatsHtml"""

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
