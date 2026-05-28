package bleep.scripts.dev

import bleep.{model, BleepScript, Commands, Started}
import bleep.symbols.{SourceFetcher, SymbolsBridge}

/** Stress-test the symbol-lookup library + its formatters against a realistic fat classpath (Spring Boot + companions, see `bench-bigdeps` in bleep.yaml).
  *
  * Run with `bleep cellar-benchmark`.
  *
  *   - One Context is built up-front from the bench project's classpath — every scenario reuses it.
  *   - For each scenario we measure wall-clock latency, print the first ~30 lines of output, and apply a heuristic pass/fail check ("does the answer contain
  *     this substring").
  *   - The final summary is the data we'd actually want for evaluating quality + speed.
  */
object SymbolBenchmark extends BleepScript("SymbolBenchmark") {

  /** Lookup scenario: (label, FQN, expected substring to find in `get` output). */
  private case class GetCase(label: String, fqn: String, expect: String)

  /** Scope-list scenario: (label, scope FQN, optional pattern, expected substring). */
  private case class FindScopeCase(label: String, scope: String, pattern: Option[String], expect: String)

  /** Global-search scenario: (label, pattern, expected substring). */
  private case class FindPatternCase(label: String, pattern: String, expect: String)

  /** Source scenario: (label, FQN, optional coordinate hint, expected substring). */
  private case class SourceCase(label: String, fqn: String, coordinate: Option[String], expect: String)

  private val getCases: List[GetCase] = List(
    GetCase("Spring entry point", "org.springframework.boot.SpringApplication", "class SpringApplication"),
    GetCase("Spring annotation", "org.springframework.web.bind.annotation.RestController", "RestController"),
    GetCase("Spring Data JPA interface", "org.springframework.data.jpa.repository.JpaRepository", "JpaRepository"),
    GetCase("Jackson ObjectMapper", "com.fasterxml.jackson.databind.ObjectMapper", "ObjectMapper"),
    GetCase("Reactor Flux", "reactor.core.publisher.Flux", "Flux"),
    GetCase("Guava ImmutableList", "com.google.common.collect.ImmutableList", "ImmutableList"),
    GetCase("Netty ChannelHandler", "io.netty.channel.ChannelHandler", "ChannelHandler"),
    GetCase("Hibernate Session", "org.hibernate.Session", "Session"),
    GetCase("Spring Security UserDetails", "org.springframework.security.core.userdetails.UserDetails", "UserDetails"),
    GetCase("Commons StringUtils", "org.apache.commons.lang3.StringUtils", "StringUtils")
  )

  private val findScopeCases: List[FindScopeCase] = List(
    FindScopeCase("Enumerate Spring boot package", "org.springframework.boot", Some("SpringApplication"), "SpringApplication"),
    FindScopeCase("ObjectMapper writeValue methods", "com.fasterxml.jackson.databind.ObjectMapper", Some("writeValue"), "writeValue"),
    FindScopeCase("Flux flatMap variants", "reactor.core.publisher.Flux", Some("flatMap"), "flatMap"),
    FindScopeCase("ImmutableList builder & companion", "com.google.common.collect.ImmutableList", Some("of"), "of"),
    FindScopeCase("JpaRepository ancestors + members", "org.springframework.data.jpa.repository.JpaRepository", None, "save")
  )

  private val findPatternCases: List[FindPatternCase] = List(
    FindPatternCase("Find RestController across classpath", "RestController", "RestController"),
    FindPatternCase("Find ObjectMapper across classpath", "ObjectMapper", "ObjectMapper"),
    FindPatternCase("Find flatMap (high-cardinality)", "flatMap", "flatMap"),
    FindPatternCase("Find @Transactional", "Transactional", "Transactional"),
    FindPatternCase("Find Authentication", "Authentication", "Authentication")
  )

  private val sourceCases: List[SourceCase] = List(
    SourceCase("SpringApplication source (auto-infer coord)", "org.springframework.boot.SpringApplication", None, "SpringApplication"),
    SourceCase("Flux source (explicit coord)", "reactor.core.publisher.Flux", Some("io.projectreactor:reactor-core:3.7.1"), "Flux")
  )

  override def run(started: Started, commands: Commands, args: List[String]): Unit = {
    val benchName = model.CrossProjectName(model.ProjectName("bench-bigdeps"), None)
    val project = started.resolvedProject(benchName)
    val jvm = started.resolvedJvm.forceGet

    println()
    println("=" * 80)
    println("SymbolBenchmark — bench-bigdeps")
    println("=" * 80)
    println(s"  classpath entries: ${project.classpath.size} jars + classes dirs")
    println(s"  JVM:               ${jvm.javaBin}")

    val ctxT0 = System.nanoTime()
    val jreCp = SymbolsBridge.jreClasspath(jvm)
    val projectCp = SymbolsBridge.projectClasspath(project)
    val ctx = tastyquery.Contexts.Context.initialize(jreCp ++ projectCp)
    val ctxMs = (System.nanoTime() - ctxT0) / 1_000_000L
    println(f"  Context init:      ${ctxMs}%6d ms (one-time)")
    println()

    given tastyquery.Contexts.Context = ctx

    val results = scala.collection.mutable.ArrayBuffer.empty[(String, String, Long, Boolean, Int)]

    section("get — full type info")
    getCases.foreach { c =>
      val (out, ms) = time(runGet(c.fqn))
      val ok = out.contains(c.expect)
      results += (("get", c.label, ms, ok, out.length))
      report(c.label, c.fqn, ms, ok, out)
    }

    section("find (scope) — enumerate package/class with optional pattern")
    findScopeCases.foreach { c =>
      val (out, ms) = time(runFindScope(c.scope, c.pattern))
      val ok = out.contains(c.expect)
      results += (("find/scope", c.label, ms, ok, out.length))
      val scopeDesc = c.pattern.fold(c.scope)(p => s"${c.scope}  pattern=$p")
      report(c.label, scopeDesc, ms, ok, out)
    }

    section("find (pattern) — substring across all classpath")
    findPatternCases.foreach { c =>
      val (out, ms) = time(runFindPattern(c.pattern, projectCp, jreCp))
      val ok = out.contains(c.expect)
      results += (("find/pattern", c.label, ms, ok, out.length))
      report(c.label, s"pattern=${c.pattern}", ms, ok, out)
    }

    section("get_source — fetch -sources.jar and slice")
    sourceCases.foreach { c =>
      val (out, ms) = time(runGetSource(c.fqn, c.coordinate, project, projectCp))
      val ok = out.contains(c.expect)
      results += (("get_source", c.label, ms, ok, out.length))
      val coordDesc = c.coordinate.fold("auto")(identity)
      report(c.label, s"${c.fqn}  ($coordDesc)", ms, ok, out)
    }

    println()
    println("=" * 80)
    println("SUMMARY")
    println("=" * 80)
    println(f"  ${"tool"}%-14s ${"scenario"}%-50s ${"ms"}%8s  ${"chars"}%8s  ok")
    results.foreach { case (tool, label, ms, ok, len) =>
      val mark = if (ok) "  PASS" else "  FAIL"
      println(f"  $tool%-14s $label%-50s $ms%8d  $len%8d  $mark")
    }
    val total = results.size
    val passed = results.count(_._4)
    val avgMs = if (results.isEmpty) 0L else results.map(_._3).sum / results.size
    println()
    println(f"  $passed / $total scenarios passed quality check")
    println(f"  avg per-scenario latency: $avgMs ms")
  }

  private def runGet(fqn: String)(using ctx: tastyquery.Contexts.Context): String =
    cellar.SymbolResolver.resolve(fqn).unsafeRunSync()(using cats.effect.unsafe.implicits.global) match {
      case cellar.LookupResult.Found(symbols)                  => cellar.GetFormatter.formatGetResult(fqn, symbols, None, Some(30), false, false)
      case cellar.LookupResult.IsPackage                       => s"'$fqn' is a package."
      case cellar.LookupResult.PartialMatch(resolved, missing) => s"PartialMatch: resolved=$resolved missing=$missing"
      case cellar.LookupResult.NotFound                        => s"NotFound: $fqn"
      case cellar.LookupResult.LookupFailed(cause)             => s"LookupFailed: ${cause.getMessage}"
    }

  private def runFindScope(scope: String, pattern: Option[String])(using ctx: tastyquery.Contexts.Context): String =
    cellar.SymbolLister.resolve(scope).unsafeRunSync()(using cats.effect.unsafe.implicits.global) match {
      case cellar.ListResolveResult.Found(target) =>
        val declared = cellar.SymbolLister.listMembers(target).toList
        val companion = target match {
          case cellar.ListTarget.Cls(cls) =>
            cellar.SymbolResolver.companionOrJavaStatics(cls) match {
              case Some(comp) if comp.isModuleClass =>
                cellar.SymbolLister.listMembers(cellar.ListTarget.Cls(comp)).toList
              case _ => Nil
            }
          case _ => Nil
        }
        val all = declared ++ companion
        val filtered = pattern.fold(all)(p => all.filter(_.name.toString.toLowerCase.contains(p.toLowerCase)))
        filtered.take(50).map(s => cellar.LineFormatter.formatLine(s)).mkString("\n")
      case other => s"resolve($scope) = $other"
    }

  private def runFindPattern(pattern: String, projectCp: tastyquery.Classpaths.Classpath, jreCp: tastyquery.Classpaths.Classpath)(using
      tastyquery.Contexts.Context
  ): String = {
    val lower = pattern.toLowerCase
    val matches = cellar.AllSymbolsStream
      .stream(projectCp, jreCp)
      .filter(_.name.toString.toLowerCase.contains(lower))
      .toList
    val sorted = matches.sortBy { sym =>
      val n = sym.name.toString
      val ln = n.toLowerCase
      (if (ln == lower) 0 else 1, if (ln.startsWith(lower)) 0 else 1, n.length, n)
    }
    val limited = sorted.take(50)
    if (limited.isEmpty) return ""

    // Mirror BleepMcpServer.findGlobal: group adjacent results from the same package.
    val groups = scala.collection.mutable.LinkedHashMap.empty[String, scala.collection.mutable.ListBuffer[tastyquery.Symbols.Symbol]]
    limited.foreach { sym =>
      val fqn = sym.displayFullName
      val idx = fqn.lastIndexOf('.')
      val pkg = if (idx < 0) "" else fqn.substring(0, idx)
      groups.getOrElseUpdate(pkg, scala.collection.mutable.ListBuffer.empty) += sym
    }
    val body = groups.iterator
      .map { case (pkg, syms) =>
        val header = if (pkg.isEmpty) "(top-level):" else s"$pkg:"
        val items = syms.iterator
          .map { sym =>
            val name = {
              val fqn = sym.displayFullName
              val idx = fqn.lastIndexOf('.')
              if (idx < 0) fqn else fqn.substring(idx + 1)
            }
            s"  $name — ${cellar.LineFormatter.formatLine(sym)}"
          }
          .mkString("\n")
        s"$header\n$items"
      }
      .mkString("\n\n")
    val hasScala2 = limited.exists(sym => cellar.TypePrinter.detectLanguage(sym) == cellar.DetectedLanguage.Scala2)
    val scala2Note = if (hasScala2) "// Note: results include Scala 2 artifacts; some signatures may be incomplete.\n\n" else ""
    scala2Note + body
  }

  private def runGetSource(fqn: String, coordinateHint: Option[String], project: bleep.ResolvedProject, classpath: tastyquery.Classpaths.Classpath)(using
      tastyquery.Contexts.Context
  ): String =
    cellar.SymbolResolver.resolve(fqn).unsafeRunSync()(using cats.effect.unsafe.implicits.global) match {
      case cellar.LookupResult.Found(symbols) =>
        sourceRef(symbols.head) match {
          case None      => s"No source position for $fqn"
          case Some(ref) =>
            val coord = coordinateHint.orElse(inferCoordinate(symbols.head, project, classpath)) match {
              case Some(c) => c
              case None    => return s"Could not infer coordinate for $fqn"
            }
            SourceFetcher.fetch(coord, ref.filePath, ref.startLine, ref.endLine) match {
              case Right(r) =>
                val info = if (ref.endLine == Int.MaxValue) "" else s" lines ${ref.startLine + 1}–${ref.endLine + 1}"
                s"// ${r.entryPath}$info  (from $coord)\n" + r.lines.take(30).mkString("\n")
              case Left(err) => err
            }
        }
      case other => s"resolve($fqn) = $other"
    }

  private case class SourceRef(filePath: String, startLine: Int, endLine: Int, language: String)
  private def sourceRef(sym: tastyquery.Symbols.Symbol): Option[SourceRef] =
    sym.tree
      .flatMap { t =>
        val pos = t.asInstanceOf[tastyquery.Trees.Tree].pos
        if (pos.isUnknown || pos.isSynthetic || pos.sourceFile == tastyquery.SourceFile.NoSource) None
        else Some(SourceRef(pos.sourceFile.path, pos.startLine, pos.endLine, "scala"))
      }
      .orElse {
        sym match {
          case s: tastyquery.Symbols.TermOrTypeSymbol if s.sourceLanguage == tastyquery.SourceLanguage.Java =>
            Some(SourceRef(javaSourcePath(s), 0, Int.MaxValue, "java"))
          case _ => None
        }
      }

  private def javaSourcePath(sym: tastyquery.Symbols.TermOrTypeSymbol): String = {
    def topLevel(s: tastyquery.Symbols.TermOrTypeSymbol): tastyquery.Symbols.TermOrTypeSymbol = s.owner match {
      case p: tastyquery.Symbols.TermOrTypeSymbol if !p.isPackage => topLevel(p)
      case _                                                      => s
    }
    topLevel(sym).displayFullName.replace('.', '/') + ".java"
  }

  private def inferCoordinate(sym: tastyquery.Symbols.Symbol, project: bleep.ResolvedProject, classpath: tastyquery.Classpaths.Classpath): Option[String] =
    topLevelClass(sym).flatMap { top =>
      val pkg = top.owner match {
        case p: tastyquery.Symbols.PackageSymbol => p.fullName.toString
        case _                                   => ""
      }
      val bin = top.name.toString
      val entry = classpath.find { e =>
        try e.listAllPackages().exists(p => p.dotSeparatedName == pkg && p.getClassDataByBinaryName(bin).isDefined)
        catch case _: Exception => false
      }
      entry.flatMap { e =>
        val ep = java.nio.file.Paths.get(e.toString)
        project.resolution.flatMap(
          _.modules.find(_.artifacts.exists(a => a.classifier.isEmpty && a.path == ep)).map(m => s"${m.organization}:${m.name}:${m.version}")
        )
      }
    }

  private def topLevelClass(sym: tastyquery.Symbols.Symbol): Option[tastyquery.Symbols.ClassSymbol] = {
    var current: tastyquery.Symbols.Symbol = sym
    var last: Option[tastyquery.Symbols.ClassSymbol] = sym match {
      case c: tastyquery.Symbols.ClassSymbol => Some(c)
      case _                                 => None
    }
    while (current.owner != null && !current.owner.isInstanceOf[tastyquery.Symbols.PackageSymbol]) {
      current = current.owner
      current match {
        case c: tastyquery.Symbols.ClassSymbol => last = Some(c)
        case _                                 => ()
      }
    }
    last
  }

  private def section(title: String): Unit = {
    println()
    println("-" * 80)
    println(title)
    println("-" * 80)
  }

  private def report(label: String, target: String, ms: Long, ok: Boolean, output: String): Unit = {
    val mark = if (ok) "PASS" else "FAIL"
    println()
    println(f"  [$mark]  ${label}%-50s  ${ms}%6d ms   ${target}")
    val preview = output.linesIterator.take(30).mkString("\n").trim
    if (preview.nonEmpty) {
      println("  ───")
      preview.linesIterator.foreach(l => println(s"    $l"))
      val remaining = output.linesIterator.size - 30
      if (remaining > 0) println(s"    ... (${remaining} more lines, ${output.length} chars total)")
    }
  }

  private def time[A](thunk: => A): (A, Long) = {
    val t0 = System.nanoTime()
    val r = thunk
    (r, (System.nanoTime() - t0) / 1_000_000L)
  }
}
