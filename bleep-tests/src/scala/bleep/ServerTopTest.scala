package bleep

import bleep.bsp.protocol._
import bleep.bsp.{ServerDirInfo, ServerState}
import bleep.commands.server.tui.{ServerRow, ServerTopState, ServerTopUpdate, ServerTopView}
import jatatui.core.backend.TestBackend
import jatatui.react.TestHarness
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.Path

/** The dashboard, rendered into an off-screen buffer and read back as text.
  *
  * No terminal, no JNI, no sockets, no sleeping: the view is a pure function of state and the state is a value a test can build. Everything that is not those
  * two things — the clock, the poll, the keyboard — lives in `ServerTopLoop` precisely so it stays out of here.
  */
class ServerTopTest extends AnyFunSuite with Matchers {
  import ServerTopState._

  private val NowMs = 1_700_000_600_000L

  private def jvm: JvmStats = JvmStats(
    heapUsedMb = 512,
    heapCommittedMb = 1024,
    heapMaxMb = 12288,
    heapLiveMb = 128,
    nonHeapUsedMb = 64,
    gc = List(GcStat("ZGC Major Cycles", 3, 42)),
    threads = 70,
    peakThreads = 73,
    daemonThreads = 68,
    cpuProcess = 0.05,
    cpuSystem = 0.2,
    loadedClasses = 20000,
    openFileDescriptors = Some(383L)
  )

  /** A cooperative scheduler on a 48 GB machine with 8 GB headroom: a 40 GB ceiling, 12 GB in use, nothing running. */
  private val idleScheduler: SchedulerDto = SchedulerDto(
    mode = SchedulerDto.Cooperative,
    unconstrainedReason = None,
    parallelism = 18,
    headroomMb = 8192L,
    machine = Some(MachineViewDto(physicalMb = 49152L, usedMb = 12288L, pressure = "normal", pressureReason = None, sampledAgoMs = 500L)),
    lock = LockDto(state = "held", holderPid = None, holderStartedAtEpochMs = None, holderHeldForMs = None),
    liveServers = 2,
    requests = 0,
    cpuInUse = 0,
    wantsMore = false,
    shuttingDown = false,
    inHeap = Nil,
    forks = Nil,
    waiting = Nil
  )

  private def compile(project: String, cpu: Int): InHeapTaskDto = InHeapTaskDto(request = "op-1", taskId = s"compile:$project", kind = "compile", cpu = cpu)

  /** A test fork of the pool's key, 2560 MB bound. */
  private def fork(id: Long, pid: Option[Long], measuredMb: Option[Long], busyCpu: Int): SchedulerForkDto = SchedulerForkDto(
    id = id,
    pid = pid,
    request = "op-1",
    kind = "test-suite",
    key = "jvm ce91585a08d64aec:shared",
    boundMb = 2560L,
    measuredMb = measuredMb,
    shared = true,
    busyCpu = busyCpu,
    evicting = false,
    ageMs = 99_000L
  )

  private def waitingCompile(project: String, cpu: Int): DemandDto =
    DemandDto(request = "op-1", taskId = s"compile:$project", kind = "compile", cpu = cpu, boundMb = None)

  /** The idle scheduler with this work on it; cpu in use follows from the work, as it does in the scheduler. */
  private def working(inHeap: List[InHeapTaskDto], forks: List[SchedulerForkDto]): SchedulerDto =
    idleScheduler.copy(inHeap = inHeap, forks = forks, cpuInUse = inHeap.map(_.cpu).sum + forks.map(_.busyCpu).sum, requests = 1)

  private def status(workspaces: List[WorkspaceDto], scheduler: SchedulerDto): DaemonStatus = DaemonStatus(
    adminProtocolVersion = BleepServerAdmin.ProtocolVersion,
    bleepVersion = "1.0.0-M11",
    pid = 4242L,
    startedAtEpochMs = NowMs - 600_000L, // ten minutes of uptime
    socketDir = "/tmp/sockets/aaaa1111",
    jvm = jvm,
    scheduler = scheduler,
    connections = List(ConnectionDto(1, NowMs, observer = false, Some("Metals"), Some("1.0"), Some("/home/dev/project"))),
    workspaces = workspaces,
    buildCache = BuildCacheDto(cachedWorkspaces = workspaces.map(_.path), bound = 12),
    analysisCache = AnalysisCacheDto(entries = 40, fileBytes = 5L * 1024 * 1024, internedClasses = 10, sharedAnalyses = 2, contentHits = 7, perWorkspace = Nil),
    config = ServerConfigDto(
      parallelism = 18,
      compileServerMaxMemory = Some("12g"),
      testRunnerHeap = None,
      maxCachedWorkspaces = 12,
      bspReadTimeoutMillis = 30 * 60000L,
      compileServerIdleTimeoutMillis = 60 * 60000L,
      testIdleTimeoutMinutes = 2,
      heapPressureThreshold = 0.8,
      machineScheduling = Some("cooperative")
    ),
    idleMs = Some(0L)
  )

  /** A daemon with two forked test JVMs: 2 GB itself, 6 GB with everything beneath it. */
  private val processTree: List[ProcessTree.Sample] = List(
    ProcessTree.Sample(4242L, None, "compile server", Some(2048L), Some(10_000L), Some(NowMs - 600_000L)),
    ProcessTree.Sample(5001L, Some(4242L), "test JVM", Some(3072L), Some(5_000L), Some(NowMs - 43_000L)),
    ProcessTree.Sample(5002L, Some(4242L), "test JVM", Some(1024L), Some(1_000L), Some(NowMs - 42_000L))
  )

  /** A 48 GB, 18-core machine — a round number to put bleep's use in proportion to. */
  private val TestMachine = ServerTopState.Machine(physicalMemoryMb = 49152L, cores = 18)

  private def info(hash: String, state: ServerState): ServerDirInfo =
    ServerDirInfo(Path.of("/tmp/sockets").resolve(hash), hash, state, Some(4242L), None, 0L)

  private def running(hash: String, isCurrent: Boolean, workspaces: List[WorkspaceDto] = Nil, scheduler: SchedulerDto = idleScheduler): ServerRow =
    ServerRow(
      info(hash, ServerState.Running),
      Some(status(workspaces, scheduler)),
      None,
      isCurrent,
      Some(processTree),
      parent = None,
      isOutdated = false,
      published = None
    )

  private def dead(hash: String): ServerRow =
    ServerRow(
      info(hash, ServerState.Dead(crashed = false)),
      None,
      None,
      isCurrent = false,
      processes = None,
      parent = None,
      isOutdated = false,
      published = None
    )

  private def withScheduler(row: ServerRow, f: SchedulerDto => SchedulerDto): ServerRow =
    row.copy(status = row.status.map(s => s.copy(scheduler = f(s.scheduler))))

  private def stateWith(rows: List[ServerRow]): ServerTopState =
    ServerTopState.initial(NowMs, TestMachine).copy(rows = rows)

  /** Join classpath entries the way the daemon's own launch command does, in `BspServerOperations`.
    *
    * Hardcoding `:` here passes on unix and quietly lies on Windows, where the separator is `;`: the fixture becomes one enormous single path, the pane
    * faithfully reports "1 entries", and the failure looks like a rendering bug rather than a test that built the wrong input.
    */
  private def classpathArg(entries: Seq[String]): String = entries.mkString(java.io.File.pathSeparator)

  /** Render at a fixed size and read the buffer back as plain text. */
  private def draw(state: ServerTopState): String = drawAt(state, width = 140)

  private def drawAt(state: ServerTopState, width: Int): String = drawAt(state, width, height = 30)

  private def drawAt(state: ServerTopState, width: Int, height: Int): String = {
    val harness = new TestHarness(width, height)
    harness.render(ServerTopView.render(state, _ => ()))
    TestBackend.bufferView(harness.backend.buffer())
  }

  /** A row of the server list — a line inside a box with a server marker on it — naming `name`. Not the summary or the process tree, which may name it too. */
  private def listRow(screen: String, name: String): String =
    screen.linesIterator
      .find(line => line.contains("│") && (line.contains("●") || line.contains("!")) && line.contains(name))
      .getOrElse(fail(s"no list row for $name"))

  /** The Overview tab, on a screen tall enough to hold it under the summary and the server list. */
  private def drawOverview(state: ServerTopState): String = drawAt(state.copy(tab = Tab.Overview), width = 140, height = 50)

  /** Every message the screen would dispatch, by clicking each cell of a column band. Scanning rather than hard-coding coordinates keeps these tests about
    * "this is clickable" instead of about the current line spacing — the first version broke the moment the layout gained a blank line.
    */
  private def clicksAnywhere(state: ServerTopState): List[Msg] =
    (0 until 30).flatMap(y => (0 until 100).flatMap(x => clickAt(state, x, y))).toList

  /** Render, click a cell, and report what the view dispatched. Covers the click targets without a terminal or a mouse. */
  private def clickAt(state: ServerTopState, x: Int, y: Int): List[Msg] = {
    val dispatched = scala.collection.mutable.ListBuffer.empty[Msg]
    val harness = new TestHarness(100, 30)
    harness.render(ServerTopView.render(state, msg => dispatched.append(msg): Unit))
    harness.renderer.dispatchMouse(new jatatui.react.MouseEvent(x, y, new tui.crossterm.KeyModifiers(0), jatatui.react.MouseEvent.Kind.DOWN)): Unit
    dispatched.toList
  }

  /** Every cell, including the ones no text lands on. The palette is built for a dark background; without one painted, text rendered on a terminal that
    * supplies its own light background is close to unreadable — which is exactly what shipped before this test existed.
    */
  test("the whole screen is painted with the palette background, not just the cells with text on them") {
    val harness = new TestHarness(100, 30)
    harness.render(ServerTopView.render(stateWith(List(running("aaaa1111", isCurrent = true))), _ => ()))
    val buffer = harness.backend.buffer()

    val corners = List((0, 0), (99, 0), (0, 29), (99, 29))
    corners.foreach { case (x, y) =>
      withClue(s"cell ($x,$y) should carry the palette background: ") {
        buffer.cellAt(x, y).style().bg().orElse(null) shouldBe bleep.testing.FancyBuildDisplay.Palette.bg
      }
    }
  }

  test("box titles keep their spaces") {
    draw(stateWith(List(running("aaaa1111", isCurrent = true)))) should include(" servers ")
  }

  /** Four servers on a machine looks like a mistake until you can see that they differ in bleep version or JVM — which is exactly what puts them in different
    * socket directories in the first place. The hash alone leaves that unanswerable without digging.
    */
  test("each row shows the things that make it a separate server") {
    def withIdentity(hash: String, version: String, jvmVersion: String) = {
      val base = running(hash, isCurrent = false)
      // As the JvmKey actually records it: the name carries the version, and jvmVersion is the JVM index.
      val identity = bleep.bsp.ServerJson(
        bleepVersion = version,
        jvmName = s"graalvm-community:$jvmVersion",
        jvmVersion = "default",
        javaBin = "/opt/jvm/bin/java",
        javaOpts = Nil,
        serverMainClass = "x",
        command = Nil,
        workingDir = "/tmp",
        spawnedAtEpochMs = 1L
      )
      base.copy(info = base.info.copy(identity = Some(identity)))
    }

    val screen = draw(stateWith(List(withIdentity("aaaa1111", "1.0.0-M11", "25.0.1"), withIdentity("bbbb2222", "1.0.0-M10", "24.0.1"))))

    withClue("the shared 1.0.0- prefix is dropped; what is left is what differs: ") {
      screen should include("M11")
      screen should include("M10")
    }
    withClue("the index is `default` on every row here and only costs width: ") {
      screen should include("graalvm:25.0.1")
      screen should include("graalvm:24.0.1")
      screen should not include "default"
    }
  }

  /** The row carries more columns than a narrow terminal can hold, and something has to be cut. It must never be the marker saying which server is yours,
    * because that is the one a reader acts on.
    */
  test("on a narrow terminal the this-build marker survives, whatever else is cut") {
    val screen = drawAt(stateWith(List(running("aaaa1111", isCurrent = true), running("bbbb2222", isCurrent = false))), width = 72)

    screen should include("← this build")
    screen should include("pid 4242")
  }

  test("a server with nothing recorded says so in the version column rather than showing a blank") {
    draw(stateWith(List(running("aaaa1111", isCurrent = true)))) should include("unknown")
  }

  /** A hundred stopped servers used to fill the list and push the running ones off the screen. They hold disk and nothing else, so they are a count on the main
    * screen and a list of their own behind it.
    */
  test("the main view shows live servers only; stopped ones are a count that opens their own screen") {
    val state = stateWith(List(running("aaaa1111", isCurrent = true), dead("bbbb2222")))
    val screen = draw(state)

    screen should include("pid 4242")
    screen should include("← this build")
    screen should include("10m0s")
    screen should not include "bbbb2222"
    screen should include("1 stopped")

    val deadScreen = draw(press(state, KeyPress.ShowDead))
    deadScreen should include("bbbb2222")
    deadScreen should include("dead")
    deadScreen should not include "aaaa1111"
  }

  test("the processes tab shows the selected server's work, then the daemon and the JVMs it forked as a tree") {
    val busyFork = fork(id = 3L, pid = Some(5001L), measuredMb = Some(3072L), busyCpu = 1)
    val row = running("aaaa1111", isCurrent = true, scheduler = working(Nil, List(busyFork)).copy(waiting = List(waitingCompile("bleep-core", cpu = 2))))
    val screen = drawAt(stateWith(List(row)), width = 140, height = 40)

    screen should include("▸ test suite  on fork #3 (pid 5001)  1 slot")
    screen should include("· compile     compile:bleep-core  waiting for 2 slots")
    screen should include("compile server  pid 4242  heap 512 MB/12.0 GB  self 2.0 GB")
    screen should include("├─ test JVM  pid 5001")
    screen should include("└─ test JVM  pid 5002")
  }

  test("memory is transitive: a server and its daemon are charged for everything beneath them") {
    val screen = drawAt(stateWith(List(running("aaaa1111", isCurrent = true))), width = 140, height = 40)
    val serverLine = listRow(screen, "pid 4242")
    val daemonLine = screen.linesIterator.find(_.contains("compile server  pid 4242")).get
    val forkLine = screen.linesIterator.find(_.contains("pid 5001")).get

    serverLine should include("2.0 GB +4.0 GB forked")
    daemonLine should include("6.0 GB")
    forkLine should include("3.0 GB")
  }

  /** The point of the whole screen: how much of the machine bleep has, in proportion to what the machine has, before any detail. */
  test("the summary puts bleep's memory and cpu in proportion to the machine") {
    val screen = draw(stateWith(List(running("aaaa1111", isCurrent = true), running("bbbb2222", isCurrent = false))))
    screen should include("12.0 GB of 48.0 GB — 25% of this machine")
    screen should include("of 18 cores")
  }

  /** The servers eating a machine are the ones nobody looks at: an older bleep's, kept in use by a client started long ago and pinned to that version. When
    * they hold a real share, saying so — and naming the client — is the one line that tells you what to do.
    */
  test("when other bleep versions hold the memory, the summary says so and names what keeps them alive") {
    val keeper = ProcessTree.Parent(84842L, "bleep mcp-server", Some(NowMs - 8L * 24 * 3600 * 1000))
    val old = running("dddd4444", isCurrent = false).copy(isOutdated = true, parent = Some(keeper))
    val mine = {
      val r = running("aaaa1111", isCurrent = true)
      r.copy(processes = Some(List(ProcessTree.Sample(1L, None, "compile server", Some(512L), None, None))))
    }
    val screen = draw(stateWith(List(mine, old)))
    screen should include("6.0 GB of it is 1 server from other bleep versions than this one, kept alive by bleep mcp-server (pid 84842, up 8d0h)")
  }

  test("every server in the list says how long it has been up, whatever its state") {
    val old = running("dddd4444", isCurrent = false).copy(
      processes = Some(List(ProcessTree.Sample(1L, None, "compile server", Some(100L), None, Some(NowMs - 3L * 24 * 3600 * 1000))))
    )
    val wedged = ServerRow(
      info("cccc3333", ServerState.Wedged),
      None,
      None,
      isCurrent = false,
      processes = Some(List(ProcessTree.Sample(2L, None, "compile server", Some(100L), None, Some(NowMs - 90_000L)))),
      parent = None,
      isOutdated = false,
      published = None
    )
    val screen = draw(stateWith(List(running("aaaa1111", isCurrent = true), old, wedged)))
    listRow(screen, "pid 4242") should include("up 10m0s")
    listRow(screen, "pid 1 ") should include("up 3d0h")
    listRow(screen, "pid 2 ") should include("up 1m30s")
  }

  /** "6 GB" does not say whether a server is fat itself or carrying a fleet of test JVMs, and the two call for different fixes. */
  test("each server's memory is its own, plus what its forks add") {
    val screen = draw(stateWith(List(running("aaaa1111", isCurrent = true))))
    listRow(screen, "pid 4242") should include("2.0 GB +4.0 GB forked")

    val alone = running("bbbb2222", isCurrent = true).copy(processes = Some(List(ProcessTree.Sample(1L, None, "compile server", Some(512L), None, None))))
    val aloneRow = listRow(draw(stateWith(List(alone))), "pid 1 ")
    aloneRow should include("512 MB")
    aloneRow should not include "forked"
  }

  /** Deterministic, so lines do not trade places as memory moves; newest first, so the server you just caused is on top and the long-lived ones sink. */
  test("servers are listed newest first, by pid, however they arrive and whatever they cost") {
    def started(hash: String, pid: Long, agoMs: Long, memoryMb: Long) =
      running(hash, isCurrent = false).copy(
        processes = Some(List(ProcessTree.Sample(pid, None, "compile server", Some(memoryMb), None, Some(NowMs - agoMs))))
      )
    val old = started("aaaa1111", 22574L, agoMs = 9L * 24 * 3600 * 1000, memoryMb = 14000L)
    val fresh = started("bbbb2222", 8368L, agoMs = 60_000L, memoryMb = 300L)
    val middle = started("cccc3333", 83374L, agoMs = 3600_000L, memoryMb = 2000L)

    val state = ServerTopUpdate.update(stateWith(Nil), Msg.Refreshed(List(old, fresh, middle), NowMs))._1
    state.live.map(_.hash) shouldBe List("bbbb2222", "cccc3333", "aaaa1111")

    val rows = draw(state).linesIterator.filter(line => line.contains("│") && line.contains("●")).toList
    rows.map(row => List("pid 8368 ", "pid 83374", "pid 22574").find(row.contains).getOrElse(row)) shouldBe List("pid 8368 ", "pid 83374", "pid 22574")
    withClue("the arrows walk the list in the order it is drawn: ") {
      press(state, KeyPress.Down).selectedRow.map(_.hash) shouldBe Some("cccc3333")
    }
  }

  test("the processes tab names who started the server") {
    val keeper = ProcessTree.Parent(84842L, "bleep mcp-server", Some(NowMs - 3600_000L))
    val row = running("aaaa1111", isCurrent = true).copy(parent = Some(keeper))
    drawAt(stateWith(List(row)), width = 140, height = 40) should include("started by bleep mcp-server (pid 84842, running 1h0m)")
  }

  test("a server with no process to measure says so instead of drawing an empty tree") {
    val gone = running("aaaa1111", isCurrent = true).copy(processes = None)
    drawAt(stateWith(List(gone)), width = 140, height = 40) should include("no process to measure")
  }

  test("a wedged server stays on the main view — it is alive and holding memory — and can be killed but not restarted") {
    val wedged =
      ServerRow(info("cccc3333", ServerState.Wedged), None, None, isCurrent = false, processes = None, parent = None, isOutdated = false, published = None)
    val state = stateWith(List(wedged))

    draw(state) should include("wedged")
    press(state, KeyPress.Kill).pending.map(_.prompt) shouldBe Some("kill pid 4242 (cccc3333)? (y/n)")
    press(state, KeyPress.Restart).pending shouldBe None
  }

  test("cpu is a rate between two readings, per process, summed up the tree") {
    def at(cpuMs: Long) = {
      val row = running("aaaa1111", isCurrent = true)
      row.copy(processes = Some(List(ProcessTree.Sample(4242L, None, "compile server", Some(100L), Some(cpuMs), None))))
    }
    val first = ServerTopUpdate.update(ServerTopState.initial(NowMs, TestMachine), Msg.Refreshed(List(at(10_000L)), NowMs))._1
    withClue("one reading cannot say how busy anything is: ") {
      first.cpuPercent shouldBe empty
    }
    // 1.5s of CPU in 1s of wall time is one and a half cores.
    val second = ServerTopUpdate.update(first, Msg.Refreshed(List(at(11_500L)), NowMs + 1000))._1
    second.cpuPercent(4242L) shouldBe 150.0 +- 0.001
    draw(second) should include("150%")
  }

  test("clearing stopped servers asks first, then prunes") {
    val state = press(stateWith(List(running("aaaa1111", isCurrent = true), dead("bbbb2222"), dead("dddd4444"))), KeyPress.ShowDead)
    val asked = press(state, KeyPress.PruneDead)
    asked.pending.map(_.prompt) shouldBe Some("delete 2 stopped server directories, 0 MB of logs and metrics? (y/n)")

    val (_, effects) = ServerTopUpdate.update(asked, Msg.Key(KeyPress.Yes))
    effects shouldBe List(Effect.PruneDead)
  }

  test("esc leaves the stopped-servers screen rather than the program") {
    val state = press(stateWith(List(dead("bbbb2222"))), KeyPress.ShowDead)
    val back = press(state, KeyPress.Back)
    back.screen shouldBe Screen.Main
    back.quit shouldBe false
    press(back, KeyPress.Back).quit shouldBe true
  }

  test("the overview keeps the live set distinct from heap used, which is the number that says retaining vs churning") {
    val screen = drawOverview(stateWith(List(running("aaaa1111", isCurrent = true))))

    screen should include("Retained")
    screen should include("128 MB still held")
    screen should include("383 file descriptors")
    screen should include("Major Cycles")
  }

  test("an unmeasurable live set renders n/a rather than a zero that looks like a measurement") {
    val row = running("aaaa1111", isCurrent = true)
    val unsupported = row.copy(status = row.status.map(s => s.copy(jvm = s.jvm.copy(heapLiveMb = -1L, openFileDescriptors = None))))

    val screen = drawOverview(stateWith(List(unsupported)))
    screen should include("not reported by this JVM")
    screen should include("not reported on this platform")
  }

  test("a server that cannot be asked shows the reason instead of an empty pane") {
    val tooOld = ServerRow(
      info("cccc3333", ServerState.Running),
      status = None,
      error = Some(bleep.bsp.AdminError.TooOld(Path.of("/tmp/sockets/cccc3333"))),
      isCurrent = false,
      processes = None,
      parent = None,
      isOutdated = false,
      published = None
    )

    draw(stateWith(List(tooOld)).copy(tab = Tab.Overview)) should include("older bleep")
  }

  /** Work costs cpu slots; a fork costs machine memory, and slots only while work runs on it. Listed together those read as nonsense — "using 0 slot(s), 5120
    * MB" — and leave you unable to explain why a server at zero slots is holding gigabytes.
    */
  test("forks are listed apart from the work, since they are charged for different things") {
    val busyFork = fork(id = 1L, pid = Some(5001L), measuredMb = Some(3072L), busyCpu = 1)
    val warmFork = fork(id = 2L, pid = Some(5002L), measuredMb = Some(1024L), busyCpu = 0)
    val state = stateWith(List(running("aaaa1111", isCurrent = true, scheduler = working(Nil, List(busyFork, warmFork))))).copy(tab = Tab.Activity)

    val screen = draw(state)
    screen should include("RUNNING NOW — 1 of 18 slots: 0 in the heap, 1 on forks")
    screen should include("FORKS — 2, charged 4096 MB; 4096 MB measured from here")
    screen should include("1 slot(s)")
    screen should include("warm")
    withClue("the explanation belongs next to the numbers that prompt the question: ") {
      screen should include("charged its bound")
    }
  }

  /** Until the scheduler has measured a fork's process tree it charges the bound — heap plus overhead — which can be several times what the fork uses. A charge
    * is not a measurement and must not be worded as one.
    */
  test("a fork's memory is called a charge, bound until measured, and the measurement is shown once there is one") {
    val starting = fork(id = 1L, pid = None, measuredMb = None, busyCpu = 0)
    val row = running("aaaa1111", isCurrent = true, scheduler = working(Nil, List(starting)))
    val screen = draw(stateWith(List(row)).copy(tab = Tab.Activity))
    screen should include("starting, charged 2560 MB bound")
    screen should include("no pid yet")
    screen should not include "holding"

    val measured = withScheduler(row, _.copy(forks = List(fork(id = 1L, pid = Some(5001L), measuredMb = Some(1200L), busyCpu = 0))))
    draw(stateWith(List(measured)).copy(tab = Tab.Activity)) should include("measured 1200 MB (bound 2560 MB)")
  }

  test("a memory total over only some of the servers says how many it left out") {
    val measured = running("aaaa1111", isCurrent = true)
    val gone = running("bbbb2222", isCurrent = false).copy(processes = None)
    draw(stateWith(List(measured, gone))) should include("(1 server could not be measured)")
    draw(stateWith(List(measured))) should not include "could not be measured"
  }

  test("with only warm forks alive the work section says nothing is running, not zero compiles") {
    val warm = fork(id = 1L, pid = Some(5002L), measuredMb = Some(512L), busyCpu = 0)
    val state = stateWith(List(running("aaaa1111", isCurrent = true, scheduler = working(Nil, List(warm))))).copy(tab = Tab.Activity)

    draw(state) should include("RUNNING NOW — nothing")
  }

  test("the activity tab shows what is running and who is connected") {
    val state = stateWith(List(running("aaaa1111", isCurrent = true, scheduler = working(List(compile("bleep-core", cpu = 4)), Nil)))).copy(tab = Tab.Activity)

    val screen = draw(state)
    screen should include("bleep-core")
    screen should include("Metals")
  }

  test("the workspaces tab lists active operations under their workspace") {
    val workspace = WorkspaceDto(
      path = "/home/dev/project",
      buildCached = true,
      activeOperations = List(OperationDto("op-1", "compile", List("bleep-core", "bleep-cli"), 4000))
    )
    val state = stateWith(List(running("aaaa1111", isCurrent = true, workspaces = List(workspace)))).copy(tab = Tab.Workspaces)

    val screen = draw(state)
    screen should include("/home/dev/project")
    screen should include("compile  bleep-core, bleep-cli")
  }

  /** `parallelism` is each server's own, so there is no machine-wide slot capacity to quote: an earlier header added the servers' capacities and claimed 36
    * slots on an 18-core machine. Held slots do sum — each server really is holding those — so that is all the header counts.
    */
  test("the header counts the slots held across servers and claims no machine-wide capacity") {
    def busy(hash: String) = withScheduler(running(hash, isCurrent = false), _.copy(cpuInUse = 9))

    val screen = draw(stateWith(List(busy("aaaa1111"), busy("bbbb2222"))))
    screen should include("18 slots busy")
    screen should not include "of 36 slots"
    screen should not include "of 18 slots busy"
  }

  /** The machine-wide line is the scheduler's own arithmetic, from the freshest probe and every server's `state.json`: what a claiming server would see. */
  test("the summary shows the machine's memory against the fork ceiling, what is pending for starting forks, and the pressure") {
    val starting = fork(id = 7L, pid = None, measuredMb = None, busyCpu = 0)
    val published = bleep.machine.StateJson(
      version = 1,
      pid = 4242L,
      startedAtEpochMs = NowMs - 600_000L,
      bleepVersion = "1.0.0-M11",
      updatedAtEpochMs = NowMs,
      requests = 1,
      cpuInUse = 0,
      wantsMore = false,
      shuttingDown = false,
      idleSinceEpochMs = None,
      forks = List(bleep.machine.StateFork(7L, None, bleep.machine.ForkKind.TestSuite, 2560L, bleep.machine.StateForkState.Starting, NowMs - 1000L))
    )
    val row = running("aaaa1111", isCurrent = true, scheduler = working(Nil, List(starting))).copy(published = Some(published))

    val screen = draw(stateWith(List(row)))
    screen should include(
      "12.0 GB used of 40.0 GB fork ceiling (48.0 GB − 8.0 GB headroom), 2.5 GB pending for starting forks — pressure normal"
    )
  }

  test("a server without a pressure signal says so and why, instead of reporting normal") {
    val noSignal = withScheduler(
      running("aaaa1111", isCurrent = true),
      s => s.copy(machine = s.machine.map(_.copy(pressure = "no-signal", pressureReason = Some("macOS 12 has no memory pressure sysctl"))))
    )
    draw(stateWith(List(noSignal))) should include("no pressure signal (macOS 12 has no memory pressure sysctl)")
  }

  test("servers running unconstrained are named in the summary, with the reason") {
    val unconstrained = withScheduler(
      running("aaaa1111", isCurrent = true),
      _.copy(mode = SchedulerDto.Unconstrained, unconstrainedReason = Some("machineScheduling: unconstrained in the server config"), machine = None)
    )
    val screen = draw(stateWith(List(unconstrained)))
    screen should include("pid 4242 runs unconstrained — machineScheduling: unconstrained in the server config")
    withClue("one unconstrained server has no machine reading to show, and the summary must not claim one: ") {
      screen should not include "fork ceiling"
    }
  }

  test("a server that could not get the lock names the holder by the server the dashboard knows it as") {
    val holder = running("bbbb2222", isCurrent = false)
    val lockedOut = withScheduler(
      running("aaaa1111", isCurrent = true),
      _.copy(lock = LockDto(state = "unavailable", holderPid = Some(4242L), holderStartedAtEpochMs = Some(NowMs - 600_000L), holderHeldForMs = Some(1300L)))
    )
    val screen = draw(stateWith(List(lockedOut, holder)))
    screen should include("could not get machine.lock: pid 4242, held for 1s")
  }

  /** A wedged server's forks still count against the machine: its `state.json` is what the other schedulers read, and so does the dashboard. */
  test("a server that does not answer is described from its state.json when it has one") {
    val published = bleep.machine.StateJson(
      version = 1,
      pid = 4242L,
      startedAtEpochMs = NowMs - 600_000L,
      bleepVersion = "1.0.0-M11",
      updatedAtEpochMs = NowMs,
      requests = 1,
      cpuInUse = 3,
      wantsMore = true,
      shuttingDown = false,
      idleSinceEpochMs = None,
      forks =
        List(bleep.machine.StateFork(1L, Some(5001L), bleep.machine.ForkKind.TestSuite, 2560L, bleep.machine.StateForkState.Measured(3072L), NowMs - 1000L))
    )
    val wedged = running("aaaa1111", isCurrent = true).copy(status = None, published = Some(published))
    draw(stateWith(List(wedged))) should include("its state.json holds 1 fork(s), 3 slot(s) busy")
  }

  test("the header answers the machine-level question before any server is selected") {
    val busy = running("aaaa1111", isCurrent = true, scheduler = working(List(compile("bleep-core", cpu = 4)), Nil))
    val screen = draw(stateWith(List(busy, dead("bbbb2222"))))

    screen should include("1 running")
    screen should include("1 stopped")
    withClue("counting compiles alone undersold a server full of test suites; slots is what busy means: ") {
      screen should include("slots busy")
    }
  }

  test("gauges render as bars with a percentage, not as empty boxes") {
    val screen = drawOverview(stateWith(List(running("aaaa1111", isCurrent = true))))

    screen should include("4%")
    withClue("a titled gauge draws a block border instead of a bar: ") {
      screen should not include "┌────────────────────┐"
    }
  }

  test("the tab bar shows every tab and which one is open") {
    val screen = draw(stateWith(List(running("aaaa1111", isCurrent = true))))

    screen should include("[Processes]")
    screen should include("Overview")
    screen should include("Workspaces")
    screen should include("Activity")
    screen should include("Config")
  }

  test("a running server says what it is doing, and a stopped one what it is holding") {
    val busy = running("aaaa1111", isCurrent = true, scheduler = working(List(compile("bleep-core", cpu = 4)), Nil))
    draw(stateWith(List(busy))) should include("compiling bleep-core")

    draw(press(stateWith(List(dead("bbbb2222"))), KeyPress.ShowDead)) should include("MB on disk")
  }

  test("an empty machine renders the invitation rather than an empty box") {
    draw(ServerTopState.initial(NowMs, TestMachine)) should include("No compile servers running")
  }

  // ── clicking ────────────────────────────────────────────────────

  private val twoServers = List(running("aaaa1111", isCurrent = true), running("bbbb2222", isCurrent = false))

  test("every server row is clickable, and selects the server it names") {
    val state = stateWith(twoServers)
    val selections = clicksAnywhere(state).collect { case msg: Msg.SelectRow => msg.index }.distinct.sorted

    selections shouldBe List(0, 1)
  }

  test("a click selects a row rather than nudging the cursor, so it lands where you pointed") {
    val state = stateWith(twoServers)
    val selected = ServerTopUpdate.update(state, Msg.SelectRow(1))._1

    selected.selectedRow.map(_.hash) shouldBe Some("bbbb2222")
  }

  test("every tab is clickable") {
    val opened = clicksAnywhere(stateWith(twoServers)).collect { case msg: Msg.SelectTab => msg.tab }.distinct

    withClue(s"got $opened: ") {
      opened should contain allElementsOf Tab.all
    }
  }

  test("the action buttons are clickable, not just documented") {
    val pressed = clicksAnywhere(stateWith(twoServers)).collect { case Msg.Key(press) => press }.distinct

    pressed should contain(KeyPress.Quit)
    pressed should contain(KeyPress.Kill)
    pressed should contain(KeyPress.Restart)
    pressed should contain(KeyPress.NextTab)
    pressed should contain(KeyPress.ShowDead)
  }

  test("a confirmation can be answered with the mouse") {
    val asked = press(stateWith(twoServers), KeyPress.Kill)
    val pressed = clicksAnywhere(asked).collect { case Msg.Key(press) => press }.distinct

    pressed should contain(KeyPress.Yes)
    pressed should contain(KeyPress.No)
  }

  test("clicking elsewhere while a confirmation is up dismisses it rather than answering") {
    val asked = press(stateWith(twoServers), KeyPress.Kill)
    asked.pending shouldBe defined

    val (after, effects) = ServerTopUpdate.update(asked, Msg.SelectRow(1))
    withClue("pointing at another server plainly means 'not that one': ") {
      after.pending shouldBe None
      effects shouldBe empty
    }
  }

  // ── update ──────────────────────────────────────────────────────

  private def press(state: ServerTopState, key: KeyPress): ServerTopState =
    ServerTopUpdate.update(state, Msg.Key(key))._1

  test("selection stays on the same server when the list changes underneath it") {
    val before = stateWith(List(running("aaaa1111", isCurrent = true), running("bbbb2222", isCurrent = false)))
    val onSecond = press(before, KeyPress.Down)
    onSecond.selectedRow.map(_.hash) shouldBe Some("bbbb2222")

    // The first server goes away between ticks. Holding the index would silently move the cursor onto a different daemon — and `k` is one keystroke away.
    val after = ServerTopUpdate.update(onSecond, Msg.Refreshed(List(running("bbbb2222", isCurrent = false)), NowMs))._1
    after.selectedRow.map(_.hash) shouldBe Some("bbbb2222")
  }

  test("selection cannot run off either end of the list") {
    val state = stateWith(List(running("aaaa1111", isCurrent = true), running("bbbb2222", isCurrent = false)))

    press(press(state, KeyPress.Up), KeyPress.Up).selected shouldBe 0
    press(press(press(state, KeyPress.Down), KeyPress.Down), KeyPress.Down).selected shouldBe 1
  }

  test("killing asks first — a compile server may be in the middle of a build") {
    val state = stateWith(List(running("aaaa1111", isCurrent = true)))
    val (asked, effects) = ServerTopUpdate.update(state, Msg.Key(KeyPress.Kill))

    effects shouldBe empty
    asked.pending.map(_.prompt) shouldBe Some("kill pid 4242 (aaaa1111)? (y/n)")

    val (confirmed, confirmedEffects) = ServerTopUpdate.update(asked, Msg.Key(KeyPress.Yes))
    confirmed.pending shouldBe None
    confirmedEffects shouldBe List(Effect.Perform(Action.Kill, asked.rows.head))
  }

  test("declining the confirmation does nothing at all") {
    val asked = press(stateWith(List(running("aaaa1111", isCurrent = true))), KeyPress.Kill)
    val (declined, effects) = ServerTopUpdate.update(asked, Msg.Key(KeyPress.No))

    declined.pending shouldBe None
    effects shouldBe empty
  }

  test("a server that vanished between the prompt and the answer is reported, not killed by index") {
    val asked = press(stateWith(List(running("aaaa1111", isCurrent = true))), KeyPress.Kill)
    val gone = ServerTopUpdate.update(asked, Msg.Refreshed(Nil, NowMs))._1.copy(pending = asked.pending)

    val (after, effects) = ServerTopUpdate.update(gone, Msg.Key(KeyPress.Yes))
    effects shouldBe empty
    after.message shouldBe Some("aaaa1111 is gone")
  }

  test("with only stopped servers there is nothing on the main view to kill") {
    val after = press(stateWith(List(dead("bbbb2222"))), KeyPress.Kill)

    after.pending shouldBe None
    after.message shouldBe Some("no server selected")
  }

  test("tab cycles and wraps") {
    val state = stateWith(List(running("aaaa1111", isCurrent = true)))
    val tabs = List.iterate(state, Tab.all.length + 1)(press(_, KeyPress.NextTab)).map(_.tab)

    tabs.take(Tab.all.length) shouldBe Tab.all
    tabs.last shouldBe Tab.Processes
  }

  test("left and right move between tabs, and left from the first wraps to the last") {
    val state = stateWith(List(running("aaaa1111", isCurrent = true)))

    press(state, KeyPress.NextTab).tab shouldBe Tab.Overview
    withClue("wrapping backwards beats doing nothing at the left edge: ") {
      press(state, KeyPress.Left).tab shouldBe Tab.all.last
    }
    press(press(state, KeyPress.NextTab), KeyPress.Left).tab shouldBe Tab.Processes
  }

  test("the log tab shows the tail the loop read, and says so when there is none") {
    val state = stateWith(List(running("aaaa1111", isCurrent = true))).copy(tab = Tab.Log)

    draw(state) should include("no log yet")
    draw(state.copy(logTail = List("[info ] compiling bleep-core", "[error] boom"))) should include("compiling bleep-core")
  }

  private def logState(lines: Int): ServerTopState =
    stateWith(List(running("aaaa1111", isCurrent = true)))
      .copy(tab = Tab.Log, logTail = (1 to lines).map(index => s"line $index").toList)

  test("the log follows the newest line, and says so") {
    val screen = draw(logState(500))

    screen should include("following")
    withClue("following means the end of the log is what you see: ") {
      screen should include("line 500")
      screen should not include "line 1 "
    }
  }

  test("scrolling back shows older lines and stops following") {
    val scrolled = ServerTopUpdate.update(logState(500), Msg.ScrollLog(100))._1
    scrolled.followingLog shouldBe false

    val screen = draw(scrolled)
    screen should include("100 lines back")
    screen should include("line 400")
    screen should not include "line 500"
  }

  test("new lines arriving do not drag the view out from under someone reading history") {
    val scrolled = ServerTopUpdate.update(logState(500), Msg.ScrollLog(100))._1
    val grown = ServerTopUpdate.update(scrolled, Msg.LogTail((1 to 600).map(index => s"line $index").toList))._1

    withClue("the reader stays where they were, counted from the end: ") {
      grown.logScrollFromBottom shouldBe 100
      draw(grown) should include("line 500")
    }
  }

  test("scrolling back to the bottom resumes following") {
    val scrolled = ServerTopUpdate.update(logState(500), Msg.ScrollLog(50))._1
    val returned = ServerTopUpdate.update(scrolled, Msg.ScrollLog(-50))._1

    returned.followingLog shouldBe true
    draw(returned) should include("following")
  }

  test("scrolling cannot run past either end of the log") {
    val state = logState(20)

    ServerTopUpdate.update(state, Msg.ScrollLog(-5))._1.logScrollFromBottom shouldBe 0
    ServerTopUpdate.update(state, Msg.ScrollLog(9999))._1.logScrollFromBottom shouldBe 19
  }

  test("the log has a scrollbar, with a thumb that is not the whole track") {
    val screen = draw(logState(500))

    screen should include("┃")
    screen should include("│")
  }

  test("arrows scroll the log while it is open, and select servers everywhere else") {
    val onLog = logState(500)
    press(onLog, KeyPress.Up).logScrollFromBottom shouldBe 1

    val onOverview = stateWith(List(running("aaaa1111", isCurrent = true), running("bbbb2222", isCurrent = false)))
    press(onOverview, KeyPress.Down).selected shouldBe 1
  }

  test("choosing another server shows its log from the end, not the previous one's position") {
    val scrolled = ServerTopUpdate.update(logState(500), Msg.ScrollLog(100))._1
    ServerTopUpdate.update(scrolled, Msg.SelectRow(0))._1.logScrollFromBottom shouldBe 0
  }

  test("the config tab shows how the server was launched, including its classpath") {
    val identity = bleep.bsp.ServerJson(
      bleepVersion = "1.0.0-M11",
      jvmName = "graalvm-community",
      jvmVersion = "25.0.1",
      javaBin = "/opt/jvm/bin/java",
      javaOpts = List("-Xmx12g", "-XX:+UseZGC"),
      serverMainClass = "bleep.bsp.BspServerDaemon",
      command = List("/opt/jvm/bin/java", "-Xmx12g", "-cp", classpathArg(List("/a/one.jar", "/b/two.jar")), "bleep.bsp.BspServerDaemon"),
      workingDir = "/tmp/socket-dir",
      spawnedAtEpochMs = 1L
    )
    val row = running("aaaa1111", isCurrent = true)
    val withIdentity = row.copy(info = row.info.copy(identity = Some(identity)))
    val screen = draw(stateWith(List(withIdentity)).copy(tab = Tab.Startup))

    screen should include("HOW THIS SERVER WAS STARTED")
    screen should include("/opt/jvm/bin/java")
    screen should include("-Xmx12g")
    withClue("the classpath is the answer to 'why is this server behaving like another version': ") {
      screen should include("2 entries")
      screen should include("/a/one.jar")
      screen should include("/b/two.jar")
    }
  }

  /** A real bleep-bsp classpath is over 200 jars. Rendered as one element per line that is 200+ children in a container, and the layout solver runs over every
    * child on every frame — which froze the dashboard outright the moment anyone opened this tab. Panes whose length is driven by data render as one widget.
    *
    * Timing is the only way to state "this does not scale with the data" without asserting on jatatui's internals; the bound is loose enough not to be flaky
    * and tight enough that the old behaviour (seconds) could never pass.
    */
  test("a pane with a real-sized classpath renders promptly, rather than one layout child per entry") {
    val identity = bleep.bsp.ServerJson(
      bleepVersion = "1.0.0-M11",
      jvmName = "graalvm-community",
      jvmVersion = "25.0.1",
      javaBin = "/opt/jvm/bin/java",
      javaOpts = List("-Xmx12g"),
      serverMainClass = "bleep.bsp.BspServerDaemon",
      command = List("/opt/jvm/bin/java", "-cp", classpathArg((1 to 202).map(index => s"/jars/lib-$index.jar")), "bleep.bsp.BspServerDaemon"),
      workingDir = "/tmp/socket-dir",
      spawnedAtEpochMs = 1L
    )
    val row = running("aaaa1111", isCurrent = true)
    val state = stateWith(List(row.copy(info = row.info.copy(identity = Some(identity))))).copy(tab = Tab.Startup)

    draw(state) // warm up, so the measurement is not dominated by first-render setup
    val startedAt = System.nanoTime()
    val screen = draw(state)
    val elapsedMs = (System.nanoTime() - startedAt) / 1000000

    screen should include("202 entries")
    withClue(s"rendering a 202-entry classpath took ${elapsedMs}ms: ") {
      elapsedMs should be < 250L
    }
  }

  test("a server with no recorded launch command says so rather than showing an empty section") {
    draw(stateWith(List(running("aaaa1111", isCurrent = true))).copy(tab = Tab.Startup)) should include("too old to record it")
  }

  test("the actions are a row of buttons") {
    val screen = draw(stateWith(List(running("aaaa1111", isCurrent = true))))

    screen should include("[ k kill ]")
    screen should include("[ r restart ]")
    screen should include("[ q quit ]")
  }

  // ── the startup tab ──────────────────────────────────────────────

  private def startupState: ServerTopState = {
    val identity = bleep.bsp.ServerJson(
      bleepVersion = "1.0.0-M11",
      jvmName = "graalvm-community",
      jvmVersion = "25.0.1",
      javaBin = "/opt/jvm/bin/java",
      javaOpts = List("-Xmx12g", "-XX:+UseZGC"),
      serverMainClass = "bleep.bsp.BspServerDaemon",
      // As long as the real thing: a coursier cache path runs well past 120 characters, which is why this pane needs an x axis at all.
      command = List(
        "/opt/jvm/bin/java",
        "-cp",
        classpathArg(
          (1 to 202).map(index =>
            s"/Users/dev/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/example/deeply/nested/group/library-$index/1.2.3/library-$index-1.2.3.jar"
          )
        ),
        "x"
      ),
      workingDir = "/tmp/socket-dir",
      spawnedAtEpochMs = 1L
    )
    val row = running("aaaa1111", isCurrent = true)
    measured(stateWith(List(row.copy(info = row.info.copy(identity = Some(identity))))).copy(tab = Tab.Startup))
  }

  /** Render once and apply whatever the view reported back, the way the loop does between frames — the startup pane's horizontal bounds arrive this way. */
  private def measured(state: ServerTopState): ServerTopState = measuredAt(state, width = 140)

  private def measuredAt(state: ServerTopState, width: Int): ServerTopState = {
    val dispatched = scala.collection.mutable.ListBuffer.empty[Msg]
    val harness = new TestHarness(width, 30)
    harness.render(ServerTopView.render(state, msg => dispatched.append(msg): Unit))
    dispatched.foldLeft(state)((current, msg) => ServerTopUpdate.update(current, msg)._1)
  }

  test("← at the left edge of the startup pane goes back a tab, so arriving with → is not a one-way trip") {
    val arrived = measured(press(startupState.copy(tab = Tab.Config), KeyPress.Right))
    arrived.tab shouldBe Tab.Startup
    press(arrived, KeyPress.Left).tab shouldBe Tab.Config

    val scrolled = press(arrived, KeyPress.Right)
    withClue("away from the edge ← scrolls back first: ") {
      press(scrolled, KeyPress.Left) shouldBe scrolled.copy(startupScrollX = 0)
    }
  }

  test("→ at the right edge of the startup pane wraps on to the next tab, and the offset never runs past the edge") {
    val atEdge = ServerTopUpdate.update(startupState, Msg.ScrollStartup(0, 100000))._1
    atEdge.startupScrollX shouldBe atEdge.startupMaxScrollX
    press(atEdge, KeyPress.Right).tab shouldBe Tab.all.head
  }

  test("a pane with nothing to scroll sideways lets the arrows change tab straight away") {
    val narrow = measured(stateWith(List(running("aaaa1111", isCurrent = true))).copy(tab = Tab.Startup))
    narrow.startupMaxScrollX shouldBe 0
    press(narrow, KeyPress.Left).tab shouldBe Tab.Config
  }

  test("the startup tab scrolls down through the classpath") {
    val top = draw(startupState)
    top should include("CLASSPATH — 202 entries")

    val scrolled = draw(ServerTopUpdate.update(startupState, Msg.ScrollStartup(40, 0))._1)
    withClue("scrolling down should reach entries the first screen could not show: ") {
      scrolled should not include "CLASSPATH — 202 entries"
      scrolled should include("library-4")
    }
  }

  test("the startup tab scrolls sideways, which is the only way to read a long path") {
    // Narrow on purpose: sideways scrolling only means anything when the content is wider than the pane, and the offset is clamped to the overflow.
    val unscrolled = drawAt(startupState, width = 80)
    val sideways = drawAt(ServerTopUpdate.update(measuredAt(startupState, width = 80), Msg.ScrollStartup(0, 24))._1, width = 80)

    sideways should not be unscrolled
    withClue("shifting right should cut off the start of each path: ") {
      sideways should not include "/Users/dev/Library/Caches/Coursier/v1"
    }
  }

  test("arrows scroll the startup pane instead of changing tab, since a classpath is wider than any terminal") {
    press(startupState, KeyPress.Right).startupScrollX should be > 0
    press(startupState, KeyPress.Down).startupScrollY should be > 0

    withClue("the tab must not change while the arrows are busy scrolling: ") {
      press(startupState, KeyPress.Right).tab shouldBe Tab.Startup
    }
  }

  test("scrolling cannot go negative in either direction") {
    ServerTopUpdate.update(startupState, Msg.ScrollStartup(-10, -10))._1.startupScrollY shouldBe 0
    ServerTopUpdate.update(startupState, Msg.ScrollStartup(-10, -10))._1.startupScrollX shouldBe 0
  }

  test("scrolling past the end is clamped by the pane, not left to run away") {
    val far = ServerTopUpdate.update(startupState, Msg.ScrollStartup(100000, 0))._1
    val screen = draw(far)

    withClue("a huge offset should still render the tail of the list rather than an empty pane: ") {
      screen should include("library-202")
    }
  }

  test("choosing another server resets the startup pane to the top left") {
    val scrolled = ServerTopUpdate.update(startupState, Msg.ScrollStartup(50, 50))._1
    val moved = ServerTopUpdate.update(scrolled, Msg.SelectRow(0))._1

    moved.startupScrollY shouldBe 0
    moved.startupScrollX shouldBe 0
  }

  /** Every bleep version ever run in a directory leaves a server that has it loaded, so "does it hold this workspace" matches several. Marking more than one
    * "this build" is worse than marking none — that label is what a reader trusts when deciding which server to kill.
    */
  test("only one server is marked as this build, however many hold the workspace") {
    val clientVersion = bleep.model.BleepVersion.current.value
    val candidates = List(("older11", Some("1.0.0-M10")), ("mine22", Some(clientVersion)), ("older33", Some("1.0.0-M9")))

    bleep.bsp.ServerDirs.currentAmong(candidates, clientVersion) shouldBe Some("mine22")
  }

  test("with no version match it still picks exactly one rather than several") {
    val candidates = List(("aaa", Some("1.0.0-M10")), ("bbb", Some("1.0.0-M9")))
    bleep.bsp.ServerDirs.currentAmong(candidates, "1.0.0-M11") shouldBe Some("aaa")
  }

  test("nothing holding the workspace means nothing is marked") {
    bleep.bsp.ServerDirs.currentAmong(Nil, "1.0.0-M11") shouldBe None
  }

  test("a server that holds the workspace but is not ours says so, instead of claiming to be this build") {
    val mine = running("mine1111", isCurrent = true)
    val other = running("other222", isCurrent = false)
    val screen = draw(stateWith(List(mine, other)))

    screen should include("← this build")
    withClue("exactly one row may claim it: ") {
      screen.linesIterator.count(_.contains("← this build")) shouldBe 1
    }
  }

  /** A container solves one layout constraint per child, and given more children than rows it drops one from the middle rather than clipping the end. The
    * CAPACITY heading vanished exactly that way, between a blank line and the row below it, which reads as a rendering glitch rather than a bug.
    */
  test("every section heading survives a pane shorter than its content") {
    // Tall enough for the overview below the summary and server list; what is being tested is that the container does not drop lines from the middle.
    val screen = drawOverview(stateWith(List(running("aaaa1111", isCurrent = true))))

    screen should include("MEMORY —")
    screen should include("SCHEDULER —")
    screen should include("WHAT IT IS KEEPING WARM")
  }

  test("the heap gauge prints its percentage once") {
    val screen = drawOverview(stateWith(List(running("aaaa1111", isCurrent = true))))
    val heapLine = screen.linesIterator.find(_.contains("Heap in use")).getOrElse(fail("no heap row"))

    withClue(s"the widget prints its own label unless silenced: $heapLine ") {
      "%".r.findAllIn(heapLine).size shouldBe 1
    }
  }

  test("the overview leads with one sentence about what the server is doing") {
    val idle = drawOverview(stateWith(List(running("aaaa1111", isCurrent = true))))
    idle should include("Idle")

    val busy = drawOverview(stateWith(List(running("aaaa1111", isCurrent = true, scheduler = working(List(compile("bleep-core", cpu = 4)), Nil)))))
    withClue("what makes a server busy is the slots it holds, whatever kind of work holds them: ") {
      busy should include("Busy — 4 of 18 slots")
      busy should include("1 compile")
    }
  }

  /** A server running one compile and sixteen test suites reported "1 compiling", because that count is of compiles. It is the wrong number to lead with. */
  test("a server full of test suites reads as busy, not as one compile") {
    val suites = (1 to 16).map(index => fork(id = index.toLong, pid = Some(5000L + index), measuredMb = Some(900L), busyCpu = 1)).toList
    val busy = running("aaaa1111", isCurrent = true, scheduler = working(List(compile("bleep-core", cpu = 1)), suites))

    val screen = drawOverview(stateWith(List(busy)))
    screen should include("17 of 18 slots")
    screen should include("16 test suites")
    screen should include("1 compile")
  }

  test("a queue with nothing running says so rather than reading as idle") {
    val queued = withScheduler(running("aaaa1111", isCurrent = true), _.copy(waiting = List(waitingCompile("x", cpu = 1))))

    drawOverview(stateWith(List(queued))) should include("waiting for capacity")
  }

  test("last activity says how long ago, and what the idle clock is doing about it") {
    val row = running("aaaa1111", isCurrent = true)
    val idleAWhile = row.copy(status = row.status.map(_.copy(idleMs = Some(300000L), connections = Nil)))

    val screen = drawOverview(stateWith(List(idleAWhile)))
    screen should include("5m0s ago")
    withClue("the same clock drives the idle shutdown, so say what it will do: ") {
      screen should include("shuts down after")
    }
  }

  test("a connected client stops the idle clock, and the overview says that rather than counting up misleadingly") {
    val row = running("aaaa1111", isCurrent = true)
    val withClient = row.copy(status = row.status.map(_.copy(idleMs = Some(300000L))))

    drawOverview(stateWith(List(withClient))) should include("idle clock is not running")
  }

  test("a server too old to report idle time says so instead of showing zero") {
    val row = running("aaaa1111", isCurrent = true)
    val old = row.copy(status = row.status.map(_.copy(idleMs = None)))

    drawOverview(stateWith(List(old))) should include("not reported by this server")
  }

  test("the overview explains what it is measuring rather than abbreviating it") {
    val screen = drawOverview(stateWith(List(running("aaaa1111", isCurrent = true))))

    screen should include("Heap in use")
    screen should include("CPU slots")
    screen should include("Machine memory")
    withClue("section headings should say what the numbers under them are for: ") {
      screen should include("how much of its heap")
      screen should include("this server's cpu slots, and the machine's memory for forks")
    }
  }

  test("q quits, and quitting during a confirmation just dismisses it") {
    press(stateWith(Nil), KeyPress.Quit).quit shouldBe true

    val asked = press(stateWith(List(running("aaaa1111", isCurrent = true))), KeyPress.Kill)
    val dismissed = press(asked, KeyPress.Quit)
    dismissed.pending shouldBe None
    withClue("the first q should cancel the prompt, not tear down the dashboard: ") {
      dismissed.quit shouldBe false
    }
  }

  test("processes are named by what bleep forked them for, and otherwise by their main class") {
    val java = Some("/opt/jvm/bin/java")
    ProcessTree.describe(java, List("-Xmx2g", "-cp", "a.jar:b.jar", "bleep.bsp.BspServerDaemon", "--socket", "/x")) shouldBe "compile server"
    ProcessTree.describe(java, List("-classpath", "a.jar", "bleep.testing.runner.ForkedTestRunner")) shouldBe "test JVM"
    ProcessTree.describe(java, List("-cp", "a.jar", "com.example.GenParsers", "arg")) shouldBe "java GenParsers"
    ProcessTree.describe(java, List("-jar", "/x/y/tool.jar")) shouldBe "java -jar tool.jar"
    ProcessTree.describe(Some("/usr/local/bin/node"), List("run.js")) shouldBe "node"
  }
}
