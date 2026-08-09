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

  private def status(workspaces: List[WorkspaceDto], active: List[MachineEntryDto]): DaemonStatus = DaemonStatus(
    adminProtocolVersion = 1,
    bleepVersion = "1.0.0-M11",
    pid = 4242L,
    startedAtEpochMs = NowMs - 600_000L, // ten minutes of uptime
    socketDir = "/tmp/sockets/aaaa1111",
    jvm = jvm,
    machine = MachineSnapshotDto(
      totalCpu = 18,
      usedCpu = active.map(_.cpu).sum,
      totalMemoryMb = 16000,
      usedMemoryMb = 2000,
      activeCompiles = active.size,
      active = active,
      waiting = Nil
    ),
    connections = List(ConnectionDto(1, NowMs, observer = false, Some("Metals"), Some("1.0"), Some("/home/dev/project"))),
    workspaces = workspaces,
    buildCache = BuildCacheDto(cachedWorkspaces = workspaces.map(_.path), bound = 12),
    analysisCache = AnalysisCacheDto(entries = 40, fileBytes = 5L * 1024 * 1024, internedClasses = 10, sharedAnalyses = 2, contentHits = 7, perWorkspace = Nil),
    config = ServerConfigDto(
      parallelism = 18,
      compileServerMaxMemory = Some("12g"),
      testRunnerMaxMemory = None,
      maxCachedWorkspaces = 12,
      bspReadTimeoutMillis = 30 * 60000L,
      compileServerIdleTimeoutMillis = 60 * 60000L,
      testIdleTimeoutMinutes = 2,
      heapPressureThreshold = 0.8
    ),
    idleMs = Some(0L)
  )

  private def info(hash: String, state: ServerState): ServerDirInfo =
    ServerDirInfo(Path.of("/tmp/sockets").resolve(hash), hash, state, Some(4242L), None, 0L)

  private def running(hash: String, isCurrent: Boolean, workspaces: List[WorkspaceDto] = Nil, active: List[MachineEntryDto] = Nil): ServerRow =
    ServerRow(info(hash, ServerState.Running), Some(status(workspaces, active)), None, isCurrent)

  private def dead(hash: String): ServerRow =
    ServerRow(info(hash, ServerState.Dead(crashed = false)), None, None, isCurrent = false)

  private def stateWith(rows: List[ServerRow]): ServerTopState =
    ServerTopState.initial(NowMs).copy(rows = rows)

  /** Join classpath entries the way the daemon's own launch command does, in `BspServerOperations`.
    *
    * Hardcoding `:` here passes on unix and quietly lies on Windows, where the separator is `;`: the fixture becomes one enormous single path, the pane
    * faithfully reports "1 entries", and the failure looks like a rendering bug rather than a test that built the wrong input.
    */
  private def classpathArg(entries: Seq[String]): String = entries.mkString(java.io.File.pathSeparator)

  /** Render at a fixed size and read the buffer back as plain text. */
  private def draw(state: ServerTopState): String = drawAt(state, width = 140)

  private def drawAt(state: ServerTopState, width: Int): String = {
    val harness = new TestHarness(width, 30)
    harness.render(ServerTopView.render(state, _ => ()))
    TestBackend.bufferView(harness.backend.buffer())
  }

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

    screen should include("bleep")
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
    screen should include("aaaa1111")
  }

  test("a server with nothing recorded says so in the version column rather than showing a blank") {
    draw(stateWith(List(running("aaaa1111", isCurrent = true)))) should include("unknown")
  }

  test("the server list shows state, heap and uptime, and marks this build's server") {
    val screen = draw(stateWith(List(running("aaaa1111", isCurrent = true), dead("bbbb2222"))))

    screen should include("aaaa1111")
    screen should include("running")
    screen should include("512/12288 MB")
    screen should include("10m0s")
    screen should include("← this build")
    screen should include("bbbb2222")
    screen should include("dead")
  }

  test("the overview keeps the live set distinct from heap used, which is the number that says retaining vs churning") {
    val screen = draw(stateWith(List(running("aaaa1111", isCurrent = true))))

    screen should include("Retained")
    screen should include("128 MB still held")
    screen should include("383 file descriptors")
    screen should include("Major Cycles")
  }

  test("an unmeasurable live set renders n/a rather than a zero that looks like a measurement") {
    val row = running("aaaa1111", isCurrent = true)
    val unsupported = row.copy(status = row.status.map(s => s.copy(jvm = s.jvm.copy(heapLiveMb = -1L, openFileDescriptors = None))))

    val screen = draw(stateWith(List(unsupported)))
    screen should include("not reported by this JVM")
    screen should include("not reported on this platform")
  }

  test("a server that cannot be asked shows the reason instead of an empty pane") {
    val tooOld = ServerRow(
      info("cccc3333", ServerState.Running),
      status = None,
      error = Some(bleep.bsp.AdminError.TooOld(Path.of("/tmp/sockets/cccc3333"))),
      isCurrent = false
    )

    draw(stateWith(List(tooOld))) should include("older bleep")
  }

  /** A suite is charged a slot and no memory; the JVM it runs in is charged memory and no slot, and stays alive between suites. Listed together those read as
    * nonsense — "using 0 slot(s), 5120 MB" — and leave you unable to explain why a server at zero slots is holding gigabytes.
    */
  test("forked JVMs are listed apart from the work, since they are charged for different things") {
    val suite = MachineEntryDto("TestFork", "test:dfmt/DfmtBatteryTest", cpu = 1, memoryMb = 0, ageMs = 99000)
    val jvm = MachineEntryDto("TestFork", "jvm ce91585a08d64aec", cpu = 0, memoryMb = 5120, ageMs = 99000)
    val state = stateWith(List(running("aaaa1111", isCurrent = true, active = List(suite, jvm)))).copy(tab = Tab.Activity)

    val screen = draw(state)
    screen should include("RUNNING NOW — 1 operation(s)")
    screen should include("FORKED JVMS — 1 holding 5120 MB")
    withClue("the explanation belongs next to the numbers that prompt the question: ") {
      screen should include("charged above, to the work")
    }
  }

  test("with only forked JVMs alive the work section says nothing is running, not zero compiles") {
    val jvm = MachineEntryDto("TestFork", "jvm abc", cpu = 0, memoryMb = 512, ageMs = 1000)
    val state = stateWith(List(running("aaaa1111", isCurrent = true, active = List(jvm)))).copy(tab = Tab.Activity)

    draw(state) should include("RUNNING NOW — nothing")
  }

  test("the activity tab shows what is running and who is connected") {
    val compiling = MachineEntryDto(kind = "Compile", label = "bleep-core", cpu = 4, memoryMb = 512, ageMs = 3000)
    val state = stateWith(List(running("aaaa1111", isCurrent = true, active = List(compiling)))).copy(tab = Tab.Activity)

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

  /** Every server on a machine sees the same cores, so adding their capacities claimed 36 slots on an 18-core machine. Held slots do sum — each server really
    * is holding those — but capacity is one number.
    */
  test("the header does not multiply the machine's capacity by the number of servers") {
    def busy(hash: String) = {
      val row = running(hash, isCurrent = false)
      row.copy(status = row.status.map(s => s.copy(machine = s.machine.copy(usedCpu = 9, totalCpu = 18))))
    }

    val screen = draw(stateWith(List(busy("aaaa1111"), busy("bbbb2222"))))
    screen should include("18 of 18 slots busy")
    screen should not include "of 36 slots"
  }

  test("the header answers the machine-level question before any server is selected") {
    val busy = running("aaaa1111", isCurrent = true, active = List(MachineEntryDto("Compile", "bleep-core", 4, 512, 3000)))
    val screen = draw(stateWith(List(busy, dead("bbbb2222"))))

    screen should include("1 running")
    screen should include("1 stopped")
    withClue("counting compiles alone undersold a server full of test suites; slots is what busy means: ") {
      screen should include("slots busy")
    }
  }

  test("gauges render as bars with a percentage, not as empty boxes") {
    val screen = draw(stateWith(List(running("aaaa1111", isCurrent = true))))

    screen should include("4%")
    withClue("a titled gauge draws a block border instead of a bar: ") {
      screen should not include "┌────────────────────┐"
    }
  }

  test("the tab bar shows every tab and which one is open") {
    val screen = draw(stateWith(List(running("aaaa1111", isCurrent = true))))

    screen should include("[Overview]")
    screen should include("Workspaces")
    screen should include("Activity")
    screen should include("Config")
  }

  test("a running server says what it is doing, and a stopped one what it is holding") {
    val busy = running("aaaa1111", isCurrent = true, active = List(MachineEntryDto("Compile", "bleep-core", 4, 512, 3000)))
    draw(stateWith(List(busy))) should include("1 running")

    draw(stateWith(List(dead("bbbb2222")))) should include("MB on disk")
  }

  test("an empty machine renders the invitation rather than an empty box") {
    draw(ServerTopState.initial(NowMs)) should include("no compile servers")
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
    asked.pending.map(_.prompt) shouldBe Some("kill aaaa1111? (y/n)")

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

  test("stopping something already stopped is refused with a reason rather than prompted") {
    val state = stateWith(List(dead("bbbb2222")))
    val after = press(state, KeyPress.Kill)

    after.pending shouldBe None
    after.message shouldBe Some("bbbb2222 is already dead")
  }

  test("tab cycles and wraps") {
    val state = stateWith(List(running("aaaa1111", isCurrent = true)))
    val tabs = List.iterate(state, Tab.all.length + 1)(press(_, KeyPress.NextTab)).map(_.tab)

    tabs.take(Tab.all.length) shouldBe Tab.all
    tabs.last shouldBe Tab.Overview
  }

  test("left and right move between tabs, and left from the first wraps to the last") {
    val state = stateWith(List(running("aaaa1111", isCurrent = true)))

    press(state, KeyPress.NextTab).tab shouldBe Tab.Workspaces
    withClue("wrapping backwards beats doing nothing at the left edge: ") {
      press(state, KeyPress.Left).tab shouldBe Tab.all.last
    }
    press(press(state, KeyPress.NextTab), KeyPress.Left).tab shouldBe Tab.Overview
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
    stateWith(List(row.copy(info = row.info.copy(identity = Some(identity))))).copy(tab = Tab.Startup)
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
    val sideways = drawAt(ServerTopUpdate.update(startupState, Msg.ScrollStartup(0, 24))._1, width = 80)

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
    val screen = draw(stateWith(List(running("aaaa1111", isCurrent = true))))

    screen should include("MEMORY —")
    screen should include("CAPACITY —")
    screen should include("WHAT IT IS KEEPING WARM")
  }

  test("the heap gauge prints its percentage once") {
    val screen = draw(stateWith(List(running("aaaa1111", isCurrent = true))))
    val heapLine = screen.linesIterator.find(_.contains("Heap in use")).getOrElse(fail("no heap row"))

    withClue(s"the widget prints its own label unless silenced: $heapLine ") {
      "%".r.findAllIn(heapLine).size shouldBe 1
    }
  }

  test("the overview leads with one sentence about what the server is doing") {
    val idle = draw(stateWith(List(running("aaaa1111", isCurrent = true))))
    idle should include("Idle")

    val compiling = MachineEntryDto("Compile", "bleep-core", 4, 512, 3000)
    val busy = draw(stateWith(List(running("aaaa1111", isCurrent = true, active = List(compiling)))))
    withClue("what makes a server busy is the slots it holds, whatever kind of work holds them: ") {
      busy should include("Busy — 4 of 18 slots")
      busy should include("1 compile")
    }
  }

  /** A server running one compile and sixteen test suites reported "1 compiling", because that count is of compiles. It is the wrong number to lead with. */
  test("a server full of test suites reads as busy, not as one compile") {
    val compile = MachineEntryDto("Compile", "bleep-core", 1, 0, 3000)
    val suites = (1 to 16).map(index => MachineEntryDto("TestFork", s"suite-$index", 1, 0, 3000)).toList
    val row = running("aaaa1111", isCurrent = true, active = compile :: suites)
    val busy = row.copy(status = row.status.map(s => s.copy(machine = s.machine.copy(usedCpu = 17))))

    val screen = draw(stateWith(List(busy)))
    screen should include("17 of 18 slots")
    screen should include("16 test suites")
    screen should include("1 compile")
  }

  test("a queue with nothing running says so rather than reading as idle") {
    val row = running("aaaa1111", isCurrent = true)
    val queued = row.copy(status = row.status.map(s => s.copy(machine = s.machine.copy(waiting = List(MachineEntryDto("Compile", "x", 1, 1, 1))))))

    draw(stateWith(List(queued))) should include("waiting for capacity")
  }

  test("last activity says how long ago, and what the idle clock is doing about it") {
    val row = running("aaaa1111", isCurrent = true)
    val idleAWhile = row.copy(status = row.status.map(_.copy(idleMs = Some(300000L), connections = Nil)))

    val screen = draw(stateWith(List(idleAWhile)))
    screen should include("5m0s ago")
    withClue("the same clock drives the idle shutdown, so say what it will do: ") {
      screen should include("shuts down after")
    }
  }

  test("a connected client stops the idle clock, and the overview says that rather than counting up misleadingly") {
    val row = running("aaaa1111", isCurrent = true)
    val withClient = row.copy(status = row.status.map(_.copy(idleMs = Some(300000L))))

    draw(stateWith(List(withClient))) should include("idle clock is not running")
  }

  test("a server too old to report idle time says so instead of showing zero") {
    val row = running("aaaa1111", isCurrent = true)
    val old = row.copy(status = row.status.map(_.copy(idleMs = None)))

    draw(stateWith(List(old))) should include("not reported by this server")
  }

  test("the overview explains what it is measuring rather than abbreviating it") {
    val screen = draw(stateWith(List(running("aaaa1111", isCurrent = true))))

    screen should include("Heap in use")
    screen should include("Compile slots")
    screen should include("Memory for forks")
    withClue("section headings should say what the numbers under them are for: ") {
      screen should include("how much of its heap")
      screen should include("what this server may spend on compiling")
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
}
