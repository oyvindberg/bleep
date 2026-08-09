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
    )
  )

  private def info(hash: String, state: ServerState): ServerDirInfo =
    ServerDirInfo(Path.of("/tmp/sockets").resolve(hash), hash, state, Some(4242L), None, 0L)

  private def running(hash: String, isCurrent: Boolean, workspaces: List[WorkspaceDto] = Nil, active: List[MachineEntryDto] = Nil): ServerRow =
    ServerRow(info(hash, ServerState.Running), Some(status(workspaces, active)), None, isCurrent)

  private def dead(hash: String): ServerRow =
    ServerRow(info(hash, ServerState.Dead(crashed = false)), None, None, isCurrent = false)

  private def stateWith(rows: List[ServerRow]): ServerTopState =
    ServerTopState.initial(NowMs).copy(rows = rows)

  /** Render at a fixed size and read the buffer back as plain text. */
  private def draw(state: ServerTopState): String = {
    val harness = new TestHarness(100, 30)
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

    screen should include("live set")
    screen should include("128MB")
    screen should include("fds 383")
    screen should include("Major Cycles")
  }

  test("an unmeasurable live set renders n/a rather than a zero that looks like a measurement") {
    val row = running("aaaa1111", isCurrent = true)
    val unsupported = row.copy(status = row.status.map(s => s.copy(jvm = s.jvm.copy(heapLiveMb = -1L, openFileDescriptors = None))))

    val screen = draw(stateWith(List(unsupported)))
    screen should include("live set      n/a")
    screen should include("fds n/a")
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

  test("the header answers the machine-level question before any server is selected") {
    val busy = running("aaaa1111", isCurrent = true, active = List(MachineEntryDto("Compile", "bleep-core", 4, 512, 3000)))
    val screen = draw(stateWith(List(busy, dead("bbbb2222"))))

    screen should include("1 running")
    screen should include("1 stopped")
    withClue("a busy machine should say so at the top, not only in a tab: ") {
      screen should include("1 compiling")
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
    draw(stateWith(List(busy))) should include("1 compiling")

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
      press(state, KeyPress.PrevTab).tab shouldBe Tab.all.last
    }
    press(press(state, KeyPress.NextTab), KeyPress.PrevTab).tab shouldBe Tab.Overview
  }

  test("the log tab shows the tail the loop read, and says so when there is none") {
    val state = stateWith(List(running("aaaa1111", isCurrent = true))).copy(tab = Tab.Log)

    draw(state) should include("no log yet")
    draw(state.copy(logTail = List("[info ] compiling bleep-core", "[error] boom"))) should include("compiling bleep-core")
  }

  test("the config tab shows how the server was launched, including its classpath") {
    val identity = bleep.bsp.ServerJson(
      bleepVersion = "1.0.0-M11",
      jvmName = "graalvm-community",
      jvmVersion = "25.0.1",
      javaBin = "/opt/jvm/bin/java",
      javaOpts = List("-Xmx12g", "-XX:+UseZGC"),
      serverMainClass = "bleep.bsp.BspServerDaemon",
      command = List("/opt/jvm/bin/java", "-Xmx12g", "-cp", "/a/one.jar:/b/two.jar", "bleep.bsp.BspServerDaemon"),
      workingDir = "/tmp/socket-dir",
      spawnedAtEpochMs = 1L
    )
    val row = running("aaaa1111", isCurrent = true)
    val withIdentity = row.copy(info = row.info.copy(identity = Some(identity)))
    val screen = draw(stateWith(List(withIdentity)).copy(tab = Tab.Config))

    screen should include("STARTED WITH")
    screen should include("/opt/jvm/bin/java")
    screen should include("-Xmx12g")
    withClue("the classpath is the answer to 'why is this server behaving like another version': ") {
      screen should include("2 entries")
      screen should include("/a/one.jar")
      screen should include("/b/two.jar")
    }
  }

  test("a server with no recorded launch command says so rather than showing an empty section") {
    draw(stateWith(List(running("aaaa1111", isCurrent = true))).copy(tab = Tab.Config)) should include("predates the recorded launch command")
  }

  test("the actions are a row of buttons") {
    val screen = draw(stateWith(List(running("aaaa1111", isCurrent = true))))

    screen should include("[ k kill ]")
    screen should include("[ r restart ]")
    screen should include("[ q quit ]")
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
