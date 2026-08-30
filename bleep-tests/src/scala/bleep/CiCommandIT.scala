package bleep

import bleep.bsp.protocol.BleepBspProtocol.{Event => PE}
import bleep.commands.BuildCreateNew
import bleep.history.TranscriptStore
import com.monovore.decline.Command

import java.nio.file.{Files, Path}

/** `bleep ci` and `--invalidated`, end-to-end over the (in-process, but production) BSP server.
  *
  * The bug being pinned here is the one the flag exists to kill: `bleep build invalidated --base X | xargs bleep compile` prints nothing when nothing changed,
  * `xargs` runs `bleep compile` anyway with no arguments, and that compiles the entire build. "Only build what changed" silently became "build everything"
  * exactly when there was nothing to do. So the empty case is asserted the strict way — not "it compiled few projects" but "no build ran at all", by way of the
  * run history staying empty.
  */
class CiCommandIT extends IntegrationTestHarness {

  private def ciPhase = commands.Ci.ciPhase(
    displayMode = commands.DisplayMode.NoTui,
    jvmOptions = Nil,
    testArgs = Nil,
    includeTags = Nil,
    excludeTags = Nil,
    flamegraph = false,
    cancel = false,
    junitReportDir = None,
    diffBase = None,
    diffOutput = OutputMode.Text,
    clientEnv = bleep.bsp.protocol.BleepBspProtocol.ClientEnv.current(noColor = bleep.PreBootstrapOpts.noColorRequested)
  )

  private def compilePhase = commands.Ci.compilePhase(
    displayMode = commands.DisplayMode.NoTui,
    flamegraph = false,
    cancel = false,
    diffBase = None,
    diffOutput = OutputMode.Text
  )

  /** Run git in the workspace, failing the test on a non-zero exit. Output is swallowed — these are setup steps, not the thing under test. */
  private def git(root: Path, args: String*): Unit = {
    val exitCode = scala.sys.process
      .Process("git" :: args.toList, root.toFile)
      .!(scala.sys.process.ProcessLogger(_ => (), _ => ()))
    if (exitCode != 0) fail(s"git ${args.mkString(" ")} failed with exit code $exitCode")
  }

  /** A repository whose HEAD is the workspace as it stands. `-c` rather than `git config` so the test never depends on the machine's git identity, and gpg
    * signing is forced off for the same reason.
    */
  private def commitAll(root: Path): Unit = {
    git(root, "init")
    git(root, "add", "-A")
    git(root, "-c", "user.email=ci@bleep.build", "-c", "user.name=bleep", "-c", "commit.gpgsign=false", "commit", "-m", "base")
  }

  private def compiledProjects(transcript: bleep.history.Transcript): Set[String] =
    transcript.events.collect { case e: PE.CompileFinished => e.project.value }.toSet

  private def testedProjects(transcript: bleep.history.Transcript): Set[String] =
    transcript.events.collect { case e: PE.SuiteFinished => e.project.value }.toSet

  integrationTest("bleep ci is ONE pass: upstream libraries compile and tests run in the same build") { ws =>
    // `myapp` is a plain library with no tests, `myapp-test` depends on it and holds the suite.
    ws.bleepNew(BuildCreateNew.Language.Java, "myapp")
    val (started, _, _) = ws.start()

    commands.Ci(ciPhase, commands.Ci.Scope.Everything(None), watch = false).run(started).orThrow

    val ids = TranscriptStore.list(started.buildPaths)
    assert(ids.size == 1, s"ci must be one build pass, not a compile run followed by a test run. History: $ids")

    val transcript = TranscriptStore.read(started.buildPaths, ids.head)
    assert(transcript.mode == "test")
    assert(
      compiledProjects(transcript) == Set("myapp", "myapp-test"),
      s"the upstream library must compile in the same pass that runs the tests, got ${compiledProjects(transcript)}"
    )
    assert(testedProjects(transcript) == Set("myapp-test"), s"suites run for the test project, got ${testedProjects(transcript)}")
    succeed
  }

  integrationTest("--invalidated: an empty set builds nothing at all, a changed source builds it and its dependents") { ws =>
    ws.bleepNew(BuildCreateNew.Language.Java, "myapp")
    commitAll(ws.root)
    val (started, _, storingLogger) = ws.start()

    // Nothing changed since HEAD. The `xargs` recipe compiled the whole build here; this must not run a build at all.
    commands.Ci(compilePhase, commands.Ci.Scope.Invalidated(Some("HEAD")), watch = false).run(started).orThrow
    assert(
      TranscriptStore.list(started.buildPaths).isEmpty,
      "nothing invalidated must mean no build ran — falling through to a full build is the bug this flag exists to remove"
    )
    assert(storingLogger.underlying.exists(_.message.plainText.contains("Nothing invalidated vs HEAD")))

    // Touch the library. It and everything transitively depending on it must be built, and nothing else exists here to build.
    val mainJava = ws.root.resolve("myapp/src/java/com/example/Main.java")
    Files.writeString(mainJava, Files.readString(mainJava) + "\n// touched, so git diff has something to report\n")

    commands.Ci(compilePhase, commands.Ci.Scope.Invalidated(Some("HEAD")), watch = false).run(started).orThrow
    val ids = TranscriptStore.list(started.buildPaths)
    assert(ids.size == 1, s"exactly one build should have run, history: $ids")
    val transcript = TranscriptStore.read(started.buildPaths, ids.head)
    assert(
      compiledProjects(transcript) == Set("myapp", "myapp-test"),
      s"the changed project and its dependent, got ${compiledProjects(transcript)}"
    )
    succeed
  }

  /** `jvm:` is a build-level field: it reaches no project's exploded config, and `bleep.yaml` lives in no project's source directory. Before this was handled
    * explicitly, bumping the JDK that compiles every class file in the build reported an empty invalidated set.
    */
  integrationTest("--invalidated: a toolchain jvm change invalidates every project") { ws =>
    ws.file("a/src/java/A.java", "public class A {}")
    ws.file("b/src/java/B.java", "public class B {}")

    // Commit a build pinned to a different JDK, then put the workspace back on the one the harness bootstraps with. Only the committed side is ever parsed
    // from git — nothing tries to fetch the JDK named there.
    ws.file(BuildLoader.BuildFileName, twoProjectBuild(jvmName = "temurin:17", resolver = None))
    commitAll(ws.root)
    ws.file(BuildLoader.BuildFileName, twoProjectBuild(jvmName = model.Jvm.graalvm.name, resolver = None))

    val (started, _, _) = ws.start()
    val invalidated = commands.BuildInvalidated.compute(started, "HEAD").map(_.value)
    assert(invalidated == Set("a", "b"), s"a different toolchain JDK compiles every project differently, got $invalidated")
    succeed
  }

  /** The deliberate non-behaviour, mirroring `ProjectDigest`: a coordinate resolves to the same bytes whichever repository serves it, and a missing artifact
    * fails resolution hard before anything compiles — so there is no false-green path to defend against, and invalidating the world because someone added a
    * repository for one dependency would buy nothing.
    */
  integrationTest("--invalidated: adding a resolver invalidates nothing") { ws =>
    ws.file("a/src/java/A.java", "public class A {}")
    ws.file("b/src/java/B.java", "public class B {}")

    ws.file(BuildLoader.BuildFileName, twoProjectBuild(jvmName = model.Jvm.graalvm.name, resolver = Some("https://repo1.maven.org/maven2")))
    commitAll(ws.root)
    ws.file(BuildLoader.BuildFileName, twoProjectBuild(jvmName = model.Jvm.graalvm.name, resolver = None))

    val (started, _, _) = ws.start()
    val invalidated = commands.BuildInvalidated.compute(started, "HEAD").map(_.value)
    assert(invalidated.isEmpty, s"a changed resolver list cannot change compilation output, got $invalidated")
    succeed
  }

  integrationTest("flag composition: --invalidated, --watch and project arguments, through the real CLI parser") { ws =>
    ws.bleepNew(BuildCreateNew.Language.Java, "myapp")
    val (started, _, _) = ws.start()
    val cli = Command("bleep", "flag composition")(Main.hasBuildOpts(started))

    def parse(args: String*): BleepBuildCommand =
      cli.parse(args.toList) match {
        case Right(cmd) => cmd
        case Left(help) => fail(s"could not parse `bleep ${args.mkString(" ")}`:\n$help")
      }

    // Bare --invalidated leaves the base to be resolved at run time (this branch's upstream); an explicit one is carried through verbatim.
    parse("compile", "--invalidated") match {
      case commands.Ci(phase, commands.Ci.Scope.Invalidated(None), false) => assert(phase.label == "compile")
      case other                                                          => fail(s"expected a compile-scoped Ci, got $other")
    }
    parse("compile", "--invalidated=origin/main", "--watch") match {
      case commands.Ci(phase, commands.Ci.Scope.Invalidated(Some("origin/main")), true) => assert(phase.label == "compile")
      case other                                                                        => fail(s"expected a watching compile-scoped Ci, got $other")
    }
    parse("test", "--invalidated") match {
      case commands.Ci(phase, commands.Ci.Scope.Invalidated(None), false) => assert(phase.label == "test")
      case other                                                          => fail(s"expected a test-scoped Ci, got $other")
    }
    parse("ci") match {
      case commands.Ci(phase, commands.Ci.Scope.Everything(None), false) => assert(phase.label == "compile and test")
      case other                                                         => fail(s"expected a full Ci, got $other")
    }
    parse("ci", "--invalidated", "--watch", "--quiet", "--junit-report", "reports") match {
      case commands.Ci(phase, commands.Ci.Scope.Invalidated(None), true) => assert(phase.label == "compile and test")
      case other                                                         => fail(s"expected a watching, invalidation-scoped Ci, got $other")
    }

    // Plain compile/test are untouched: they still hand watch to ReactiveBsp itself.
    assert(parse("compile").isInstanceOf[commands.ReactiveBsp])
    assert(parse("test").isInstanceOf[commands.ReactiveBsp])

    // --invalidated computes the project list. Being handed one too is a contradiction, not a filter.
    val conflict = parse("compile", "--invalidated", "myapp").run(started)
    assert(conflict.left.exists(_.getMessage.contains("takes no project arguments")), s"got $conflict")
    succeed
  }

  private def twoProjectBuild(jvmName: String, resolver: Option[String]): String = {
    val lines = List(
      "$schema: https://raw.githubusercontent.com/oyvindberg/bleep/master/schema.json",
      "$version: dev",
      "jvm:",
      s"  name: $jvmName"
    ) ++ resolver.toList.flatMap(r => List("resolvers:", s"  - $r")) ++ List(
      "projects:",
      "  a: {}",
      "  b:",
      "    dependsOn: a",
      ""
    )
    lines.mkString("\n")
  }
}
