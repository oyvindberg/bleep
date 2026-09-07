package bleep.testing.runner;

import java.io.Flushable;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.function.Consumer;
import sbt.testing.*;

/**
 * Runs one test suite and reports it as encoded {@link TestProtocol} lines.
 *
 * <p>Everything about <em>how</em> a suite is run lives here: loading the framework, ordering its
 * fingerprints against what the class on disk actually is, executing the tasks it hands back, and
 * turning sbt-testing events into protocol messages. None of that knows where the lines go or which
 * classes it is looking at — a {@link Consumer} takes the output and a {@link ClassLoader} supplies
 * the classes.
 *
 * <p>Those two parameters are the whole reason this is not part of {@link ForkedTestRunner}. In a
 * fork, the sink writes to the parent's socket and the loader is the fork's own system loader. In
 * process, the sink pushes onto a queue and the loader is one built over the project's test
 * classpath. Same code, same wire format, same semantics — which is the point: a second
 * implementation of fingerprint ordering would be a second set of answers to "did this suite run",
 * and the two would drift.
 *
 * <p>No static mutable state, deliberately. In a fork there is one suite at a time; in process
 * there are as many as the DAG admits, concurrently, in this JVM. Anything static here would be
 * shared between them.
 */
public final class SuiteRunner {

  private final Consumer<String> sink;
  private final ClassLoader loader;

  /**
   * Streams to flush before a test result is reported, so captured output arrives ahead of the
   * event it belongs to. Empty in process, where there is nothing between the test and the console.
   */
  private final List<Flushable> toFlush;

  public SuiteRunner(Consumer<String> sink, ClassLoader loader, List<Flushable> toFlush) {
    this.sink = sink;
    this.loader = loader;
    this.toFlush = toFlush;
  }

  /**
   * Run the suite named by an encoded {@link TestProtocol} RunSuite command.
   *
   * <p>The entry point for a caller on the other side of a classloader boundary: every parameter
   * and the result are platform types, so nothing needs this class's own types to call it. An
   * in-process caller reaches this reflectively and decodes the lines the sink receives with the
   * same decoder the forked path uses.
   */
  public void runSerialized(String runSuiteCommandLine) {
    TestProtocol.ParsedCommand cmd = TestProtocol.parseCommand(runSuiteCommandLine);
    if (!(cmd instanceof TestProtocol.ParsedCommand.RunSuite)) {
      throw new IllegalArgumentException(
          "expected an encoded RunSuite command, got: " + runSuiteCommandLine);
    }
    TestProtocol.ParsedCommand.RunSuite runSuite = (TestProtocol.ParsedCommand.RunSuite) cmd;
    runSuite(
        runSuite.className,
        runSuite.framework,
        runSuite.runner.name(),
        runSuite.frameworkClass,
        runSuite.args);
  }

  private void flushAll() throws IOException {
    for (Flushable f : toFlush) f.flush();
  }

  public void runSuite(
      String className,
      String frameworkName,
      String runnerKind,
      String frameworkClass,
      List<String> args) {

    sink.accept(
        TestProtocol.encodeLog(
            // bleep talking to itself about which suite it was handed. Not the user's test output.
            "debug",
            "runSuite called: className=" + className + ", frameworkName=" + frameworkName));

    // The server decided this, with the project's classpath in front of it. Nothing here re-derives
    // it from frameworkName, which is a display label.
    if (TestProtocol.RunnerKind.JUNIT_PLATFORM.name().equals(runnerKind)) {
      JUnitPlatformRunner junitRunner = new JUnitPlatformRunner(sink, toFlush);
      junitRunner.runSuite(className);
      return;
    }

    Framework framework;
    Runner runner;
    try {
      flushAll();
      sink.accept(TestProtocol.encodeLog("debug", "Loading framework: " + frameworkClass));
      framework = loadFramework(frameworkClass);
      sink.accept(
          TestProtocol.encodeLog("debug", "Framework loaded: " + framework.getClass().getName()));
      runner = framework.runner(args.toArray(new String[0]), new String[0], loader);
    } catch (Throwable e) {
      sink.accept(TestProtocol.encodeLog("error", stackTraceToString(e)));
      sink.accept(
          TestProtocol.encodeSuiteErrored(
              className,
              0,
              "Error loading framework "
                  + frameworkClass
                  + ": "
                  + e.getClass().getName()
                  + ": "
                  + e.getMessage(),
              stackTraceToString(e)));
      return;
    }

    // One Runner, one done() — even for a single suite. The lifecycle lives in the caller so the
    // batch path ([[runSuites]]) can share one Runner across a project's suites, which is the sbt
    // interface's contract (one runner per framework per run) and what stateful frameworks need.
    try {
      runOneSuiteOn(framework, runner, frameworkName, className);
    } catch (InterruptedException e) {
      throw new RuntimeException(e);
    } finally {
      try {
        runner.done();
      } catch (Throwable ignored) {
        // done() is best-effort cleanup; a failure here must not mask the suite's own result.
      }
    }
  }

  /**
   * Run every suite in {@code classNames} through ONE {@link Runner} — maven surefire's {@code
   * forkCount=1 reuseForks=true}, and what sbt and mill do: {@link Framework#runner} once, all
   * suites through it, {@link Runner#done()} once. A fresh Runner (and done()) per suite — the
   * shape a shared fork used before — breaks frameworks that keep per-JVM state (munit, ZIO Test),
   * which set up on runner creation and tear down on done(); N of each in one JVM corrupted their
   * results.
   *
   * <p>Suites run sequentially, one at a time — surefire's {@code reuseForks=true}. sbt-interface
   * frameworks share one {@code Runner} and have no lock-aware scheduler, so bleep never runs their
   * suites concurrently in a shared fork; concurrency for them is a fork per suite (per-suite
   * mode). Each suite's events are attributed by the class name bound to it here. {@code
   * setCurrentSuite} lets the caller tag captured output with the suite running on the current
   * thread (null clears it).
   */
  public void runSuites(
      List<String> classNames,
      String frameworkName,
      String frameworkClass,
      List<String> args,
      Consumer<String> setCurrentSuite) {
    Framework framework;
    Runner runner;
    try {
      flushAll();
      framework = loadFramework(frameworkClass);
      runner = framework.runner(args.toArray(new String[0]), new String[0], loader);
    } catch (Throwable e) {
      sink.accept(TestProtocol.encodeLog("error", stackTraceToString(e)));
      for (String className : classNames) {
        sink.accept(
            TestProtocol.encodeSuiteErrored(
                className,
                0,
                "Error loading framework "
                    + frameworkClass
                    + ": "
                    + e.getClass().getName()
                    + ": "
                    + e.getMessage(),
                stackTraceToString(e)));
      }
      return;
    }

    try {
      for (String className : classNames) {
        if (Thread.interrupted()) break;
        setCurrentSuite.accept(className);
        try {
          runOneSuiteOn(framework, runner, frameworkName, className);
        } catch (InterruptedException e) {
          Thread.currentThread().interrupt();
          break;
        } finally {
          setCurrentSuite.accept(null);
        }
      }
    } finally {
      try {
        runner.done();
      } catch (Throwable ignored) {
        // best-effort cleanup
      }
      try {
        flushAll();
      } catch (IOException ignored) {
        // best-effort
      }
      // The batch terminator: the parent's runSuites stream ends on this (each suite's own
      // SuiteDone
      // has already gone by). Without it the parent waits forever for a run that has finished.
      sink.accept(TestProtocol.encodeBatchComplete());
    }
  }

  /**
   * Run one suite on an already-created {@link Runner}. Does NOT create the runner or call {@link
   * Runner#done()} — the caller owns that lifecycle so a batch can share one Runner across a
   * project's suites. Reports the suite's terminal outcome; a per-suite failure is reported and
   * swallowed so a batch's other suites still run, while an interruption propagates to stop the
   * run.
   */
  private void runOneSuiteOn(
      Framework framework, Runner runner, String frameworkName, String className)
      throws InterruptedException {
    long startTime = System.currentTimeMillis();

    // Counters declared outside try so they're accessible in catch for SuiteDone reporting
    final int[] passed = {0};
    final int[] failed = {0};
    final int[] skipped = {0};
    final int[] ignored = {0};

    try {
      // Try each fingerprint from the framework until we find one that produces tasks.
      // Different fingerprints match different test patterns (e.g. @Test annotation vs
      // TestCase subclass), so we need to find the right one for this class.
      Fingerprint[] fingerprints = framework.fingerprints();

      if (fingerprints.length == 0) {
        sink.accept(
            TestProtocol.encodeError("Framework has no fingerprints: " + frameworkName, null));
        return;
      }

      Task[] tasks = null;

      // Try fingerprints that agree with what the class actually is before the rest.
      //
      // "First fingerprint that yields a task" is not enough on its own. A framework that declares
      // both a class and a module fingerprint — specs2 does — is
      // free to hand back a task for either without checking, and only fails later when it tries to
      // load the form that does not exist: a `class Fixture extends
      // Specification` matched against the module fingerprint produced a task whose whole error
      // message was "example.Specs2Fixture$". Whether a suite is a
      // Scala object is not a guess; the compiler emits `Fixture$` for one and not for the other.
      Fingerprint[] ordered = orderFingerprintsFor(className, fingerprints);

      for (Fingerprint fingerprint : ordered) {
        TaskDef taskDef =
            new TaskDef(className, fingerprint, true, new Selector[] {new SuiteSelector()});
        Task[] candidate = runner.tasks(new TaskDef[] {taskDef});
        if (candidate.length > 0) {
          tasks = candidate;
          sink.accept(
              TestProtocol.encodeLog(
                  "debug", "Matched fingerprint: " + describeFingerprint(fingerprint)));
          break;
        }
      }

      if (tasks == null || tasks.length == 0) {
        // No fingerprint produced a task: the loaded framework does not recognize this class as
        // a suite. Not an empty suite (the framework never claimed it) — a framework mismatch.
        sink.accept(
            TestProtocol.encodeSuiteNoFrameworkMatched(
                className,
                System.currentTimeMillis() - startTime,
                "No test framework recognized " + className + " as a suite"));
        return;
      }

      // Custom event handler to capture test events
      EventHandler eventHandler =
          new EventHandler() {
            @Override
            public void handle(Event event) {
              String status;
              switch (event.status()) {
                case Success:
                  status = "passed";
                  passed[0]++;
                  break;
                case Failure:
                  status = "failed";
                  failed[0]++;
                  break;
                case Error:
                  status = "error";
                  failed[0]++;
                  break;
                case Skipped:
                  status = "skipped";
                  skipped[0]++;
                  break;
                case Ignored:
                  status = "ignored";
                  ignored[0]++;
                  break;
                case Canceled:
                  status = "assumption-failed";
                  skipped[0]++;
                  break;
                case Pending:
                  status = "pending";
                  ignored[0]++;
                  break;
                default:
                  status = "unknown";
                  break;
              }

              String throwableStr = null;
              String message = null;
              StackTraceElement location = null;
              if (event.throwable() != null && event.throwable().isDefined()) {
                Throwable t = event.throwable().get();
                message = t.getMessage();
                throwableStr = stackTraceToString(t);
                location = failureLocation(t, className);
              }

              // Extract test name from selector if available
              String testName = extractTestName(event);

              // Flush output before reporting test finished
              try {
                flushAll();
              } catch (IOException e) {
                // Ignore
              }

              sink.accept(
                  TestProtocol.encodeTestFinished(
                      className,
                      testName,
                      status,
                      event.duration(),
                      message,
                      throwableStr,
                      location == null ? null : location.getClassName(),
                      location == null ? null : location.getFileName(),
                      location == null ? 0 : location.getLineNumber()));
            }
          };

      // Execute tasks
      Logger logger = createLogger(className);
      executeTasks(tasks, eventHandler, new Logger[] {logger});

      // NB: no runner.done() here — the caller owns the Runner's lifecycle and calls done() once,
      // after all of a batch's suites, per the sbt interface's one-runner-per-framework contract.

      // Final flush
      flushAll();

      long durationMs = System.currentTimeMillis() - startTime;
      int total = passed[0] + failed[0] + skipped[0] + ignored[0];
      if (total == 0) {
        // The framework claimed the class (a task ran) but no test fired an event: an empty suite.
        sink.accept(TestProtocol.encodeSuiteEmpty(className, durationMs));
      } else {
        sink.accept(
            TestProtocol.encodeSuiteExecuted(
                className, passed[0], failed[0], skipped[0], ignored[0], durationMs));
      }

    } catch (InterruptedException e) {
      // Cancelled — report and propagate so a batch stops rather than starting its next suite.
      sink.accept(TestProtocol.encodeLog("warn", "Suite " + className + " was cancelled"));
      throw e;
    } catch (SecurityException e) {
      if (e.getMessage() != null && e.getMessage().contains("System.exit")) {
        sink.accept(
            TestProtocol.encodeSuiteErrored(
                className,
                System.currentTimeMillis() - startTime,
                "Test attempted a blocked System.exit",
                null));
      } else {
        // Report and swallow (do not abort a batch's siblings); a non-exit SecurityException here
        // is unexpected, so it is surfaced as this suite's error rather than rethrown.
        sink.accept(TestProtocol.encodeLog("error", stackTraceToString(e)));
        sink.accept(
            TestProtocol.encodeSuiteErrored(
                className,
                System.currentTimeMillis() - startTime,
                "Error running suite "
                    + className
                    + ": "
                    + e.getClass().getName()
                    + ": "
                    + e.getMessage(),
                stackTraceToString(e)));
      }
    } catch (Throwable e) {
      // Must catch Throwable (not just Exception): a framework may let an Error (AssertionError,
      // or a LinkageError propagated from executeTasks) escape. Report it as an errored suite —
      // NOT SuiteExecuted with faked counts — so the outcome carries the real reason.
      sink.accept(TestProtocol.encodeLog("error", stackTraceToString(e)));
      Throwable reported =
          (e instanceof SuiteExecutionException && e.getCause() != null) ? e.getCause() : e;
      sink.accept(
          TestProtocol.encodeSuiteErrored(
              className,
              System.currentTimeMillis() - startTime,
              "Error running suite "
                  + className
                  + ": "
                  + reported.getClass().getName()
                  + ": "
                  + reported.getMessage(),
              stackTraceToString(reported)));
    }
  }

  private void executeTasks(Task[] tasks, EventHandler eventHandler, Logger[] loggers)
      throws InterruptedException {
    for (Task task : tasks) {
      // Check for interruption before each task
      if (Thread.interrupted()) {
        throw new InterruptedException();
      }

      try {
        Task[] nestedTasks = task.execute(eventHandler, loggers);
        // Recursively execute nested tasks
        executeTasks(nestedTasks, eventHandler, loggers);
      } catch (InterruptedException e) {
        throw e;
      } catch (Throwable e) {
        // Do NOT swallow and continue. A Throwable escaping task.execute — LinkageError,
        // NoClassDefFoundError, ExceptionInInitializerError, typically from a stale sibling
        // compile — means this suite's classpath cannot be trusted, not that one test failed.
        // Swallowing it here let the suite fall through to SuiteDone(...,0,0,0) and be reported
        // PASSED: a green build over a suite that never ran. Propagate so the caller's handler
        // records a real failure with a non-zero count.
        throw new SuiteExecutionException(e);
      }
    }
  }

  /**
   * Wraps a non-interruption Throwable that escaped {@code task.execute} so it propagates out of
   * {@link #executeTasks} (whose only checked throw is InterruptedException) to runSuite's outer
   * handler, which reports it as a suite failure.
   */
  private static final class SuiteExecutionException extends RuntimeException {
    SuiteExecutionException(Throwable cause) {
      super(cause);
    }
  }

  /**
   * Instantiate an sbt.testing.Framework by class name.
   *
   * <p>One line, because the server sends the class rather than a label to guess from. This used to
   * special-case JUnit, Kotest and TestNG, probe lists of candidate classes, and fall back to
   * treating the display name as a class name — which is how "Spock" and "kotlin.test" arrived at
   * Class.forName verbatim.
   */
  private Framework loadFramework(String frameworkClass) throws Exception {
    Class<?> clazz = loader.loadClass(frameworkClass);
    return (Framework) clazz.getDeclaredConstructor().newInstance();
  }

  /** Check if this framework should use JUnit Platform Launcher directly. */
  private Logger createLogger(final String suiteName) {
    return new Logger() {
      @Override
      public boolean ansiCodesSupported() {
        // Frameworks ask this before colourising. Answering an unconditional `true` meant `bleep
        // test --no-color` still got ANSI escapes from ScalaTest,
        // hedgehog and friends: the flag lives in the client JVM and this runs in a forked one, so
        // the only thing that crosses is the environment. The
        // client sets NO_COLOR when the user asks for no colour, and the no-color.org convention
        // means a user who sets it themselves is honoured too.
        String noColor = System.getenv("NO_COLOR");
        return noColor == null || noColor.isEmpty();
      }

      @Override
      public void error(String msg) {
        sink.accept(TestProtocol.encodeLog("error", msg));
      }

      @Override
      public void warn(String msg) {
        sink.accept(TestProtocol.encodeLog("warn", msg));
      }

      @Override
      public void info(String msg) {
        sink.accept(TestProtocol.encodeLog("info", msg));
      }

      @Override
      public void debug(String msg) {
        sink.accept(TestProtocol.encodeLog("debug", msg));
      }

      @Override
      public void trace(Throwable t) {
        sink.accept(TestProtocol.encodeLog("error", stackTraceToString(t)));
      }
    };
  }

  /**
   * Puts fingerprints whose `isModule` matches the class on disk first, keeping the framework's own
   * order within each group. Nothing is discarded: a framework that disagrees with this reading
   * still gets every fingerprint tried, just second.
   */
  private Fingerprint[] orderFingerprintsFor(String className, Fingerprint[] fingerprints) {
    Class<?> asModule = loadClass(className + "$");
    Class<?> asPlain = loadClass(className);
    boolean isModule = asModule != null;

    // Ranked, highest first, keeping the framework's own order within a rank:
    //   2 — the class really does extend what the fingerprint names
    //   1 — only the class/object shape agrees
    //   0 — neither
    //
    // Shape alone is not enough to tell a framework's fingerprints apart when several describe
    // objects. Weaver declares one for suites and another for global
    // resources; picking by shape chose the resource one and the run died with
    // "example.WeaverFixture$ is not an instance of weaver.IOGlobalResource". What the
    // class extends is the question the fingerprint is actually asking, so ask that first.
    List<List<Fingerprint>> byRank = new ArrayList<>();
    for (int i = 0; i < 3; i++) byRank.add(new ArrayList<>());
    for (Fingerprint fp : fingerprints) {
      Boolean declaredModule = fingerprintIsModule(fp);
      boolean shapeAgrees = declaredModule != null && declaredModule == isModule;
      // Each fingerprint is checked against the class it is talking about: a module fingerprint
      // means `Foo$`, a class fingerprint means `Foo`. Checking both against the object's class
      // scored a class fingerprint naming `org.scalacheck.Properties` just as highly as the module
      // one — `Foo$` extends Properties either way — and picking it made ScalaCheck unrunnable.
      // A Scala 3 mirror class extends nothing, so the wrong shape now scores itself out.
      Class<?> meant = (declaredModule != null && declaredModule) ? asModule : asPlain;
      boolean extendsIt =
          meant != null
              && fingerprintSuperclass(fp).map(sup -> sup.isAssignableFrom(meant)).orElse(false);
      int rank = extendsIt ? 2 : (shapeAgrees ? 1 : 0);
      byRank.get(2 - rank).add(fp);
    }

    List<Fingerprint> ordered = new ArrayList<>();
    for (List<Fingerprint> rank : byRank) ordered.addAll(rank);
    return ordered.toArray(new Fingerprint[0]);
  }

  /** The class a SubclassFingerprint names, when it names one and it can be loaded. */
  private Optional<Class<?>> fingerprintSuperclass(Fingerprint fp) {
    if (!(fp instanceof SubclassFingerprint)) return Optional.empty();
    return Optional.ofNullable(loadClass(((SubclassFingerprint) fp).superclassName()));
  }

  private Class<?> loadClass(String name) {
    try {
      return Class.forName(name, false, loader);
    } catch (ClassNotFoundException | LinkageError e) {
      return null;
    }
  }

  /** Null when the fingerprint kind says nothing about module-ness. */
  private Boolean fingerprintIsModule(Fingerprint fp) {
    if (fp instanceof SubclassFingerprint) return ((SubclassFingerprint) fp).isModule();
    if (fp instanceof AnnotatedFingerprint) return ((AnnotatedFingerprint) fp).isModule();
    return null;
  }

  private String describeFingerprint(Fingerprint fp) {
    if (fp instanceof SubclassFingerprint) {
      SubclassFingerprint sfp = (SubclassFingerprint) fp;
      return "SubclassFingerprint(" + sfp.superclassName() + ", isModule=" + sfp.isModule() + ")";
    } else if (fp instanceof AnnotatedFingerprint) {
      AnnotatedFingerprint afp = (AnnotatedFingerprint) fp;
      return "AnnotatedFingerprint(" + afp.annotationName() + ", isModule=" + afp.isModule() + ")";
    }
    return fp.toString();
  }

  public static String stackTraceToString(Throwable t) {
    StringWriter sw = new StringWriter();
    t.printStackTrace(new PrintWriter(sw));
    return sw.toString();
  }

  /**
   * The first stack frame belonging to the suite class itself, which is where the failing assertion
   * lives for every framework we support — the frames above it are inside the assertion library.
   *
   * <p>Deliberately not "the first frame with a line number": that points at someone else's source,
   * and an annotation on the wrong file is worse than no annotation. Returns null when the
   * throwable has no frame in the suite, which is normal for a failure thrown from a helper or a
   * fixture.
   *
   * <p>Inner and anonymous classes ({@code MyTest$$anon$1}) still belong to the suite, so match on
   * the {@code $} boundary rather than equality alone. Causes are walked because assertion
   * libraries routinely wrap.
   */
  private StackTraceElement failureLocation(Throwable t, String suiteClass) {
    for (Throwable current = t; current != null; current = current.getCause()) {
      for (StackTraceElement frame : current.getStackTrace()) {
        String cn = frame.getClassName();
        boolean inSuite = cn.equals(suiteClass) || cn.startsWith(suiteClass + "$");
        if (inSuite && frame.getFileName() != null && frame.getLineNumber() > 0) {
          return frame;
        }
      }
      if (current.getCause() == current) break; // self-referential cause, seen in the wild
    }
    return null;
  }

  /**
   * Extract the test name from an event. Tries to get the test method name from the selector, falls
   * back to fullyQualifiedName.
   */
  private String extractTestName(Event event) {
    Selector selector = event.selector();

    if (selector instanceof TestSelector) {
      // TestSelector contains the test method name
      return ((TestSelector) selector).testName();
    } else if (selector instanceof NestedTestSelector) {
      // NestedTestSelector for nested tests
      return ((NestedTestSelector) selector).testName();
    } else {
      // Fall back to fully qualified name for suite-level events
      return event.fullyQualifiedName();
    }
  }
}
