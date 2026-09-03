package bleep.testing.runner;

import static org.junit.platform.engine.discovery.DiscoverySelectors.selectClass;

import java.io.Flushable;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Consumer;
import org.junit.platform.engine.TestExecutionResult;
import org.junit.platform.engine.reporting.ReportEntry;
import org.junit.platform.engine.support.descriptor.ClassSource;
import org.junit.platform.engine.support.descriptor.MethodSource;
import org.junit.platform.launcher.Launcher;
import org.junit.platform.launcher.LauncherDiscoveryRequest;
import org.junit.platform.launcher.TestExecutionListener;
import org.junit.platform.launcher.TestIdentifier;
import org.junit.platform.launcher.TestPlan;
import org.junit.platform.launcher.core.LauncherDiscoveryRequestBuilder;
import org.junit.platform.launcher.core.LauncherFactory;

/**
 * Runs JUnit 5 tests via JUnit Platform Launcher directly, bypassing sbt test-interface.
 *
 * <p>This enables proper JUnit Platform lifecycle including LauncherSessionListener SPI, which is
 * required for frameworks like Quarkus that set up custom classloaders (FacadeClassLoader) during
 * session initialization.
 *
 * <p>Using openSession() instead of create() triggers: - Quarkus's CustomLauncherInterceptor →
 * FacadeClassLoader as TCCL - Spring Boot's test context management - Any other
 * LauncherSessionListener implementations
 */
class JUnitPlatformRunner {

  /** Fully-qualified name of the session interface, absent before JUnit Platform 1.8. */
  private static final String LAUNCHER_SESSION = "org.junit.platform.launcher.LauncherSession";

  private final Consumer<String> sink;

  /**
   * Streams to flush before a result is reported. Empty in process; the fork's captured pair
   * otherwise.
   */
  private final List<Flushable> toFlush;

  JUnitPlatformRunner(Consumer<String> sink, List<Flushable> toFlush) {
    this.sink = sink;
    this.toFlush = toFlush;
  }

  /** A {@link Launcher} plus whatever has to be closed afterwards. */
  private static final class LauncherHandle implements AutoCloseable {
    final Launcher launcher;

    /** The {@code LauncherSession}, or null on a platform that predates the concept. */
    private final AutoCloseable session;

    LauncherHandle(Launcher launcher, AutoCloseable session) {
      this.launcher = launcher;
      this.session = session;
    }

    @Override
    public void close() throws Exception {
      if (session != null) {
        session.close();
      }
    }
  }

  // ---- Shared-session mode (set by a fork, off in-process) ----
  //
  // In a forked JVM every suite shares ONE LauncherSession, so a LauncherSessionListener fires once
  // for the whole fork rather than once per suite (and, with concurrent suites, N racing opens).
  // The
  // session is opened lazily by the first JUnit suite and closed at fork shutdown. In-process
  // leaves
  // this off: there the daemon JVM runs many projects' suites under different classloaders, and one
  // shared session across them would be wrong — each opens its own, as before.
  private static volatile boolean SHARE_SESSION = false;
  private static volatile LauncherHandle SHARED_HANDLE = null;
  private static final Object SHARED_LOCK = new Object();

  /** Turn on one-session-per-JVM. Called once by a fork at startup, before any suite runs. */
  static void enableSharedSession() {
    SHARE_SESSION = true;
  }

  /**
   * Close the shared session, if one was opened. Called once at fork shutdown; runs
   * launcherSessionClosed SPI.
   */
  static void closeSharedSession() {
    synchronized (SHARED_LOCK) {
      if (SHARED_HANDLE != null) {
        try {
          SHARED_HANDLE.close();
        } catch (Exception ignored) {
          // Shutdown path: nothing useful to do if the session will not close.
        }
        SHARED_HANDLE = null;
      }
    }
  }

  /** The one shared launcher, opened on first use under lock. */
  private LauncherHandle sharedHandle() {
    LauncherHandle h = SHARED_HANDLE;
    if (h != null) return h;
    synchronized (SHARED_LOCK) {
      if (SHARED_HANDLE == null) SHARED_HANDLE = openLauncher();
      return SHARED_HANDLE;
    }
  }

  /**
   * Obtain a launcher, using the session lifecycle when the platform on the classpath has one.
   *
   * <p>This class is compiled against the oldest launcher API bleep supports and executed against
   * whatever version the project resolved — bleep injects the launcher at the project's own
   * platform version rather than overriding a choice the project made. {@code
   * LauncherFactory.openSession()} arrived in 1.8, so calling it directly made every project on
   * Jupiter 5.7 or older (Spring Boot 2.5 and earlier pin exactly that) die here with
   * NoSuchMethodError. Reflection is what lets one compiled runner span the whole range.
   *
   * <p>The no-session path is not a degraded fallback: before 1.8 there is no
   * LauncherSessionListener SPI at all, so there is nothing to miss. Only that one condition — the
   * type or the method being absent — is treated as "this platform is older". Every other
   * reflective failure is a real bug and is rethrown.
   */
  private LauncherHandle openLauncher() {
    Class<?> sessionType;
    Method openSession;
    try {
      sessionType = Class.forName(LAUNCHER_SESSION, false, LauncherFactory.class.getClassLoader());
      openSession = LauncherFactory.class.getMethod("openSession");
    } catch (ClassNotFoundException | NoSuchMethodException pre18) {
      send(
          TestProtocol.encodeLog(
              "debug",
              "JUnit Platform predates LauncherSession (1.8); using LauncherFactory.create()."
                  + " LauncherSessionListener extensions (Quarkus, Spring Boot) do not exist on"
                  + " this version."));
      return new LauncherHandle(LauncherFactory.create(), null);
    }

    try {
      Object session = openSession.invoke(null);
      // Look the method up on the *interface*: the implementation returned here is a
      // package-private class, so a method resolved from session.getClass() cannot be invoked.
      Launcher launcher = (Launcher) sessionType.getMethod("getLauncher").invoke(session);
      return new LauncherHandle(launcher, (AutoCloseable) session);
    } catch (InvocationTargetException e) {
      Throwable cause = e.getCause();
      if (cause instanceof RuntimeException) throw (RuntimeException) cause;
      if (cause instanceof Error) throw (Error) cause;
      throw new RuntimeException("LauncherFactory.openSession() failed", cause);
    } catch (ReflectiveOperationException e) {
      throw new RuntimeException("could not open a LauncherSession on a platform that has one", e);
    }
  }

  /**
   * Run a single test class using JUnit Platform Launcher with full session lifecycle.
   *
   * @param className fully qualified test class name
   */
  void runSuite(String className) {

    long startTime = System.currentTimeMillis();
    String currentSuite = className;

    send(
        // These four describe how bleep decided to run the suite — the launcher it chose and the
        // plan it built. Useful when debugging bleep, noise in
        // front of a user trying to read why their test failed, so they travel at debug level and
        // reach the daemon log rather than the report.
        TestProtocol.encodeLog(
            "debug", "Using JUnit Platform Launcher directly for: " + className));

    int[] passed = {0};
    int[] failed = {0};
    int[] skipped = {0};
    int[] ignored = {0};
    Map<String, Long> testStartNanos = new ConcurrentHashMap<>();

    try {
      // Flush any pending output
      flushAll();

      // A LauncherSession is where LauncherSessionListener SPI fires (the SmallRye/Mutiny
      // registrar,
      // Quarkus's CustomLauncherInterceptor that builds the FacadeClassLoader). In fork mode one
      // session is opened for the whole JVM and shared by every suite, so those listeners fire
      // exactly ONCE — maven surefire's one-session-per-fork semantics. That is the difference
      // between a listener that pre-registers a global once and N concurrent suites each racing to
      // register it. Per-suite otherwise (in-process, or a platform predating sessions).
      boolean shareSession = SHARE_SESSION;
      LauncherHandle handle = shareSession ? sharedHandle() : openLauncher();
      try {
        Launcher launcher = handle.launcher;

        LauncherDiscoveryRequest request =
            LauncherDiscoveryRequestBuilder.request().selectors(selectClass(className)).build();

        TestExecutionListener listener =
            new TestExecutionListener() {
              @Override
              public void testPlanExecutionStarted(TestPlan testPlan) {
                long count = testPlan.countTestIdentifiers(t -> t.isTest());
                send(
                    TestProtocol.encodeLog(
                        "debug",
                        "TestPlan started: "
                            + count
                            + " test(s) in plan, roots="
                            + testPlan.getRoots().size()));
                for (TestIdentifier root : testPlan.getRoots()) {
                  send(
                      TestProtocol.encodeLog(
                          "debug",
                          "  Root: " + root.getDisplayName() + " [" + root.getUniqueId() + "]"));
                  for (TestIdentifier child : testPlan.getChildren(root)) {
                    send(
                        TestProtocol.encodeLog(
                            "debug",
                            "    Child: "
                                + child.getDisplayName()
                                + " isTest="
                                + child.isTest()
                                + " ["
                                + child.getType()
                                + "]"));
                  }
                }
              }

              @Override
              public void executionStarted(TestIdentifier testIdentifier) {
                if (testIdentifier.isTest() && !isChildlessVintageClass(testIdentifier)) {
                  testStartNanos.put(testIdentifier.getUniqueId(), System.nanoTime());
                  String testName = testIdentifier.getDisplayName();
                  send(TestProtocol.encodeTestStarted(currentSuite, testName));
                }
              }

              @Override
              public void executionFinished(
                  TestIdentifier testIdentifier, TestExecutionResult result) {
                if (!testIdentifier.isTest()) {
                  // A container fails on its own whenever the failure is not attributable to any
                  // one test: @AfterClass/@AfterAll/@BeforeClass/@BeforeAll throwing, a class-level
                  // rule, a @Parameters method blowing up, an engine dying. Dropping these reported
                  // a suite whose teardown asserted as green, with exit 0.
                  if (result.getStatus() == TestExecutionResult.Status.FAILED) {
                    reportContainerFailure(testIdentifier, result);
                  }
                  return;
                }

                String testName = testIdentifier.getDisplayName();
                long durationMs = elapsedMs(testStartNanos.remove(testIdentifier.getUniqueId()));

                String status;
                String message = null;
                String throwableStr = null;

                switch (result.getStatus()) {
                  case SUCCESSFUL:
                    if (isChildlessVintageClass(testIdentifier)) {
                      // Count nothing, so the suite finishes with zero events and is reported as
                      // Empty rather than as one green test. See isChildlessVintageClass.
                      send(
                          TestProtocol.encodeLog(
                              "warn",
                              currentSuite
                                  + " has no runnable tests — JUnit 4 reported the class itself as"
                                  + " a leaf. An empty @Parameters list or @SuiteClasses({}) does"
                                  + " this."));
                      return;
                    }
                    status = "passed";
                    passed[0]++;
                    break;
                  case FAILED:
                    status = "failed";
                    failed[0]++;
                    if (result.getThrowable().isPresent()) {
                      Throwable t = result.getThrowable().get();
                      message = t.getMessage();
                      throwableStr = stackTraceToString(t);
                    }
                    break;
                  case ABORTED:
                    status = "skipped";
                    skipped[0]++;
                    if (result.getThrowable().isPresent()) {
                      message = result.getThrowable().get().getMessage();
                    }
                    break;
                  default:
                    status = "unknown";
                    break;
                }

                // Flush output before reporting
                try {
                  flushAll();
                } catch (Exception e) {
                  // Ignore
                }

                send(
                    TestProtocol.encodeTestFinished(
                        currentSuite, testName, status, durationMs, message, throwableStr));
              }

              /**
               * True when the platform handed us the requested class itself as a leaf "test".
               *
               * <p>JUnit 4's {@code Description.isTest()} means only "I have no children", so a
               * class that produced no runnable tests — an empty {@code @Parameters} list,
               * {@code @SuiteClasses({})} — arrives through the vintage engine as a single leaf
               * whose unique id ends in {@code [runner:<the class we asked for>]} instead of the
               * usual {@code [test:method(class)]}. Reporting it as one passed test made a suite
               * that ran nothing look exactly like a green one.
               */
              private boolean isChildlessVintageClass(TestIdentifier testIdentifier) {
                return testIdentifier.getUniqueId().endsWith("[runner:" + currentSuite + "]");
              }

              /**
               * Surface a failed container as a synthetic failed test, so it lands in the counts,
               * in the failures section, and in the exit code like any other failure.
               */
              private void reportContainerFailure(
                  TestIdentifier testIdentifier, TestExecutionResult result) {
                String testName = containerTestName(testIdentifier);
                String message = null;
                String throwableStr = null;
                if (result.getThrowable().isPresent()) {
                  Throwable t = result.getThrowable().get();
                  message = t.getMessage();
                  throwableStr = stackTraceToString(t);
                }
                failed[0]++;

                try {
                  flushAll();
                } catch (Exception e) {
                  // Ignore
                }

                // Started/finished as a pair: the reader counts started tests, and a finish without
                // a start leaves its running-test bookkeeping short.
                send(TestProtocol.encodeTestStarted(currentSuite, testName));
                send(
                    TestProtocol.encodeTestFinished(
                        currentSuite, testName, "failed", 0, message, throwableStr));
              }

              @Override
              public void executionSkipped(TestIdentifier testIdentifier, String reason) {
                if (!testIdentifier.isTest()) {
                  // A skipped container (@Disabled/@Ignore on the class) reports nothing for the
                  // tests underneath it, so without this the suite finishes having emitted zero
                  // events and is reported as an empty — i.e. failed — suite.
                  if (!testIdentifier.getParentId().isPresent()) return;
                  String containerName = containerTestName(testIdentifier);
                  skipped[0]++;
                  send(
                      TestProtocol.encodeTestFinished(
                          currentSuite, containerName, "skipped", 0, reason, null));
                  return;
                }
                String testName = testIdentifier.getDisplayName();
                skipped[0]++;
                send(
                    TestProtocol.encodeTestFinished(
                        currentSuite, testName, "skipped", 0, reason, null));
              }

              @Override
              public void reportingEntryPublished(
                  TestIdentifier testIdentifier, ReportEntry entry) {
                // Forward as log output
                send(TestProtocol.encodeLog("info", entry.toString()));
              }
            };

        // Discover before executing. If NO engine claims this class at all — the classic case is a
        // JUnit 4 (@org.junit.Test) class routed here with junit-platform present but no
        // junit-vintage-engine — no engine contributes a root, and execute() silently runs nothing.
        // Report NoFrameworkMatched, not a green pass.
        //
        // Test on getRoots() (engine descriptors), NOT countTestIdentifiers(isTest): dynamic
        // frameworks like Kotest register their engine root here but report zero *test* identifiers
        // until execution registers them, so an isTest count of 0 at discovery is a false negative.
        TestPlan plan = launcher.discover(request);
        if (plan.getRoots().isEmpty()) {
          send(
              TestProtocol.encodeSuiteNoFrameworkMatched(
                  className,
                  System.currentTimeMillis() - startTime,
                  "No JUnit Platform engine claimed "
                      + className
                      + ". A JUnit 4 test class needs junit-vintage-engine on the test"
                      + " classpath."));
          return;
        }

        launcher.execute(request, listener);
      } finally {
        // A shared session is closed once, at fork shutdown; a per-suite one is this suite's to
        // close.
        if (!shareSession) handle.close();
      }

      // Flush and report done
      flushAll();

      long durationMs = System.currentTimeMillis() - startTime;
      int total = passed[0] + failed[0] + skipped[0] + ignored[0];
      if (total == 0) {
        // An engine claimed the class but ran no tests — an empty suite (or a container that
        // registered nothing). Still not a pass.
        send(TestProtocol.encodeSuiteEmpty(className, durationMs));
      } else {
        send(
            TestProtocol.encodeSuiteExecuted(
                className, passed[0], failed[0], skipped[0], ignored[0], durationMs));
      }

    } catch (Throwable e) {
      send(TestProtocol.encodeLog("error", stackTraceToString(e)));
      send(
          TestProtocol.encodeSuiteErrored(
              className,
              System.currentTimeMillis() - startTime,
              "Error in JUnit Platform runner for "
                  + className
                  + ": "
                  + e.getClass().getName()
                  + ": "
                  + e.getMessage(),
              stackTraceToString(e)));
    }
  }

  /**
   * Run a whole set of classes in ONE JUnit Platform execution, at a bleep-chosen degree of
   * parallelism.
   *
   * <p>This is the shape that makes an execution-scoped fixture — a {@code @QuarkusTest}
   * application above all — build once and be reused by every class, exactly as it is under maven
   * surefire's one-execute-per-module. Per-class results are still reported: the listener
   * attributes each test and container to the requested class it belongs to (a {@code @Nested
   * Foo$Bar} test back to {@code Foo}) and sends that class's own SuiteDone when its container
   * finishes, so the parent demultiplexes per suite as before. Parallelism is bleep's: the
   * configuration parameters set here override any {@code junit-platform.properties} on the
   * classpath, so junit's engine runs exactly the number of classes at once that bleep decided (1
   * serialises — what {@code @QuarkusTest} requires).
   */
  void runSuites(List<String> classNames, int parallelism) {
    long batchStart = System.currentTimeMillis();
    Set<String> requested = new LinkedHashSet<>(classNames);
    Map<String, int[]> counts =
        new ConcurrentHashMap<>(); // per class: [passed, failed, skipped, ignored]
    Map<String, Long> startedAt = new ConcurrentHashMap<>();
    Map<String, Long> testStartNanos = new ConcurrentHashMap<>();
    Set<String> reported = ConcurrentHashMap.newKeySet();
    for (String c : requested) counts.put(c, new int[4]);

    // Emit a class's terminal exactly once. Idempotent via `reported.add`. Empty (nothing ran) is
    // not a pass, matching runSuite.
    Consumer<String> emitDone =
        cls -> {
          if (!reported.add(cls)) return;
          int[] c = counts.getOrDefault(cls, new int[4]);
          long dur = System.currentTimeMillis() - startedAt.getOrDefault(cls, batchStart);
          if (c[0] + c[1] + c[2] + c[3] == 0) send(TestProtocol.encodeSuiteEmpty(cls, dur));
          else send(TestProtocol.encodeSuiteExecuted(cls, c[0], c[1], c[2], c[3], dur));
        };

    try {
      flushAll();
      boolean shareSession = SHARE_SESSION;
      LauncherHandle handle = shareSession ? sharedHandle() : openLauncher();
      try {
        Launcher launcher = handle.launcher;

        LauncherDiscoveryRequestBuilder builder = LauncherDiscoveryRequestBuilder.request();
        for (String c : classNames) builder.selectors(selectClass(c));
        boolean parallel = parallelism > 1;
        // bleep decides the degree; these override any junit-platform.properties the project ships,
        // so the number is ours, not junit's.
        builder.configurationParameter(
            "junit.jupiter.execution.parallel.enabled", Boolean.toString(parallel));
        builder.configurationParameter("junit.jupiter.execution.parallel.config.strategy", "fixed");
        builder.configurationParameter(
            "junit.jupiter.execution.parallel.config.fixed.parallelism",
            Integer.toString(Math.max(1, parallelism)));
        builder.configurationParameter(
            "junit.jupiter.execution.parallel.mode.classes.default", "concurrent");
        builder.configurationParameter(
            "junit.jupiter.execution.parallel.mode.default", "same_thread");
        LauncherDiscoveryRequest request = builder.build();

        TestExecutionListener listener =
            new TestExecutionListener() {
              private String suiteOf(TestIdentifier id) {
                String cls = classOf(id);
                if (cls == null) return null;
                if (requested.contains(cls)) return cls;
                for (String r : requested)
                  if (cls.startsWith(r + "$")) return r; // @Nested Foo$Bar -> Foo
                return null;
              }

              @Override
              public void executionStarted(TestIdentifier id) {
                String suite = suiteOf(id);
                if (suite == null) return;
                startedAt.putIfAbsent(suite, System.currentTimeMillis());
                if (id.isTest()) {
                  testStartNanos.put(id.getUniqueId(), System.nanoTime());
                  send(TestProtocol.encodeTestStarted(suite, id.getDisplayName()));
                }
              }

              @Override
              public void executionFinished(TestIdentifier id, TestExecutionResult result) {
                String suite = suiteOf(id);
                if (id.isTest()) {
                  if (suite == null) return;
                  int[] c = counts.get(suite);
                  String status;
                  String message = null;
                  String throwableStr = null;
                  switch (result.getStatus()) {
                    case SUCCESSFUL:
                      status = "passed";
                      c[0]++;
                      break;
                    case FAILED:
                      status = "failed";
                      c[1]++;
                      if (result.getThrowable().isPresent()) {
                        message = result.getThrowable().get().getMessage();
                        throwableStr = stackTraceToString(result.getThrowable().get());
                      }
                      break;
                    case ABORTED:
                      status = "skipped";
                      c[2]++;
                      if (result.getThrowable().isPresent())
                        message = result.getThrowable().get().getMessage();
                      break;
                    default:
                      status = "unknown";
                      break;
                  }
                  try {
                    flushAll();
                  } catch (Exception e) {
                    // ignore
                  }
                  long durationMs = elapsedMs(testStartNanos.remove(id.getUniqueId()));
                  send(
                      TestProtocol.encodeTestFinished(
                          suite, id.getDisplayName(), status, durationMs, message, throwableStr));
                } else {
                  // A failed container (@AfterAll/@BeforeAll, a class rule) is a failure not
                  // attributable to one test — surface it as a synthetic one.
                  if (result.getStatus() == TestExecutionResult.Status.FAILED && suite != null) {
                    counts.get(suite)[1]++;
                    String message = result.getThrowable().map(Throwable::getMessage).orElse(null);
                    String throwableStr =
                        result
                            .getThrowable()
                            .map(JUnitPlatformRunner::stackTraceToString)
                            .orElse(null);
                    String name = containerTestName(id);
                    send(TestProtocol.encodeTestStarted(suite, name));
                    send(
                        TestProtocol.encodeTestFinished(
                            suite, name, "failed", 0, message, throwableStr));
                  }
                  // The requested class's own container finishing is that suite's terminal.
                  String cls = classOf(id);
                  if (cls != null && requested.contains(cls)) emitDone.accept(cls);
                }
              }

              @Override
              public void executionSkipped(TestIdentifier id, String reason) {
                String suite = suiteOf(id);
                if (suite == null) return;
                startedAt.putIfAbsent(suite, System.currentTimeMillis());
                if (id.isTest()) {
                  counts.get(suite)[2]++;
                  send(
                      TestProtocol.encodeTestFinished(
                          suite, id.getDisplayName(), "skipped", 0, reason, null));
                } else if (requested.contains(classOf(id))) {
                  // A whole class disabled: no tests will fire, so this is its terminal — an empty
                  // (nothing ran) suite.
                  emitDone.accept(classOf(id));
                }
              }

              @Override
              public void reportingEntryPublished(TestIdentifier id, ReportEntry entry) {
                send(TestProtocol.encodeLog("info", entry.toString()));
              }
            };

        launcher.execute(request, listener);
      } finally {
        if (!shareSession) handle.close();
      }
      flushAll();
      // Any requested class no engine claimed (filtered out, or a JUnit 4 class with no vintage
      // engine) never got a container-finish. Report it, once.
      for (String c : requested) emitDone.accept(c);
      send(TestProtocol.encodeBatchComplete());
    } catch (Throwable e) {
      send(TestProtocol.encodeLog("error", stackTraceToString(e)));
      long dur = System.currentTimeMillis() - batchStart;
      for (String c : requested) {
        if (reported.add(c))
          send(
              TestProtocol.encodeSuiteErrored(
                  c,
                  dur,
                  "Batch execution failed: " + e.getClass().getName() + ": " + e.getMessage(),
                  stackTraceToString(e)));
      }
      send(TestProtocol.encodeBatchComplete());
    }
  }

  /**
   * The class a test or container belongs to, from its source. Null for engine roots and anything
   * without a class source.
   */
  private static String classOf(TestIdentifier id) {
    return id.getSource()
        .map(
            s -> {
              if (s instanceof MethodSource) return ((MethodSource) s).getClassName();
              if (s instanceof ClassSource) return ((ClassSource) s).getClassName();
              return null;
            })
        .orElse(null);
  }

  /**
   * Wall-clock duration of one test, measured by the runner because JUnit Platform's {@link
   * TestExecutionListener} does not carry one. The start is stamped in {@code executionStarted}
   * (keyed by unique id, safe under the concurrent classes of a batch) and read back here with the
   * entry removed. {@link System#nanoTime()} so a wall-clock adjustment mid-suite cannot make it
   * negative; a missing start (a test that finished without a matching start, e.g. skipped) reads 0
   * rather than a bogus age-of-the-map.
   */
  private static long elapsedMs(Long startNanos) {
    return startNanos == null ? 0L : Math.max(0L, (System.nanoTime() - startNanos) / 1_000_000L);
  }

  private void send(String message) {
    sink.accept(message);
  }

  private void flushAll() throws IOException {
    for (Flushable f : toFlush) f.flush();
  }

  /**
   * A name for a container reported as if it were a test. Roots are engine descriptors ("JUnit
   * Vintage"); everything below them is a class, a {@code @Nested} class, or a parameterized group,
   * all of which fail for class-lifecycle reasons.
   */
  private static String containerTestName(TestIdentifier testIdentifier) {
    String suffix = testIdentifier.getParentId().isPresent() ? " (class-level)" : " (engine-level)";
    return testIdentifier.getDisplayName() + suffix;
  }

  private static String stackTraceToString(Throwable t) {
    java.io.StringWriter sw = new java.io.StringWriter();
    t.printStackTrace(new java.io.PrintWriter(sw));
    return sw.toString();
  }
}
