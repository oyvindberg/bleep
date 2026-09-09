package bleep.testing.runner;

import static org.junit.platform.engine.discovery.DiscoverySelectors.selectClass;

import java.io.Flushable;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.List;
import java.util.Map;
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
 * required for frameworks that set up custom classloaders during session initialization.
 *
 * <p>Using openSession() instead of create() triggers any registered LauncherSessionListener
 * implementations — e.g. a framework that installs a custom classloader as the thread-context
 * classloader, or a test-context manager.
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
                  + " LauncherSessionListener extensions do not exist on this version."));
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

      // A LauncherSession is where LauncherSessionListener SPI fires (a framework's registrar, or
      // an interceptor that builds a custom classloader). In fork mode one
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
   * Run a project's classes through the shared LauncherSession — one {@code launcher.execute} per
   * class — at a bleep-chosen degree of parallelism.
   *
   * <p>The shared session is what makes an execution-scoped fixture — an application booted for the
   * run — build once and be reused by every class, as under maven surefire's
   * one-execute-per-module. Per-class results are still reported: the listener attributes each test
   * and container to the requested class it belongs to (a {@code @Nested Foo$Bar} test back to
   * {@code Foo}) and sends that class's own SuiteDone when its container finishes, so the parent
   * demultiplexes per suite.
   *
   * <p>Parallelism is bleep's own: a fixed thread pool runs {@code parallelism} classes at once,
   * each as its own {@code launcher.execute}. bleep sets NO JUnit configuration parameters, so a
   * {@code junit-platform.properties} on the classpath still governs parallelism WITHIN a class
   * (jupiter's own parallel execution) — bleep neither enables nor overrides it. And because each
   * class runs in a separate execution, {@code @ResourceLock} across classes is not coordinated by
   * the engine; keep {@code parallelism} at 1 (the default) when a project's classes share mutable
   * state, which is also what a singleton-per-JVM application requires.
   */
  void runSuites(List<String> classNames, int parallelism) {
    // One launcher.execute PER class, on the shared LauncherSession — not one execute selecting all
    // classes. The one-execute form lost tests for engines whose test tree is not keyed by the
    // selected class: a cucumber scenario's class source is the glue/feature, a spek test's is the
    // spec node, so neither maps back to the requested Fixture and it reported zero. Per-class
    // execute attributes every test in that execute to the one class it selected — exactly what the
    // single-suite path already does correctly. The session is opened once (SHARE_SESSION) and
    // reused across the classes, so a session-scoped fixture — a booted application, a
    // LauncherSessionListener — is still built once, which was the point of batching. `parallelism`
    // bounds how many classes execute at once (1 = sequential, the safe default).
    try {
      if (parallelism <= 1) {
        for (String c : classNames) {
          ForkedTestRunner.setCurrentSuite(c);
          try {
            runSuite(c);
          } finally {
            ForkedTestRunner.setCurrentSuite(null);
          }
        }
      } else {
        java.util.concurrent.ExecutorService pool =
            java.util.concurrent.Executors.newFixedThreadPool(
                Math.min(parallelism, Math.max(1, classNames.size())));
        try {
          java.util.List<java.util.concurrent.Future<?>> futures = new java.util.ArrayList<>();
          for (String c : classNames) {
            futures.add(
                pool.submit(
                    () -> {
                      ForkedTestRunner.setCurrentSuite(c);
                      try {
                        runSuite(c);
                      } finally {
                        ForkedTestRunner.setCurrentSuite(null);
                      }
                      return null;
                    }));
          }
          for (java.util.concurrent.Future<?> f : futures) {
            try {
              f.get();
            } catch (java.util.concurrent.ExecutionException e) {
              send(
                  TestProtocol.encodeLog(
                      "error", stackTraceToString(e.getCause() == null ? e : e.getCause())));
            } catch (InterruptedException e) {
              Thread.currentThread().interrupt();
              break;
            }
          }
        } finally {
          pool.shutdownNow();
        }
      }
    } finally {
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
