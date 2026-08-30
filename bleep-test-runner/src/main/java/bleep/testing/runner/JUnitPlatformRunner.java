package bleep.testing.runner;

import static org.junit.platform.engine.discovery.DiscoverySelectors.selectClass;

import java.io.OutputStream;
import java.io.PrintWriter;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import org.junit.platform.engine.TestExecutionResult;
import org.junit.platform.engine.reporting.ReportEntry;
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

  private final PrintWriter protocolOut;

  JUnitPlatformRunner(PrintWriter protocolOut) {
    this.protocolOut = protocolOut;
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
   * @param capturedOut captured stdout stream
   * @param capturedErr captured stderr stream
   */
  void runSuite(String className, OutputStream capturedOut, OutputStream capturedErr) {

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

    try {
      // Flush any pending output
      capturedOut.flush();
      capturedErr.flush();

      // Open a LauncherSession where the platform has one — this triggers LauncherSessionListener
      // SPI.
      // Quarkus's CustomLauncherInterceptor creates FacadeClassLoader here.
      try (LauncherHandle handle = openLauncher()) {
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
                long durationMs = 0; // JUnit Platform doesn't provide per-test duration in listener

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
                  capturedOut.flush();
                  capturedErr.flush();
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
                  capturedOut.flush();
                  capturedErr.flush();
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
      }

      // Flush and report done
      capturedOut.flush();
      capturedErr.flush();

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

  private void send(String message) {
    protocolOut.println(message);
    protocolOut.flush();
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
