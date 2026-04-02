package bleep.testing.runner;

import static org.junit.platform.engine.discovery.DiscoverySelectors.selectClass;

import java.io.OutputStream;
import java.io.PrintWriter;
import org.junit.platform.engine.TestExecutionResult;
import org.junit.platform.engine.reporting.ReportEntry;
import org.junit.platform.launcher.Launcher;
import org.junit.platform.launcher.LauncherDiscoveryRequest;
import org.junit.platform.launcher.LauncherSession;
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

  private final PrintWriter protocolOut;

  JUnitPlatformRunner(PrintWriter protocolOut) {
    this.protocolOut = protocolOut;
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
        TestProtocol.encodeLog("info", "Using JUnit Platform Launcher directly for: " + className));

    int[] passed = {0};
    int[] failed = {0};
    int[] skipped = {0};
    int[] ignored = {0};

    try {
      // Flush any pending output
      capturedOut.flush();
      capturedErr.flush();

      // Open a LauncherSession — this triggers LauncherSessionListener SPI.
      // Quarkus's CustomLauncherInterceptor creates FacadeClassLoader here.
      try (LauncherSession session = LauncherFactory.openSession()) {
        Launcher launcher = session.getLauncher();

        LauncherDiscoveryRequest request =
            LauncherDiscoveryRequestBuilder.request().selectors(selectClass(className)).build();

        TestExecutionListener listener =
            new TestExecutionListener() {
              @Override
              public void testPlanExecutionStarted(TestPlan testPlan) {
                long count = testPlan.countTestIdentifiers(t -> t.isTest());
                send(
                    TestProtocol.encodeLog(
                        "info",
                        "TestPlan started: "
                            + count
                            + " test(s) in plan, roots="
                            + testPlan.getRoots().size()));
                for (TestIdentifier root : testPlan.getRoots()) {
                  send(
                      TestProtocol.encodeLog(
                          "info",
                          "  Root: " + root.getDisplayName() + " [" + root.getUniqueId() + "]"));
                  for (TestIdentifier child : testPlan.getChildren(root)) {
                    send(
                        TestProtocol.encodeLog(
                            "info",
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
                if (testIdentifier.isTest()) {
                  String testName = testIdentifier.getDisplayName();
                  send(TestProtocol.encodeTestStarted(currentSuite, testName));
                }
              }

              @Override
              public void executionFinished(
                  TestIdentifier testIdentifier, TestExecutionResult result) {
                if (!testIdentifier.isTest()) return;

                String testName = testIdentifier.getDisplayName();
                long durationMs = 0; // JUnit Platform doesn't provide per-test duration in listener

                String status;
                String message = null;
                String throwableStr = null;

                switch (result.getStatus()) {
                  case SUCCESSFUL:
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

              @Override
              public void executionSkipped(TestIdentifier testIdentifier, String reason) {
                if (!testIdentifier.isTest()) return;
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

        launcher.execute(request, listener);
      }

      // Flush and report done
      capturedOut.flush();
      capturedErr.flush();

      long durationMs = System.currentTimeMillis() - startTime;
      send(
          TestProtocol.encodeSuiteDone(
              className, passed[0], failed[0], skipped[0], ignored[0], durationMs));

    } catch (Throwable e) {
      send(
          TestProtocol.encodeLog(
              "error",
              "Error in JUnit Platform runner: " + e.getClass().getName() + ": " + e.getMessage()));
      send(TestProtocol.encodeLog("error", stackTraceToString(e)));
      send(
          TestProtocol.encodeSuiteDone(
              className,
              passed[0],
              failed[0] + 1,
              skipped[0],
              ignored[0],
              System.currentTimeMillis() - startTime));
    }
  }

  private void send(String message) {
    protocolOut.println(message);
    protocolOut.flush();
  }

  private static String stackTraceToString(Throwable t) {
    java.io.StringWriter sw = new java.io.StringWriter();
    t.printStackTrace(new java.io.PrintWriter(sw));
    return sw.toString();
  }
}
