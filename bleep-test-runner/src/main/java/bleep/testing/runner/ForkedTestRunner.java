package bleep.testing.runner;

import java.io.*;
import java.security.Permission;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import sbt.testing.*;

/**
 * Entry point for forked test execution.
 *
 * <p>This class runs in a forked JVM process and communicates with bleep over stdin/stdout using
 * the TestProtocol. It loads test frameworks dynamically and executes test suites as requested.
 *
 * <p>Key features: - Captures stdout/stderr from tests and sends via protocol - Prevents
 * System.exit from killing the JVM (on older JVMs) - Supports cancellation via protocol or stdin
 * EOF - Handles test exceptions gracefully
 *
 * <p>Usage: java -cp <classpath> bleep.testing.runner.ForkedTestRunner
 */
public class ForkedTestRunner {

  // Protocol output - use dedicated streams to avoid test interference
  private static volatile PrintWriter protocolOut;

  // Flag to indicate we're shutting down
  private static final AtomicBoolean shuttingDown = new AtomicBoolean(false);

  // Currently running test task thread for cancellation
  private static final AtomicReference<Thread> currentTask = new AtomicReference<>(null);

  // Common framework class name mappings
  private static final Map<String, String> FRAMEWORK_CLASSES = new HashMap<>();

  // JUnit can be either JUnit 4 (junit-interface) or JUnit 5 (jupiter-interface)
  // We try jupiter first, then fall back to junit-interface
  private static final String[] JUNIT_FRAMEWORKS = {
    "net.aichler.jupiter.api.JupiterFramework", // JUnit 5 (jupiter-interface)
    "com.github.sbt.junit.JupiterFramework", // JUnit 5 (sbt-jupiter-interface)
    "com.novocode.junit.JUnitFramework" // JUnit 4 (junit-interface)
  };

  static {
    FRAMEWORK_CLASSES.put("ScalaTest", "org.scalatest.tools.Framework");
    FRAMEWORK_CLASSES.put("scalatest", "org.scalatest.tools.Framework");
    FRAMEWORK_CLASSES.put("MUnit", "munit.Framework");
    FRAMEWORK_CLASSES.put("munit", "munit.Framework");
    FRAMEWORK_CLASSES.put("utest", "utest.runner.Framework");
    FRAMEWORK_CLASSES.put("ZIO Test", "zio.test.sbt.ZTestFramework");
    FRAMEWORK_CLASSES.put("zio-test", "zio.test.sbt.ZTestFramework");
    FRAMEWORK_CLASSES.put("Specs2", "org.specs2.runner.Specs2Framework");
    FRAMEWORK_CLASSES.put("specs2", "org.specs2.runner.Specs2Framework");
    // JUnit is handled specially in loadFramework() to try multiple implementations
    FRAMEWORK_CLASSES.put("Weaver", "weaver.sbt.WeaverFramework");
    FRAMEWORK_CLASSES.put("weaver", "weaver.sbt.WeaverFramework");
  }

  public static void main(String[] args) {
    // Save original streams for protocol communication
    PrintStream originalOut = System.out;
    PrintStream originalErr = System.err;
    InputStream originalIn = System.in;

    protocolOut = new PrintWriter(originalOut, true);

    try {
      // Install stdout/stderr capture
      CapturingOutputStream capturedOut = new CapturingOutputStream("stdout");
      CapturingOutputStream capturedErr = new CapturingOutputStream("stderr");
      System.setOut(new PrintStream(capturedOut, true));
      System.setErr(new PrintStream(capturedErr, true));

      // Install security manager to catch System.exit (if supported)
      installSecurityManager();

      // Signal ready
      send(TestProtocol.encodeReady());

      BufferedReader in = new BufferedReader(new InputStreamReader(originalIn));

      // Main command loop
      boolean running = true;
      while (running && !shuttingDown.get()) {
        try {
          String line = in.readLine();
          if (line == null) {
            // EOF - parent process closed stdin, shut down
            running = false;
          } else {
            TestProtocol.ParsedCommand cmd = TestProtocol.parseCommand(line);
            if (cmd instanceof TestProtocol.ParsedCommand.Shutdown) {
              running = false;
            } else if (cmd instanceof TestProtocol.ParsedCommand.RunSuite) {
              TestProtocol.ParsedCommand.RunSuite runSuite =
                  (TestProtocol.ParsedCommand.RunSuite) cmd;
              // Run in current thread so we can interrupt it
              currentTask.set(Thread.currentThread());
              try {
                runSuite(
                    runSuite.className,
                    runSuite.framework,
                    runSuite.args,
                    capturedOut,
                    capturedErr);
              } finally {
                currentTask.set(null);
              }
            } else if (cmd instanceof TestProtocol.ParsedCommand.GetThreadDump) {
              send(generateThreadDump());
            } else if (cmd instanceof TestProtocol.ParsedCommand.Invalid) {
              TestProtocol.ParsedCommand.Invalid invalid = (TestProtocol.ParsedCommand.Invalid) cmd;
              send(TestProtocol.encodeError("Failed to decode command: " + invalid.message, null));
            }
          }
        } catch (Exception e) {
          if (e instanceof InterruptedException) {
            // We were interrupted (cancellation) - continue loop to get next command
            Thread.interrupted(); // Clear interrupt flag
            continue;
          }
          send(
              TestProtocol.encodeError(
                  "Error in command loop: " + e.getMessage(), stackTraceToString(e)));
        }
      }
    } catch (Exception e) {
      send(
          TestProtocol.encodeError(
              "Fatal error in test runner: " + e.getMessage(), stackTraceToString(e)));
    } finally {
      // Restore original streams
      System.setOut(originalOut);
      System.setErr(originalErr);
    }
  }

  private static void send(String message) {
    protocolOut.println(message);
    protocolOut.flush();
  }

  /**
   * Security manager that catches System.exit calls. Note: SecurityManager is deprecated in Java
   * 17+ and may not be available.
   */
  @SuppressWarnings("removal")
  private static void installSecurityManager() {
    try {
      final SecurityManager originalSm = System.getSecurityManager();

      System.setSecurityManager(
          new SecurityManager() {
            @Override
            public void checkPermission(Permission perm) {
              if (originalSm != null) {
                originalSm.checkPermission(perm);
              }
            }

            @Override
            public void checkPermission(Permission perm, Object context) {
              if (originalSm != null) {
                originalSm.checkPermission(perm, context);
              }
            }

            @Override
            public void checkExit(int status) {
              send(
                  TestProtocol.encodeLog(
                      "warn", "Test attempted System.exit(" + status + ") - blocked"));
              throw new SecurityException("System.exit(" + status + ") blocked by test runner");
            }
          });
    } catch (UnsupportedOperationException e) {
      // SecurityManager is not supported on this JVM (Java 17+)
      // Tests calling System.exit will terminate the forked JVM
    }
  }

  private static void runSuite(
      String className,
      String frameworkName,
      List<String> args,
      OutputStream capturedOut,
      OutputStream capturedErr) {
    long startTime = System.currentTimeMillis();

    try {
      // Flush any pending output before starting
      capturedOut.flush();
      capturedErr.flush();

      // Load the framework
      Framework framework = loadFramework(frameworkName);

      // Get the runner
      Runner runner =
          framework.runner(
              args.toArray(new String[0]), new String[0], ForkedTestRunner.class.getClassLoader());

      // Create a TaskDef for the class - let the framework handle discovery
      // BSP already told us this is a test class, so trust that
      Fingerprint[] fingerprints = framework.fingerprints();
      Fingerprint fingerprint = fingerprints.length > 0 ? fingerprints[0] : null;

      if (fingerprint == null) {
        send(TestProtocol.encodeError("Framework has no fingerprints: " + frameworkName, null));
        return;
      }

      TaskDef taskDef =
          new TaskDef(className, fingerprint, false, new Selector[] {new SuiteSelector()});
      Task[] tasks = runner.tasks(new TaskDef[] {taskDef});

      if (tasks.length == 0) {
        send(TestProtocol.encodeError("No tests found for class: " + className, null));
        return;
      }

      // Collect results
      final int[] passed = {0};
      final int[] failed = {0};
      final int[] skipped = {0};
      final int[] ignored = {0};

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
                  status = "cancelled";
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
              if (event.throwable() != null && event.throwable().isDefined()) {
                Throwable t = event.throwable().get();
                message = t.getMessage();
                throwableStr = stackTraceToString(t);
              }

              // Extract test name from selector if available
              String testName = extractTestName(event);

              // Flush output before reporting test finished
              try {
                capturedOut.flush();
                capturedErr.flush();
              } catch (IOException e) {
                // Ignore
              }

              send(
                  TestProtocol.encodeTestFinished(
                      className, testName, status, event.duration(), message, throwableStr));
            }
          };

      // Execute tasks
      Logger logger = createLogger(className);
      executeTasks(tasks, eventHandler, new Logger[] {logger});

      // Done
      runner.done();

      // Final flush
      capturedOut.flush();
      capturedErr.flush();

      long durationMs = System.currentTimeMillis() - startTime;
      send(
          TestProtocol.encodeSuiteDone(
              className, passed[0], failed[0], skipped[0], ignored[0], durationMs));

    } catch (InterruptedException e) {
      // Cancelled - report and re-throw to exit the run
      send(TestProtocol.encodeLog("warn", "Suite " + className + " was cancelled"));
      throw new RuntimeException(e);
    } catch (SecurityException e) {
      if (e.getMessage() != null && e.getMessage().contains("System.exit")) {
        // System.exit was blocked - report failure
        send(
            TestProtocol.encodeSuiteDone(
                className, 0, 1, 0, 0, System.currentTimeMillis() - startTime));
      } else {
        throw e;
      }
    } catch (Exception e) {
      send(
          TestProtocol.encodeError(
              "Error running suite " + className + ": " + e.getMessage(), stackTraceToString(e)));
    }
  }

  private static void executeTasks(Task[] tasks, EventHandler eventHandler, Logger[] loggers)
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
      } catch (Exception e) {
        // Log error but continue with other tasks
        send(TestProtocol.encodeLog("error", "Task execution failed: " + e.getMessage()));
      }
    }
  }

  private static Framework loadFramework(String name) throws Exception {
    // Special handling for JUnit - try multiple implementations
    if (name.equalsIgnoreCase("JUnit")) {
      for (String frameworkClass : JUNIT_FRAMEWORKS) {
        try {
          Class<?> clazz = Class.forName(frameworkClass);
          Framework fw = (Framework) clazz.getDeclaredConstructor().newInstance();
          send(TestProtocol.encodeLog("info", "Loaded JUnit framework: " + frameworkClass));
          return fw;
        } catch (ClassNotFoundException e) {
          // Try next one
        }
      }
      throw new ClassNotFoundException(
          "No JUnit framework found. Tried: " + String.join(", ", JUNIT_FRAMEWORKS));
    }

    String className = FRAMEWORK_CLASSES.getOrDefault(name, name);
    Class<?> clazz = Class.forName(className);
    return (Framework) clazz.getDeclaredConstructor().newInstance();
  }

  private static Logger createLogger(final String suiteName) {
    return new Logger() {
      @Override
      public boolean ansiCodesSupported() {
        return true;
      }

      @Override
      public void error(String msg) {
        send(TestProtocol.encodeLog("error", msg));
      }

      @Override
      public void warn(String msg) {
        send(TestProtocol.encodeLog("warn", msg));
      }

      @Override
      public void info(String msg) {
        send(TestProtocol.encodeLog("info", msg));
      }

      @Override
      public void debug(String msg) {
        send(TestProtocol.encodeLog("debug", msg));
      }

      @Override
      public void trace(Throwable t) {
        send(TestProtocol.encodeLog("error", stackTraceToString(t)));
      }
    };
  }

  private static String stackTraceToString(Throwable t) {
    StringWriter sw = new StringWriter();
    t.printStackTrace(new PrintWriter(sw));
    return sw.toString();
  }

  /**
   * Extract the test name from an event. Tries to get the test method name from the selector, falls
   * back to fullyQualifiedName.
   */
  private static String extractTestName(Event event) {
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

  /** Generate a thread dump of all threads in the JVM. Returns encoded JSON response. */
  private static String generateThreadDump() {
    List<TestProtocol.ThreadDumpEntry> entries = new ArrayList<>();

    // Get all thread stack traces
    Map<Thread, StackTraceElement[]> allStackTraces = Thread.getAllStackTraces();

    for (Map.Entry<Thread, StackTraceElement[]> entry : allStackTraces.entrySet()) {
      Thread thread = entry.getKey();
      StackTraceElement[] stackTrace = entry.getValue();

      // Convert stack trace to list of strings
      List<String> stackLines = new ArrayList<>();
      for (StackTraceElement element : stackTrace) {
        stackLines.add(element.toString());
      }

      entries.add(
          new TestProtocol.ThreadDumpEntry(
              thread.getName(), thread.getState().toString(), stackLines));
    }

    return TestProtocol.encodeThreadDump(entries);
  }

  /** Output stream that captures writes and sends them via protocol. */
  private static class CapturingOutputStream extends OutputStream {
    private final String name;
    private final StringBuilder buffer = new StringBuilder();
    private final Object lock = new Object();

    CapturingOutputStream(String name) {
      this.name = name;
    }

    @Override
    public void write(int b) {
      synchronized (lock) {
        if (b == '\n') {
          flush();
        } else {
          buffer.append((char) b);
        }
      }
    }

    @Override
    public void write(byte[] b, int off, int len) {
      synchronized (lock) {
        String s = new String(b, off, len);
        for (int i = 0; i < s.length(); i++) {
          char c = s.charAt(i);
          if (c == '\n') {
            flush();
          } else {
            buffer.append(c);
          }
        }
      }
    }

    @Override
    public void flush() {
      synchronized (lock) {
        if (buffer.length() > 0) {
          String level = "stderr".equals(name) ? "error" : "info";
          send(TestProtocol.encodeLog(level, buffer.toString()));
          buffer.setLength(0);
        }
      }
    }
  }
}
