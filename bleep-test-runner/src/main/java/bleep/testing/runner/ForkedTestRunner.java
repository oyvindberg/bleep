package bleep.testing.runner;

import java.io.*;
import java.net.InetAddress;
import java.net.Socket;
import java.nio.charset.StandardCharsets;
import java.security.Permission;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.TreeMap;
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

  // Currently running suite name (for output tagging)
  private static volatile String currentSuite = null;

  public static void main(String[] args) {
    // Save original streams for protocol communication
    PrintStream originalOut = System.out;
    PrintStream originalErr = System.err;

    // The protocol runs over a loopback socket the parent is already listening on, NOT over this
    // process's stdin/stdout.
    //
    // Sharing stdout with the protocol meant that anything writing to file descriptor 1 landed in
    // the middle of the JSON stream, and a test cannot be stopped from doing that: `System.out` is
    // captured below, but a subprocess started with inherited IO writes to the descriptor directly,
    // beneath any Java-level redirection. Scala Native's test binaries are spawned that way by
    // `scala.scalanative.testinterface.ProcessRunner` (a hardcoded `ProcessBuilder.inheritIO()`),
    // and so is anything a user's own test launches the same way. The parent saw
    // "Protocol error: expected json value got 'Test r...'" and reported a suite that never
    // finished.
    //
    // With the protocol on its own channel, stdout and stderr are just output: the parent drains
    // them and attributes the lines to the running suite, so that subprocess output reaches the
    // user instead of corrupting the run.
    Socket protocolSocket = null;
    try {
      String portProperty = System.getProperty(PROTOCOL_PORT_PROPERTY);
      if (portProperty == null) {
        // No fallback to stdio. The runner is always launched by a bleep of the same version, which
        // always sets this, so a missing port means a broken launch and not an older parent.
        originalErr.println(
            "bleep test runner: -D"
                + PROTOCOL_PORT_PROPERTY
                + " was not set; cannot reach the parent");
        System.exit(2);
      }
      protocolSocket = new Socket(InetAddress.getLoopbackAddress(), Integer.parseInt(portProperty));
      protocolSocket.setTcpNoDelay(true);
      protocolOut =
          new PrintWriter(
              new OutputStreamWriter(protocolSocket.getOutputStream(), StandardCharsets.UTF_8),
              true);

      // Install stdout/stderr capture. Still worth doing even though the protocol has moved: output
      // written through `System.out` can be attributed to the suite that produced it, which raw
      // descriptor writes drained by the parent cannot be.
      CapturingOutputStream capturedOut = new CapturingOutputStream("stdout");
      CapturingOutputStream capturedErr = new CapturingOutputStream("stderr");
      System.setOut(new PrintStream(capturedOut, true));
      System.setErr(new PrintStream(capturedErr, true));

      // Install security manager to catch System.exit (if supported)
      installSecurityManager();

      // Signal ready
      send(TestProtocol.encodeReady());

      BufferedReader in =
          new BufferedReader(
              new InputStreamReader(protocolSocket.getInputStream(), StandardCharsets.UTF_8));

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
                    runSuite.runner,
                    runSuite.frameworkClass,
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
      if (protocolSocket != null) {
        try {
          protocolSocket.close();
        } catch (IOException ignored) {
          // The parent may have closed first; nothing useful left to do either way.
        }
      }
    }
  }

  /**
   * System property carrying the port the parent listens on for this fork's protocol connection.
   */
  static final String PROTOCOL_PORT_PROPERTY = "bleep.test.protocolPort";

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
      TestProtocol.RunnerKind runnerKind,
      String frameworkClass,
      List<String> args,
      OutputStream capturedOut,
      OutputStream capturedErr) {

    // Set current suite for output tagging
    currentSuite = className;

    send(
        TestProtocol.encodeLog(
            "info",
            "runSuite called: className=" + className + ", frameworkName=" + frameworkName));

    // The server decided this, with the project's classpath in front of it. Nothing here re-derives
    // it from frameworkName, which is a display label.
    if (runnerKind == TestProtocol.RunnerKind.JUNIT_PLATFORM) {
      JUnitPlatformRunner junitRunner = new JUnitPlatformRunner(protocolOut);
      junitRunner.runSuite(className, capturedOut, capturedErr);
      return;
    }

    long startTime = System.currentTimeMillis();

    // Counters declared outside try so they're accessible in catch for SuiteDone reporting
    final int[] passed = {0};
    final int[] failed = {0};
    final int[] skipped = {0};
    final int[] ignored = {0};

    try {
      // Flush any pending output before starting
      capturedOut.flush();
      capturedErr.flush();

      // Load the framework
      send(TestProtocol.encodeLog("debug", "Loading framework: " + frameworkClass));
      Framework framework = loadFramework(frameworkClass);
      send(TestProtocol.encodeLog("debug", "Framework loaded: " + framework.getClass().getName()));

      // Get the runner
      Runner runner =
          framework.runner(
              args.toArray(new String[0]), new String[0], ForkedTestRunner.class.getClassLoader());

      // Try each fingerprint from the framework until we find one that produces tasks.
      // Different fingerprints match different test patterns (e.g. @Test annotation vs
      // TestCase subclass), so we need to find the right one for this class.
      Fingerprint[] fingerprints = framework.fingerprints();

      if (fingerprints.length == 0) {
        send(TestProtocol.encodeError("Framework has no fingerprints: " + frameworkName, null));
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
          send(
              TestProtocol.encodeLog(
                  "debug", "Matched fingerprint: " + describeFingerprint(fingerprint)));
          break;
        }
      }

      if (tasks == null || tasks.length == 0) {
        // No fingerprint produced a task: the loaded framework does not recognize this class as
        // a suite. Not an empty suite (the framework never claimed it) — a framework mismatch.
        send(
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
                capturedOut.flush();
                capturedErr.flush();
              } catch (IOException e) {
                // Ignore
              }

              send(
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

      // Done
      runner.done();

      // Final flush
      capturedOut.flush();
      capturedErr.flush();

      long durationMs = System.currentTimeMillis() - startTime;
      int total = passed[0] + failed[0] + skipped[0] + ignored[0];
      if (total == 0) {
        // The framework claimed the class (a task ran) but no test fired an event: an empty suite.
        send(TestProtocol.encodeSuiteEmpty(className, durationMs));
      } else {
        send(
            TestProtocol.encodeSuiteExecuted(
                className, passed[0], failed[0], skipped[0], ignored[0], durationMs));
      }

    } catch (InterruptedException e) {
      // Cancelled - report and re-throw to exit the run
      send(TestProtocol.encodeLog("warn", "Suite " + className + " was cancelled"));
      throw new RuntimeException(e);
    } catch (SecurityException e) {
      if (e.getMessage() != null && e.getMessage().contains("System.exit")) {
        send(
            TestProtocol.encodeSuiteErrored(
                className,
                System.currentTimeMillis() - startTime,
                "Test attempted a blocked System.exit",
                null));
      } else {
        throw e;
      }
    } catch (Throwable e) {
      // Must catch Throwable (not just Exception): a framework may let an Error (AssertionError,
      // or a LinkageError propagated from executeTasks) escape. Report it as an errored suite —
      // NOT SuiteExecuted with faked counts — so the outcome carries the real reason.
      send(TestProtocol.encodeLog("error", stackTraceToString(e)));
      Throwable reported =
          (e instanceof SuiteExecutionException && e.getCause() != null) ? e.getCause() : e;
      send(
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
  private static Framework loadFramework(String frameworkClass) throws Exception {
    Class<?> clazz = Class.forName(frameworkClass);
    return (Framework) clazz.getDeclaredConstructor().newInstance();
  }

  /** Check if this framework should use JUnit Platform Launcher directly. */
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

  /**
   * Puts fingerprints whose `isModule` matches the class on disk first, keeping the framework's own
   * order within each group. Nothing is discarded: a framework that disagrees with this reading
   * still gets every fingerprint tried, just second.
   */
  private static Fingerprint[] orderFingerprintsFor(String className, Fingerprint[] fingerprints) {
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
  private static Optional<Class<?>> fingerprintSuperclass(Fingerprint fp) {
    if (!(fp instanceof SubclassFingerprint)) return Optional.empty();
    return Optional.ofNullable(loadClass(((SubclassFingerprint) fp).superclassName()));
  }

  private static Class<?> loadClass(String name) {
    try {
      return Class.forName(name, false, ForkedTestRunner.class.getClassLoader());
    } catch (ClassNotFoundException | LinkageError e) {
      return null;
    }
  }

  /** Null when the fingerprint kind says nothing about module-ness. */
  private static Boolean fingerprintIsModule(Fingerprint fp) {
    if (fp instanceof SubclassFingerprint) return ((SubclassFingerprint) fp).isModule();
    if (fp instanceof AnnotatedFingerprint) return ((AnnotatedFingerprint) fp).isModule();
    return null;
  }

  private static String describeFingerprint(Fingerprint fp) {
    if (fp instanceof SubclassFingerprint) {
      SubclassFingerprint sfp = (SubclassFingerprint) fp;
      return "SubclassFingerprint(" + sfp.superclassName() + ", isModule=" + sfp.isModule() + ")";
    } else if (fp instanceof AnnotatedFingerprint) {
      AnnotatedFingerprint afp = (AnnotatedFingerprint) fp;
      return "AnnotatedFingerprint(" + afp.annotationName() + ", isModule=" + afp.isModule() + ")";
    }
    return fp.toString();
  }

  private static String stackTraceToString(Throwable t) {
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
  private static StackTraceElement failureLocation(Throwable t, String suiteClass) {
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
          // Include current suite in log message if available
          send(TestProtocol.encodeLog(currentSuite, level, buffer.toString()));
          buffer.setLength(0);
        }
      }
    }
  }
}
