package bleep.testing.runner;

import java.io.*;
import java.net.InetAddress;
import java.net.Socket;
import java.nio.charset.StandardCharsets;
import java.security.Permission;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicBoolean;
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

  // Suites currently running, keyed by class name, so a CancelSuite can interrupt exactly one
  // without touching the fork or its siblings. A project's class names are unique, so the name is a
  // sufficient key. When the fork runs one suite at a time (an exclusive per-suite session) this
  // map
  // simply never holds more than one entry.
  private static final Map<String, Thread> runningSuites = new ConcurrentHashMap<>();

  // The suite the CURRENT thread is running, for tagging that thread's captured output.
  // Thread-local,
  // not global: with several suites in flight at once, each runs on its own thread, and a line
  // written on a suite's thread belongs to that suite. Output from framework/async threads that
  // never
  // set this (a Vert.x event loop, a Netty worker) has no owning suite and is tagged null.
  private static final ThreadLocal<String> currentSuite = new ThreadLocal<>();

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
        // No fallback to stdio. The runner is always launched by a bleep built from the same source
        // as the server, which always sets this, so a missing port means a mismatched launch rather
        // than an older parent -- and a mismatched pair would misread the protocol a moment later
        // anyway. Better to say so here than to deadlock or decode garbage.
        //
        // The one way to reach this in practice is building bleep itself with a *released* bleep:
        // the server then resolves `bleep-test-runner` out of the build being compiled (that is
        // deliberate, so bleep's own tests exercise the runner they just built) while the server
        // itself is the released one. `--dev` is what keeps the two halves together.
        originalErr.println(
            "bleep test runner: -D"
                + PROTOCOL_PORT_PROPERTY
                + " was not set, so this fork cannot reach the bleep that started it.");
        originalErr.println(
            "  This runner was built from a different source tree than the server driving it.");
        originalErr.println(
            "  When building bleep itself, run the tests through the dev script:"
                + " `bleep setup-dev-script bleep-cli` then `./bleep-cli.sh --dev test`.");
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

      // One LauncherSession for this whole fork: a LauncherSessionListener fires once here, not
      // once
      // per suite — which is what a test harness written for maven's one-fork-per-module assumes,
      // and what keeps concurrent suites from racing on a "register this global once" listener.
      //
      // Guarded because it is a JUnit-Platform concept and JUnitPlatformRunner links against
      // org.junit.platform.launcher.*. A fork for an sbt test-interface project (ScalaTest, MUnit,
      // utest, ...) has no JUnit Platform on its runtime classpath (junit-platform-launcher is a
      // `provided`, compile-only dependency of bleep-test-runner), so merely referencing that class
      // would NoClassDefFoundError and kill the fork before it reaches Ready. No JUnit suites will
      // run in such a fork, so there is nothing to share; skip it.
      if (junitPlatformOnClasspath()) {
        JUnitPlatformRunner.enableSharedSession();
      }

      // Signal ready
      send(TestProtocol.encodeReady());

      BufferedReader in =
          new BufferedReader(
              new InputStreamReader(protocolSocket.getInputStream(), StandardCharsets.UTF_8));

      // Main command loop.
      //
      // A RunSuite starts a thread and the loop goes straight back to reading, so several suites
      // can
      // be in flight at once (a per-project shared session dispatches them concurrently). The loop
      // is
      // never blocked on a running suite, which is what lets a CancelSuite for one suite — or a
      // Shutdown — be acted on while others keep running. An exclusive per-suite session sends one
      // RunSuite at a time and never overlaps them; the same code serves it with a map of size one.
      boolean running = true;
      while (running && !shuttingDown.get()) {
        try {
          String line = in.readLine();
          if (line == null) {
            // EOF - parent closed the protocol socket, shut down
            running = false;
          } else {
            TestProtocol.ParsedCommand cmd = TestProtocol.parseCommand(line);
            if (cmd instanceof TestProtocol.ParsedCommand.Shutdown) {
              running = false;
            } else if (cmd instanceof TestProtocol.ParsedCommand.RunSuite) {
              startSuiteThread((TestProtocol.ParsedCommand.RunSuite) cmd, capturedOut, capturedErr);
            } else if (cmd instanceof TestProtocol.ParsedCommand.RunSuites) {
              startSuitesThread(
                  (TestProtocol.ParsedCommand.RunSuites) cmd, capturedOut, capturedErr);
            } else if (cmd instanceof TestProtocol.ParsedCommand.CancelSuite) {
              String toCancel = ((TestProtocol.ParsedCommand.CancelSuite) cmd).className;
              Thread t = runningSuites.get(toCancel);
              // Interrupt only that suite's thread. A suite already finished (t == null) is a no-op
              // —
              // the cancel raced its completion, which is harmless.
              if (t != null) t.interrupt();
            } else if (cmd instanceof TestProtocol.ParsedCommand.GetThreadDump) {
              send(generateThreadDump());
            } else if (cmd instanceof TestProtocol.ParsedCommand.Invalid) {
              TestProtocol.ParsedCommand.Invalid invalid = (TestProtocol.ParsedCommand.Invalid) cmd;
              send(TestProtocol.encodeError("Failed to decode command: " + invalid.message, null));
            }
          }
        } catch (Exception e) {
          send(
              TestProtocol.encodeError(
                  "Error in command loop: " + e.getMessage(), SuiteRunner.stackTraceToString(e)));
        }
      }

      // Leaving the loop (Shutdown or EOF): interrupt whatever is still running so a wedged or
      // cancelled suite lets go, and give the threads a moment to emit their terminal responses
      // before the JVM's shutdown hooks (which stop a Quarkus app and its containers) run.
      shuttingDown.set(true);
      for (Thread t : runningSuites.values()) t.interrupt();
      long deadline = System.currentTimeMillis() + 5000;
      for (Thread t : runningSuites.values()) {
        long remaining = deadline - System.currentTimeMillis();
        if (remaining > 0) {
          try {
            t.join(remaining);
          } catch (InterruptedException ignored) {
            Thread.currentThread().interrupt();
          }
        }
      }
    } catch (Exception e) {
      send(
          TestProtocol.encodeError(
              "Fatal error in test runner: " + e.getMessage(), SuiteRunner.stackTraceToString(e)));
    } finally {
      // Close the shared LauncherSession, running its listeners' launcherSessionClosed — where a
      // booted application and its containers are asked to stop. Guarded for the same reason as the
      // open above: an sbt-interface fork has no JUnit Platform on its classpath, so touching
      // JUnitPlatformRunner here would NoClassDefFoundError in the finally and mask the real
      // result.
      if (junitPlatformOnClasspath()) {
        JUnitPlatformRunner.closeSharedSession();
      }
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

  // Synchronized: several suite threads share this one socket, and a response must reach the parent
  // as one whole line. Without the lock two println/flush pairs could interleave mid-line and the
  // parent would fail to decode the spliced JSON.
  private static synchronized void send(String message) {
    protocolOut.println(message);
    protocolOut.flush();
  }

  /**
   * Is JUnit Platform's launcher on this fork's classpath? Only then may we touch {@link
   * JUnitPlatformRunner}, which links against {@code org.junit.platform.launcher.*}. sbt
   * test-interface forks (ScalaTest, MUnit, utest, ...) have no JUnit Platform — {@code
   * junit-platform-launcher} is a {@code provided}, compile-only dependency of bleep-test-runner —
   * so referencing that class in such a fork NoClassDefFoundErrors. Probed with the class the
   * runner's shared-session lifecycle needs; loaded lazily (initialize=false) so the check itself
   * never triggers the failure it is guarding against.
   */
  private static boolean junitPlatformOnClasspath() {
    try {
      Class.forName(
          "org.junit.platform.launcher.TestExecutionListener",
          false,
          ForkedTestRunner.class.getClassLoader());
      return true;
    } catch (ClassNotFoundException e) {
      return false;
    }
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

  /**
   * Start a suite on its own thread and return at once, so the command loop can keep serving.
   *
   * <p>Each suite gets a fresh {@link SuiteRunner} (which holds no static state, precisely so N of
   * them can share this JVM). The thread registers itself under the suite's class name for the
   * lifetime of the run so a CancelSuite can find and interrupt it, and tags its own captured
   * output via the thread-local {@link #currentSuite}. What stays a fork's concern is what only a
   * fork has: the socket the lines go out on and the streams the tests write to.
   */
  private static void startSuiteThread(
      TestProtocol.ParsedCommand.RunSuite runSuite,
      OutputStream capturedOut,
      OutputStream capturedErr) {
    String className = runSuite.className;
    Thread t =
        new Thread(
            () -> {
              currentSuite.set(className);
              try {
                new SuiteRunner(
                        ForkedTestRunner::send,
                        ForkedTestRunner.class.getClassLoader(),
                        Arrays.asList(capturedOut, capturedErr))
                    .runSuite(
                        className,
                        runSuite.framework,
                        runSuite.runner.name(),
                        runSuite.frameworkClass,
                        runSuite.args);
              } finally {
                currentSuite.remove();
                runningSuites.remove(className, Thread.currentThread());
              }
            },
            "suite-" + className);
    // Register before start so a CancelSuite arriving immediately still finds the thread.
    runningSuites.put(className, t);
    t.start();
  }

  /**
   * Run a whole set of JUnit-Platform classes in ONE launcher execution on its own thread.
   *
   * <p>The batch is a single unit of work — junit's engine parallelises the classes inside it at
   * the degree bleep chose — so it registers under one key and its per-class results come out of
   * {@link JUnitPlatformRunner#runSuites} tagged by class. A {@link
   * TestProtocol#encodeBatchComplete} is sent when the one execute returns, telling the parent to
   * stop reading. Cancellation of a batch is fork-level (there is no per-class thread here to
   * interrupt).
   */
  private static void startSuitesThread(
      TestProtocol.ParsedCommand.RunSuites runSuites,
      OutputStream capturedOut,
      OutputStream capturedErr) {
    Thread t =
        new Thread(
            () -> {
              try {
                new JUnitPlatformRunner(
                        ForkedTestRunner::send, Arrays.asList(capturedOut, capturedErr))
                    .runSuites(runSuites.classNames, runSuites.parallelism);
              } finally {
                runningSuites.remove(BATCH_KEY, Thread.currentThread());
              }
            },
            "suites-batch");
    runningSuites.put(BATCH_KEY, t);
    t.start();
  }

  /**
   * The single key a batched (RunSuites) execution registers under; a project runs at most one
   * batch at a time.
   */
  private static final String BATCH_KEY = " batch";

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

  /**
   * Output stream that captures writes and sends them via protocol, one buffer per writing thread.
   *
   * <p>System.out is one stream shared by every thread in the JVM, so with several suites running
   * at once their bytes would interleave in a single buffer and a line could come out half from one
   * suite and half from another. A per-thread buffer keeps each writer's partial line to itself,
   * and the completed line is tagged with whatever suite that thread is running ({@link
   * #currentSuite}) — or none, for a framework thread that belongs to no single suite. When only
   * one suite runs at a time this is exactly the old behaviour with one live buffer.
   */
  private static class CapturingOutputStream extends OutputStream {
    private final String name;
    private final ThreadLocal<StringBuilder> buffer = ThreadLocal.withInitial(StringBuilder::new);

    CapturingOutputStream(String name) {
      this.name = name;
    }

    @Override
    public void write(int b) {
      if (b == '\n') {
        flush();
      } else {
        buffer.get().append((char) b);
      }
    }

    @Override
    public void write(byte[] b, int off, int len) {
      StringBuilder buf = buffer.get();
      String s = new String(b, off, len);
      for (int i = 0; i < s.length(); i++) {
        char c = s.charAt(i);
        if (c == '\n') {
          flush();
        } else {
          buf.append(c);
        }
      }
    }

    @Override
    public void flush() {
      StringBuilder buf = buffer.get();
      if (buf.length() > 0) {
        String level = "stderr".equals(name) ? "error" : "info";
        send(TestProtocol.encodeLog(currentSuite.get(), level, buf.toString()));
        buf.setLength(0);
      }
    }
  }
}
