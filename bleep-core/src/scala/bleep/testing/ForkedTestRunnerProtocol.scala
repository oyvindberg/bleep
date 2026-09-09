package bleep.testing

/** Names shared between bleep and the forked test runner it launches.
  *
  * `bleep-test-runner` is plain Java with no dependency on the rest of bleep, so the two sides cannot share a constant directly. They are always the same
  * version — the client resolves the runner at its own `BleepVersion.current` — so a mismatch here is a build error waiting to happen rather than a
  * compatibility concern; keeping the string in one named place on this side is what makes it greppable from the other.
  */
object ForkedTestRunnerProtocol {

  /** System property carrying the loopback port the fork connects back on to speak the test protocol.
    *
    * Must match `bleep.testing.runner.ForkedTestRunner.PROTOCOL_PORT_PROPERTY`.
    */
  val PortProperty: String = "bleep.test.protocolPort"

  /** System property carrying the file the fork writes its exit diagnostic to (thread dump, whether the command loop had exited, which suites were still
    * running). A FILE, not fd 2: when a fork exits from under a run — a test's `System.exit`, or the JVM tearing down — whatever it writes to stderr races the
    * pipe closing and is routinely lost, which is exactly how "N suites never reported a result" ended up with no cause. A file survives that teardown, and the
    * parent reads it after the fork dies. Its ABSENCE is itself a signal: a `Runtime.halt` (or a hard OS kill) runs no shutdown hooks, so no file is written.
    *
    * Must match `bleep.testing.runner.ForkedTestRunner.EXIT_LOG_PROPERTY`.
    */
  val ExitLogProperty: String = "bleep.test.exitLog"
}
