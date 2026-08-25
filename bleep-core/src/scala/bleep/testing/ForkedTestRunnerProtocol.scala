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
}
