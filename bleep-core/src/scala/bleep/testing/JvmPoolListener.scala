package bleep.testing

/** What the pool did with its forked JVMs, reported to whoever cares.
  *
  * The pool lives in bleep-core and the telemetry lives in bleep-bsp, which depends on it — so the pool cannot call the recorder, and a singleton it could
  * reach would be exactly the global mutable state this codebase refuses. The dependency is inverted instead: the pool announces, and whoever constructs it
  * decides whether anything listens.
  *
  * What this makes answerable, none of which was recorded anywhere before: how many JVMs a test run actually started, whether they were reused or respawned,
  * how long each lived, which of them were killed rather than finishing, and — with [[bleep.bsp.TestRunner]]'s suite events, keyed on the same pid — which
  * suite ran on which JVM. A slow or flaky test run is usually a question about one of those.
  *
  * Deliberately not `IO`: these are called from inside the pool's own bookkeeping, including a process's exit handler, and an effect there would either need an
  * unsafeRun at the call site or restructuring the pool around an effectful listener. They must be cheap and must not throw.
  */
trait JvmPoolListener {

  /** A JVM was forked. `label` is what it was acquired for — the suite name, in practice. */
  def onForkStart(pid: Long, label: String, xmxMb: Option[Long]): Unit

  /** A JVM the pool had is gone. `exit` is the human description the pool already computes, which distinguishes a fork bleep killed from one the OS killed —
    * both report exit 137 and mean very different things.
    */
  def onForkEnd(pid: Long, lifetimeMs: Long, exit: String, killedByUs: Option[String]): Unit

  /** A JVM already in the pool was handed out again instead of a new one being started. The ratio of this to [[onForkStart]] is how much the pool is actually
    * saving, which is otherwise invisible.
    */
  def onForkReused(pid: Long, label: String): Unit
}

object JvmPoolListener {
  val noop: JvmPoolListener = new JvmPoolListener {
    def onForkStart(pid: Long, label: String, xmxMb: Option[Long]): Unit = ()
    def onForkEnd(pid: Long, lifetimeMs: Long, exit: String, killedByUs: Option[String]): Unit = ()
    def onForkReused(pid: Long, label: String): Unit = ()
  }
}
