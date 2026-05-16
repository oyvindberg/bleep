package bleep.bsp.protocol

/** Why a process / task / compile was killed.
  *
  * This is the explicit cancellation signal — when stopping something, complete a `Deferred[IO, KillReason]` with one of these. The killed code then surfaces
  * the reason in its outcome (e.g. `ProjectCompileCancelled(reason)`) so callers can distinguish a user-initiated cancel from a server shutdown.
  */
sealed trait KillReason
object KillReason {

  /** User requested cancellation (Ctrl-C, $/cancelRequest) */
  case object UserRequest extends KillReason

  /** Operation exceeded its time limit */
  case object Timeout extends KillReason

  /** Parent process / task is dying and taking children with it */
  case object ParentDying extends KillReason

  /** Server is shutting down */
  case object ServerShutdown extends KillReason

  /** Client connection died (failed to send notification) */
  case object DeadClient extends KillReason
}
