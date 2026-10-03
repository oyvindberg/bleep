package bleep.machine

import ryddig.TypedLogger

import java.nio.file.Path

/** A tiny process for [[MachineLockTest]]: takes `machine.lock` at `args(0)`, prints `LOCKED`, holds it for `args(1)` ms, prints `RELEASED`.
  *
  * A real second process rather than a thread, because the lock is an OS file lock whose whole point is what happens across processes — including when one of
  * them is killed or stopped.
  */
object LockHolderMain {
  def main(args: Array[String]): Unit = {
    val path = Path.of(args(0))
    val holdMs = args(1).toLong
    val lock = new FileMachineLock(path, StateFile.selfIdentity("lock-holder"), TypedLogger.DevNull)
    lock.locked(waitMs = 10_000L) { (state, _) =>
      state match {
        case LockState.Held =>
          System.out.println("LOCKED")
          System.out.flush()
          Thread.sleep(holdMs)
        case other =>
          System.out.println(s"NOT LOCKED: $other")
          System.out.flush()
          sys.exit(2)
      }
    }
    System.out.println("RELEASED")
    System.out.flush()
    lock.close()
  }
}
