package bleep.internal

import scala.annotation.nowarn

/** A thread's id. `Thread.threadId()` only exists from JDK 19, and the compile server runs on the build's own JVM, which can be JDK 17. `getId` returns the
  * same value on every JDK; it is deprecated from 19 only because a subclass could override it, which `threadId` is final against.
  */
object threadId {
  @nowarn("cat=deprecation")
  def apply(t: Thread): Long = t.getId
}
