package bleep.bsp

import bleep.bsp.protocol.ConnectionDto

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicLong
import scala.jdk.CollectionConverters._

/** Who is connected to this daemon, and which of them count as *use*.
  *
  * Before this, liveness was inferred from a set of client threads. That is fine while every connection is a real client, but `bleep server top` holds a
  * connection open purely to watch — and a watcher counted as a client would make the daemon immortal for as long as you looked at it. Reconnecting per poll is
  * no better: each connect refreshed the idle clock, so a 1Hz poll resets the idle window forever.
  *
  * So connections declare themselves. Observers are recorded and rendered, but they do not hold the daemon open and do not touch `lastActivityMs`. Watching
  * your servers never changes their lifecycle — if a daemon idles out while `top` is showing it, the row flips to dead, which is the truth and matches what the
  * log says.
  *
  * One instance per daemon, created in `runWithLock` and passed structurally through [[DaemonInfo]] — never a global.
  */
class ConnectionRegistry {

  private case class Entry(
      connId: Int,
      connectedAtEpochMs: Long,
      observer: Boolean,
      clientName: Option[String],
      clientVersion: Option[String],
      workspace: Option[String]
  )

  private val entries = new ConcurrentHashMap[Int, Entry]()

  /** Wall-clock of the last moment this daemon did anything for a *real* client. Observers deliberately never move it. */
  private val lastActivityMs = new AtomicLong(System.currentTimeMillis())

  def register(connId: Int, connectedAtEpochMs: Long): Unit = {
    entries.put(connId, Entry(connId, connectedAtEpochMs, observer = false, clientName = None, clientVersion = None, workspace = None)): Unit
    touch()
  }

  /** Called when a connection's first admin request declares `observer: true`. Idempotent. */
  def markObserver(connId: Int): Unit =
    entries.computeIfPresent(connId, (_, entry) => entry.copy(observer = true)): Unit

  /** Called at `build/initialize`: this connection is a real client doing real work. */
  def markClient(connId: Int, clientName: Option[String], clientVersion: Option[String], workspace: Option[String]): Unit = {
    entries.computeIfPresent(
      connId,
      (_, entry) => entry.copy(observer = false, clientName = clientName, clientVersion = clientVersion, workspace = workspace)
    ): Unit
    touch()
  }

  def unregister(connId: Int): Unit = {
    val removed = Option(entries.remove(connId))
    // Reset the idle clock as a real client leaves, so a daemon that just finished a long session gets the full idle window before being reaped. An observer
    // leaving must not grant that extension.
    if (removed.exists(!_.observer)) touch()
  }

  /** How many connections are real clients. The idle watchdog reaps only when this is zero. */
  def nonObserverCount: Int = entries.values().asScala.count(!_.observer)

  def idleForMs(nowMs: Long): Long = nowMs - lastActivityMs.get()

  def snapshot: List[ConnectionDto] =
    entries
      .values()
      .asScala
      .toList
      .sortBy(_.connId)
      .map(e => ConnectionDto(e.connId, e.connectedAtEpochMs, e.observer, e.clientName, e.clientVersion, e.workspace))

  private def touch(): Unit = lastActivityMs.set(System.currentTimeMillis())
}
