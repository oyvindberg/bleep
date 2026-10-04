package bleep.machine

/** Why a server gives back memory that none of its own work asked it to (design §5.1, §5.2): the OS is already reclaiming, or another live server has published
  * that it could not admit something. Low memory with nobody waiting is neither — a warm server is not thrown away for nothing.
  *
  * The same trigger drives both responses: a busy server sheds the caches of its idle workspaces; an idle server, idle for long enough, yields itself.
  */
sealed trait MemoryNeed {
  def describe: String
}

object MemoryNeed {

  /** The OS says so: `Elevated` or `Critical` (design §9). */
  case class UnderPressure(level: Pressure) extends MemoryNeed {
    def describe: String = s"memory pressure is ${Pressure.name(level)}"
  }

  /** Another server's `state.json` has `wantsMore` (design §6.2), by pid. */
  case class OthersWantMore(pids: List[Long]) extends MemoryNeed {
    def describe: String = s"server${if (pids.size == 1) "" else "s"} ${pids.mkString(", ")} want${if (pids.size == 1) "s" else ""} more memory"
  }

  /** Pressure first, since it is the stronger signal; then whoever is waiting. `others` is every other live server as last read. */
  def of(pressure: Pressure, others: List[StateJson]): Option[MemoryNeed] =
    pressure match {
      case Pressure.Elevated | Pressure.Critical                        => Some(UnderPressure(pressure))
      case Pressure.Normal | Pressure.NoSignal(_) | Pressure.Warming(_) =>
        val wanting = others.filter(_.wantsMore).map(_.pid).sorted
        if (wanting.isEmpty) None else Some(OthersWantMore(wanting))
    }
}
