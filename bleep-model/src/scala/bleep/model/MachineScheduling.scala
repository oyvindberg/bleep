package bleep.model

import io.circe.{Decoder, Encoder}

/** How a compile server takes part in machine-wide scheduling (design §9.1).
  *
  *   - [[Cooperative]] (the default): the server reads the machine's memory, publishes its forks in `state.json`, takes `machine.lock` to claim memory, and
  *     admits forks only while they fit alongside every other server's.
  *   - [[Unconstrained]]: the same scheduler without the machine-wide parts — no probes, no `state.json`, no lock, no memory room, no pressure brake. What
  *     remains is per-server `parallelism`, the heap gate, warm-fork reuse and idle eviction, one spawn per tick, and the guarantee of one fork per command.
  *
  * A server also runs unconstrained, with a loud warning, where bleep has no memory probe for the OS and architecture.
  */
sealed abstract class MachineScheduling(val value: String)

object MachineScheduling {
  case object Cooperative extends MachineScheduling("cooperative")
  case object Unconstrained extends MachineScheduling("unconstrained")

  val all: List[MachineScheduling] = List(Cooperative, Unconstrained)

  def parse(value: String): Either[String, MachineScheduling] =
    all.find(_.value == value).toRight(s"machineScheduling must be one of ${all.map(_.value).mkString(", ")}, got '$value'")

  implicit val decoder: Decoder[MachineScheduling] = Decoder.decodeString.emap(parse)
  implicit val encoder: Encoder[MachineScheduling] = Encoder.encodeString.contramap(_.value)
}
