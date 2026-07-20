package bleep.testing

import bleep.MachineResources
import cats.effect.{IO, Ref}

/** What a fork of a given kind has actually been observed to cost, so the next one can be admitted on evidence instead of on its `-Xmx`.
  *
  * A fork's declared heap bound is a ceiling, not a prediction, and treating the two as the same thing is what made the governor useless: measured on a real
  * build, the median fork cost 610MB while being charged 2560MB — a 4.2x overstatement, which is to say the budget "filled up" at a quarter of the machine's
  * real capacity. Since the fleet is homogeneous (a project's suites share a classpath, hence a [[JvmPool]] key), the first fork of a kind teaches us what the
  * rest will cost.
  *
  * Estimates only ever go up. A suite that peaked at 3GB once will be admitted as a 3GB fork forever after, even if the next run is lighter. That asymmetry is
  * deliberate: under-charging risks over-committing the machine, and the cost of over-charging is only some lost parallelism. The model is per-server-process
  * and deliberately not persisted — a stale number on disk that nobody can see would be worse than re-learning it in the first few seconds of a run.
  */
trait ForkCostModel {

  /** What to charge a fork of this kind, in MB. Falls back to the footprint implied by its heap bound until something has been observed. */
  def estimateMb(key: String, heapBoundMb: Long): IO[Long]

  /** Record an observed cost. Keeps the high-water mark. */
  def observe(key: String, footprintMb: Long): IO[Unit]

  /** For logging/debugging: everything learned so far. */
  def learned: IO[Map[String, Long]]
}

object ForkCostModel {

  /** Never charge less than this, however light a fork looked. A JVM that measured small once can still spike, and admitting an unbounded number of "free"
    * forks would defeat the budget entirely.
    */
  val FloorMb: Long = 256L

  def create: IO[ForkCostModel] =
    Ref.of[IO, Map[String, Long]](Map.empty).map(fromRef)

  def fromRef(ref: Ref[IO, Map[String, Long]]): ForkCostModel =
    new ForkCostModel {
      def estimateMb(key: String, heapBoundMb: Long): IO[Long] =
        ref.get.map { learned =>
          learned.get(key) match {
            // Observed: trust it, but never below the floor.
            case Some(observed) => math.max(FloorMb, observed)
            // Unobserved: the only thing we know is the bound it will be held to. Conservative by
            // construction, and it stops being used for this kind of fork after the first one runs.
            case None => MachineResources.forkFootprintMb(heapBoundMb)
          }
        }

      def observe(key: String, footprintMb: Long): IO[Unit] =
        if (footprintMb <= 0) IO.unit
        else ref.update(m => m.updated(key, math.max(m.getOrElse(key, 0L), footprintMb)))

      def learned: IO[Map[String, Long]] = ref.get
    }

  /** A model that never learns — for platforms where [[bleep.ProcessMemory]] can't measure (Windows), and for tests that want the old fixed behaviour. Always
    * charges the bound's footprint, which is exactly what the pool did before measurement existed.
    */
  val static: ForkCostModel =
    new ForkCostModel {
      def estimateMb(key: String, heapBoundMb: Long): IO[Long] = IO.pure(MachineResources.forkFootprintMb(heapBoundMb))
      def observe(key: String, footprintMb: Long): IO[Unit] = IO.unit
      def learned: IO[Map[String, Long]] = IO.pure(Map.empty)
    }
}
