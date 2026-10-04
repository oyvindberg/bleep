package bleep.machine

/** The rate of a cumulative page counter — macOS's compressions plus decompressions — smoothed over about ten seconds (design §9).
  *
  * Why a rate, and why smoothed: the owner's ground truth for "the machine is overloaded" is kernel_task above 90 % CPU, and on a 48 GB Mac driven there the
  * used memory did not move (42.4–42.8 GB throughout — anonymous pages became compressor pages), the kernel's pressure level went to 2 forty seconds late, and
  * the only figure that tracked it was the compressor's churn: 6k–50k pages/s calm, 98k/s at 95 % kernel_task, 236k/s at 111 %, 338k/s at 160 %. The raw
  * counter difference between two ticks 10 ms apart is a few pages or a burst of thousands, so it must be averaged.
  *
  * An exponentially weighted average with a ten-second time constant, weighted by the interval between samples: `alpha = 1 − exp(−dt/τ)`. It is one number of
  * state whatever the cadence (a 10 ms tick moves it by a thousandth, a 3 s slow check by a quarter), it needs no window of samples, and a step in the rate
  * reaches 63 % of its new value in ten seconds and 95 % in thirty — at 98k/s from a 50k/s calm it crosses the 75k/s threshold in about fourteen seconds, well
  * inside the kernel level's forty. A counter that went backwards (a wrap, a reboot of the statistics) is taken as a new baseline and rates nothing until the
  * next sample; two samples at the same instant rate nothing either.
  */
object Churn {

  /** Time constant of the average. Calibrated from one overload test on the owner's machine (design §9); a named value for that reason. */
  val TimeConstantMs: Long = 10_000L

  sealed trait Rate
  object Rate {

    /** One sample so far, or a counter that just went backwards: no rate yet. Modelled rather than read as zero, because zero would read as calm. */
    case object Unknown extends Rate
    case class PagesPerSecond(value: Double) extends Rate
  }

  /** The last sample and the average so far. */
  case class State(atMs: Long, pages: Long, ewma: Option[Double]) {
    def rate: Rate = ewma.fold[Rate](Rate.Unknown)(Rate.PagesPerSecond(_))
  }

  /** Fold one sample in. The first sample starts the baseline; a later one that is earlier in time or lower in count restarts it. */
  def update(state: Option[State], atMs: Long, pages: Long): State =
    state match {
      case None           => State(atMs, pages, None)
      case Some(previous) =>
        val dtMs = atMs - previous.atMs
        val dPages = pages - previous.pages
        if (dtMs <= 0L || dPages < 0L) State(atMs, pages, if (dPages < 0L) None else previous.ewma)
        else {
          val instantaneous = dPages.toDouble * 1000.0 / dtMs.toDouble
          val alpha = 1.0 - math.exp(-dtMs.toDouble / TimeConstantMs.toDouble)
          val next = previous.ewma match {
            case None      => instantaneous
            case Some(avg) => avg + alpha * (instantaneous - avg)
          }
          State(atMs, pages, Some(next))
        }
    }
}
