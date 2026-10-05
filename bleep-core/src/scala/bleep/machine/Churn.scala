package bleep.machine

/** The rate of a cumulative page counter — macOS's compressions plus decompressions — over the last few seconds (design §9).
  *
  * Why a rate: the owner's ground truth for "the machine is overloaded" is kernel_task above 90 % CPU, and on a 48 GB Mac driven there the used memory did not
  * move, the kernel's pressure level came forty seconds late, and the only figure that tracked it was the compressor's churn. Why a *short* window: the machine
  * has a cliff, not a slope — in 1 GB steps, +6 GB held read 20k pages/s and 6 % kernel_task, +7 GB read 243k/s and 154 % — and a ten-second exponential
  * average read 91k at that moment, reacting after the damage. The estimator must show a cliff within one sampling interval and still not flap on a single
  * burst.
  *
  * So: the mean rate over a window of [[WindowMs]] (2.5 s) — the counter's difference between the newest sample and the oldest one still inside the window,
  * over the time between them. At 10 ms ticks the window holds a few seconds of samples (thinned to one per [[MinSpacingMs]], so at most ~25 are kept), and a
  * single 10 ms burst of X pages moves the rate by X/2.5 — to lift a calm 5k/s above the 25k/s threshold a burst would have to be 50k pages, 800 MB, which is
  * compression taking off, not noise. At 3 s slow checks the window holds no earlier sample, so the previous sample is kept as the anchor and the rate is the
  * raw rate over that interval: a cliff is seen at the very next check. A counter that went backwards (a wrap, a reboot of the statistics) is a new baseline
  * and rates nothing until the next sample; two samples at the same instant rate nothing either.
  */
object Churn {

  /** How far back the rate looks. Calibrated from the owner's two runs (design §9): long enough to average a tick's burstiness, short enough that the next slow
    * check after a cliff shows it in full.
    */
  val WindowMs: Long = 2500L

  /** Samples closer together than this replace the newest rather than accumulating, so a 10 ms cadence does not keep 250 of them. */
  val MinSpacingMs: Long = 100L

  sealed trait Rate
  object Rate {

    /** One sample so far, or a counter that just went backwards: no rate yet. Modelled rather than read as zero, because zero would read as calm. */
    case object Unknown extends Rate
    case class PagesPerSecond(value: Double) extends Rate
  }

  /** The samples kept, oldest first: the anchor (the newest sample at or before `now − WindowMs`, or the oldest kept) and everything after it. */
  case class State(samples: Vector[(Long, Long)]) {
    def rate: Rate =
      if (samples.size < 2) Rate.Unknown
      else {
        val (t0, p0) = samples.head
        val (t1, p1) = samples.last
        if (t1 <= t0) Rate.Unknown else Rate.PagesPerSecond((p1 - p0).toDouble * 1000.0 / (t1 - t0).toDouble)
      }
  }

  /** Fold one sample in. The first sample starts the baseline; a later one that is earlier in time or lower in count restarts it. */
  def update(state: Option[State], atMs: Long, pages: Long): State =
    state match {
      case None                 => State(Vector((atMs, pages)))
      case Some(State(samples)) =>
        val (lastAt, lastPages) = samples.last
        if (atMs <= lastAt || pages < lastPages) {
          if (pages < lastPages || atMs < lastAt) State(Vector((atMs, pages))) // wrap or clock step: new baseline
          else State(samples) // same instant: nothing to add
        } else {
          val appended = if (atMs - lastAt < MinSpacingMs && samples.size >= 2) samples.init :+ ((atMs, pages)) else samples :+ ((atMs, pages))
          // Keep the newest sample at or before the window's start as the anchor, and drop everything older than it.
          val windowStart = atMs - WindowMs
          val anchor = appended.lastIndexWhere { case (t, _) => t <= windowStart }
          State(if (anchor <= 0) appended else appended.drop(anchor))
        }
    }
}
