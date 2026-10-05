package bleep.machine

/** A process that lives for `args(0)` milliseconds and exits 0: what tests start under a scheduler grant when they need a real pid to measure or kill. */
object SleepMain {
  def main(args: Array[String]): Unit = {
    Thread.sleep(args(0).toLong)
    System.out.println("SLEPT")
  }
}
