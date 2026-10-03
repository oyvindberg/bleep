package bleep.analysis

/** A zinc reporter that keeps what it is given, for tests that look at what a compiler reported. */
final class CollectingReporter extends xsbti.Reporter {
  private val logged = scala.collection.mutable.ArrayBuffer.empty[xsbti.Problem]

  def reset(): Unit = logged.clear()
  def hasErrors: Boolean = logged.exists(_.severity == xsbti.Severity.Error)
  def hasWarnings: Boolean = logged.exists(_.severity == xsbti.Severity.Warn)
  def printSummary(): Unit = ()
  def problems(): Array[xsbti.Problem] = logged.toArray
  def log(problem: xsbti.Problem): Unit = logged += problem
  def comment(pos: xsbti.Position, msg: String): Unit = ()
}
