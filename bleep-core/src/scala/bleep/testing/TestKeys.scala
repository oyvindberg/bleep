package bleep.testing

/** Typed key for a test suite within a project. Replaces stringly-typed "project:suite" composite keys. */
case class SuiteKey(project: String, suite: String) extends Ordered[SuiteKey] {
  override def toString: String = s"$project:$suite"
  override def compare(that: SuiteKey): Int = {
    val c = project.compareTo(that.project)
    if (c != 0) c else suite.compareTo(that.suite)
  }
}

/** Typed key for an individual test within a suite. Replaces stringly-typed "project:suite:test" composite keys. */
case class TestKey(project: String, suite: String, test: String) {
  def suiteKey: SuiteKey = SuiteKey(project, suite)
  override def toString: String = s"$project:$suite:$test"
}
