package bleep.testing

import bleep.model.{CrossProjectName, SuiteName, TestName}

/** Typed key for a test suite within a project. */
case class SuiteKey(project: CrossProjectName, suite: SuiteName) extends Ordered[SuiteKey] {
  override def toString: String = s"${project.value}:${suite.value}"
  override def compare(that: SuiteKey): Int = {
    val c = project.value.compareTo(that.project.value)
    if (c != 0) c else suite.value.compareTo(that.suite.value)
  }
}

/** Typed key for an individual test within a suite. */
case class TestKey(project: CrossProjectName, suite: SuiteName, test: TestName) {
  def suiteKey: SuiteKey = SuiteKey(project, suite)
  override def toString: String = s"${project.value}:${suite.value}:${test.value}"
}
