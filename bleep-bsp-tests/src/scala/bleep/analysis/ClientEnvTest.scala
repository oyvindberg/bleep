package bleep.analysis

import bleep.bsp.protocol.BleepBspProtocol.ClientEnv
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** The client's shell environment is forwarded wholesale to forked test processes. A handful of variables must not make that trip, because the launcher of the
  * forked process owns them and a forwarded copy silently corrupts it.
  */
class ClientEnvTest extends AnyFunSuite with Matchers {

  test("CLASSPATH is dropped — JvmPool uses it as the long-classpath channel on Windows") {
    ClientEnv.capture(Map("CLASSPATH" -> "/whatever")) shouldBe empty
  }

  test("shell cwd bookkeeping is dropped — the fork's cwd is set per-project, not inherited") {
    ClientEnv.capture(Map("PWD" -> "/home/me", "OLDPWD" -> "/tmp", "_" -> "/usr/bin/bleep")) shouldBe empty
  }

  test("git repository-location variables are dropped — e.g. set by `git rebase --exec`, they must not redirect a fork's own git calls") {
    val env = Map(
      "GIT_DIR" -> "/repo/.git/worktrees/other",
      "GIT_WORK_TREE" -> "/repo",
      "GIT_INDEX_FILE" -> "/repo/.git/index",
      "GIT_OBJECT_DIRECTORY" -> "/repo/.git/objects",
      "GIT_ALTERNATE_OBJECT_DIRECTORIES" -> "/other/.git/objects",
      "GIT_COMMON_DIR" -> "/repo/.git",
      "GIT_CEILING_DIRECTORIES" -> "/repo"
    )
    ClientEnv.capture(env) shouldBe empty
  }

  test("GIT_ filtering is by prefix, not an enumerated list — a variable git doesn't even ship yet is still caught") {
    ClientEnv.capture(Map("GIT_SOME_FUTURE_VARIABLE" -> "x")) shouldBe empty
  }

  test("PATH and JAVA_HOME ARE forwarded — tests that shell out should find the developer's tools") {
    val env = Map("PATH" -> "/usr/bin", "JAVA_HOME" -> "/jdk")
    ClientEnv.capture(env) shouldBe env
  }

  test("ordinary variables pass through untouched") {
    val env = Map("DATABASE_URL" -> "postgres://localhost/test", "CI" -> "true", "NO_COLOR" -> "1")
    ClientEnv.capture(env) shouldBe env
  }

  test("filtering is exact-match, not prefix — CLASSPATH_EXTRA is a normal variable") {
    val env = Map("CLASSPATH_EXTRA" -> "x", "MY_CLASSPATH" -> "y")
    ClientEnv.capture(env) shouldBe env
  }

  test("capture of a real environment keeps something and drops nothing unexpected") {
    val captured = ClientEnv.capture(sys.env)
    captured.keySet.exists(ClientEnv.isDenied) shouldBe false
    captured.keySet shouldBe sys.env.keySet.filterNot(ClientEnv.isDenied)
  }
}
