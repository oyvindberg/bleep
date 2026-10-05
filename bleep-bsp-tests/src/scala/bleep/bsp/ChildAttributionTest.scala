package bleep.bsp

import bleep.bsp.ChildAttribution.{Child, Claim}
import bleep.machine.ForkId
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.Path

/** The rule that tells a Scala Native link's clang from everything else the server has spawned (design §5.3), on fake process trees. */
class ChildAttributionTest extends AnyFunSuite with Matchers {
  private val linkA = Path.of("/ws/a/.bleep/builds/normal/app/link-output/native/native-work").toAbsolutePath.normalize()
  private val linkB = Path.of("/ws/b/.bleep/builds/normal/lib/link-output/native/native-work").toAbsolutePath.normalize()
  private val claims = List(Claim(ForkId(1L), linkA), Claim(ForkId(2L), linkB))

  // What 0.5.6's LLVM.scala runs: compile = clang -c <in> -o <out> flags, link = clang++ @<workDir>/llvmLinkInfo, dsymutil <buildPath>.
  private val compileA = Child(101L, Some("/usr/bin/clang"), List("/usr/bin/clang", "-c", s"$linkA/native/Main.ll", "-o", s"$linkA/native/Main.ll.o", "-O0"))
  private val linkStepA = Child(102L, Some("/usr/bin/clang++"), List("/usr/bin/clang++", s"@$linkA/native/llvmLinkInfo"))
  private val dsymB = Child(201L, Some("/usr/bin/dsymutil"), List("/usr/bin/dsymutil", s"$linkB/native/lib-out"))
  private val testJvm = Child(301L, Some("/opt/jvm/bin/java"), List("java", "-cp", "/x/a.jar", "bleep.testing.runner.ForkedTestRunner"))
  private val git = Child(401L, Some("/usr/bin/git"), List("git", "status", "--porcelain"))

  test("a child naming a path under a claimed directory belongs to that claim, whichever tool it is") {
    val result = ChildAttribution.attribute(List(compileA, linkStepA, dsymB), known = Set.empty, claims)
    result.byFork shouldBe Map(ForkId(1L) -> Set(101L, 102L), ForkId(2L) -> Set(201L))
    result.unattributed shouldBe Nil
  }

  test("a process already registered as a fork's is not a candidate, even if it named a claimed path") {
    val suspicious = testJvm.copy(commandLine = testJvm.commandLine :+ s"$linkA/anything")
    ChildAttribution.attribute(List(suspicious), known = Set(301L), claims).byFork shouldBe Map.empty
  }

  test("a child naming no claimed path is unattributed, reported rather than dropped") {
    val result = ChildAttribution.attribute(List(git, testJvm), known = Set.empty, claims)
    result.byFork shouldBe Map.empty
    result.unattributed shouldBe List(git, testJvm)
  }

  test("a sibling directory with a longer name is not under the claim") {
    val sibling = Child(501L, Some("clang"), List("clang", "-c", s"${linkA}2/x.ll"))
    ChildAttribution.attribute(List(sibling), known = Set.empty, claims).unattributed shouldBe List(sibling)
  }

  test("a path with forward slashes matches on any platform, as the toolchain writes them into its link-info file") {
    val forward = Child(601L, Some("clang"), List("clang", s"@${linkA.toString.replace('\\', '/')}/native/llvmLinkInfo"))
    ChildAttribution.attribute(List(forward), known = Set.empty, claims).byFork shouldBe Map(ForkId(1L) -> Set(601L))
  }

  test("a child naming paths under two claims is a bug, said loudly") {
    val both = Child(701L, Some("clang"), List("clang", s"$linkA/x.ll", "-o", s"$linkB/x.o"))
    an[IllegalStateException] should be thrownBy ChildAttribution.attribute(List(both), known = Set.empty, claims)
  }
}
