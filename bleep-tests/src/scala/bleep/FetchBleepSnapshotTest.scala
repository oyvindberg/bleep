package bleep

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class FetchBleepSnapshotTest extends AnyFunSuite with Matchers {

  private def sha(v: String) = FetchBleepSnapshot.shaOf(model.BleepVersion(v))

  test("the sha is taken from a dynver snapshot") {
    sha("1.0.0-M10+83-fbe31c09-SNAPSHOT") shouldBe Some("fbe31c09")
  }

  test("a dirty working tree appends a timestamp after the sha, which is not part of it") {
    // Seen for real while testing this: an uncommitted change turns the version into this shape, and taking everything
    // after the dash would have produced a sha no run ever had.
    sha("1.0.0-M10+82-c6df0b78+20260731-1016-SNAPSHOT") shouldBe Some("c6df0b78")
  }

  test("a snapshot without the -SNAPSHOT suffix still yields its sha") {
    sha("1.0.0-M10+83-fbe31c09") shouldBe Some("fbe31c09")
  }

  test("a release has no sha, so there is no run to look for") {
    sha("1.0.0-M9") shouldBe None
    sha("1.0.0-M10") shouldBe None
  }

  test("isDevelopment and shaOf agree about what a snapshot is") {
    // The router in FetchBleepRelease switches on isDevelopment; if these two disagreed, a version would be sent to the
    // snapshot path and then rejected there for having no sha.
    val snapshots = List("1.0.0-M10+83-fbe31c09-SNAPSHOT", "1.0.0-M10+1-abcdef12-SNAPSHOT")
    val releases = List("1.0.0-M9", "0.0.1-M7")
    snapshots.foreach { v =>
      withClue(v)(model.BleepVersion(v).isDevelopment shouldBe true)
      withClue(v)(sha(v) should not be empty)
    }
    releases.foreach { v =>
      withClue(v)(model.BleepVersion(v).isDevelopment shouldBe false)
      withClue(v)(sha(v) shouldBe None)
    }
  }

  test("every platform with a native image maps to an artifact name build.yml uploads") {
    // These must stay in step with `matrix.artifact_name`; a typo here is only discovered by someone trying a snapshot.
    val expected = Set(
      "bleep-arm64-apple-darwin",
      "bleep-x86_64-apple-darwin",
      "bleep-x86_64-pc-linux",
      "bleep-arm64-pc-linux",
      "bleep-x86_64-pc-win32"
    )
    val actual = List(
      OsArch.MacosArm64(freedFromJail = false),
      OsArch.MacosAmd64,
      OsArch.LinuxAmd64,
      OsArch.LinuxArm64,
      OsArch.WindowsAmd64
    ).map(FetchBleepSnapshot.artifactNameFor)

    actual.collect { case Left(err) => err } shouldBe empty
    actual.collect { case Right(name) => name }.toSet shouldBe expected
  }
}
