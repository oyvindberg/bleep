package bleep

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.Path

/** StateSharing is the single allow-list both copy-state and the remote cache derive their filters from. These tests pin the deny-by-default semantics: the
  * shareable set is exactly {classes, test-classes, .zinc/analysis.zip}, and everything else — including things a deny-list historically leaked (zinc cache,
  * KSP caches, lock files) and anything invented in the future — stays workspace-private without anyone remembering to exclude it.
  */
class StateSharingTest extends AnyFunSuite with Matchers {

  test("the shareable set: compiled output and the portable analysis") {
    StateSharing.isShareableRel("classes") shouldBe true
    StateSharing.isShareableRel("classes/com/example/Main.class") shouldBe true
    StateSharing.isShareableRel("test-classes/SomeTest.class") shouldBe true
    StateSharing.isShareableRel(".zinc/analysis.zip") shouldBe true
  }

  test("deny by default: everything the old deny-list leaked, and anything new, is private") {
    // what the remote cache used to ship because only noop-manifest.bin was denied by name
    StateSharing.isShareableRel(".zinc/cache/some-classfile-cache-entry") shouldBe false
    StateSharing.isShareableRel("ksp/caches/lookups.bin") shouldBe false
    StateSharing.isShareableRel(".bleep-lock") shouldBe false
    StateSharing.isShareableRel("bloop.json") shouldBe false
    // what both mechanisms always excluded
    StateSharing.isShareableRel(".zinc/noop-manifest.bin") shouldBe false
    // prefix confusion must not leak: "classes-something" is not "classes/"
    StateSharing.isShareableRel("classes-backup/Main.class") shouldBe false
    // the future: unknown state is private until declared
    StateSharing.isShareableRel("some/new/state-dir/file") shouldBe false
  }

  test("isShareableIn relativizes against the variant dir, tolerating platform separators") {
    val variantDir = Path.of("/ws/.bleep/projects/app/builds/normal")
    StateSharing.isShareableIn(variantDir)(variantDir.resolve("classes").resolve("A.class")) shouldBe true
    StateSharing.isShareableIn(variantDir)(variantDir.resolve(".zinc").resolve("analysis.zip")) shouldBe true
    StateSharing.isShareableIn(variantDir)(variantDir.resolve(".zinc").resolve("noop-manifest.bin")) shouldBe false
    StateSharing.isShareableIn(variantDir)(variantDir.resolve(".bleep-lock")) shouldBe false
  }
}
