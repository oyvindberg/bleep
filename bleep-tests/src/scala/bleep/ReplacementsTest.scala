package bleep

import org.scalactic.TripleEqualsSupport
import org.scalatest.funsuite.AnyFunSuite

class ReplacementsTest extends AnyFunSuite with TripleEqualsSupport {
  val mainScope = model.Replacements.scope("main")

  test("infers templates on whole path segment") {
    val pre = RelPath.force("src/main/scala")
    val post = mainScope.templatize.relPath(pre)
    assert(post === RelPath.force("src/${SCOPE}/scala"))
  }

  test("does not infer template for partial matches in paths") {
    val pre = RelPath.force("src/amain/scala")
    val post = mainScope.templatize.relPath(pre)
    assert(post === pre)
  }

  test("infer template after between special symbols") {
    val pre = RelPath.force("src/a-main_b/scala")
    val post = mainScope.templatize.relPath(pre)
    assert(post === RelPath.force("src/a-${SCOPE}_b/scala"))
  }

  test("unconditionally fills values") {
    val pre = RelPath.force(s"src/a$${SCOPE}b/scala")
    val post = mainScope.fill.relPath(pre)
    assert(post === RelPath.force("src/amainb/scala"))
  }
}
