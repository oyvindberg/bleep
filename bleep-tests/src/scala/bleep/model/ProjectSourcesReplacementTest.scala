package bleep.model

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.io.File
import java.nio.file.Path

class ProjectSourcesReplacementTest extends AnyFunSuite with Matchers {
  test("${PROJECT_SOURCES} fills in every source directory, joined as a -sourcepath wants them") {
    val dirs = List(Path.of("/b/library/src"), Path.of("/b/library/src-extra"), Path.of("/b/.bleep/projects/lib/generated-sources/gen"))
    val opts = Options(Set(Options.Opt.WithArgs("-sourcepath", List(Replacements.known.ProjectSources))))
    Replacements.projectSources(dirs).fill.opts(opts).render shouldBe List("-sourcepath", dirs.mkString(File.pathSeparator))
  }
}
