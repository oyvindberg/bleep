package bleep

import bleep.internal.{FileUtils, YamlComments}
import bleep.rewrites.normalizeBuild
import org.scalactic.TripleEqualsSupport
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Path}

class YamlCommentsTest extends AnyFunSuite with TripleEqualsSupport {
  val prelude = """$schema: https://raw.githubusercontent.com/oyvindberg/bleep/master/schema.json
                 |$version: dev
                 |""".stripMargin

  def buildFile(yamlString: String): model.BuildFile =
    BuildLoader.Existing(Path.of("bleep.yaml"), Lazy(Right(yamlString))).buildFile.forceGet.orThrow

  /** Write `after` (a build, as YAML without comments) over `before` (with comments). */
  def rewrite(before: String, after: String): YamlComments.Printed =
    yaml.encodeShortenedKeepingComments(buildFile(after), before)

  test("comments stay where they were") {
    val before =
      """# header, above $schema
        |$schema: https://raw.githubusercontent.com/oyvindberg/bleep/master/schema.json
        |$version: dev
        |projects:
        |  # above project a
        |  a:
        |    dependencies:
        |    # above fansi
        |    - com.lihaoyi::fansi:0.3.1 # after fansi
        |    # above the object form
        |    - configuration: provided
        |      module: org.graalvm.sdk:nativeimage:25.0.1
        |
        |    # after a blank line
        |    platform:
        |      name: jvm # after jvm
        |# end of document
        |""".stripMargin
    val printed = rewrite(before, before)
    assert(printed.orphans === Nil)
    assert(printed.yaml === before)
  }

  test("a dependency keeps its comment when its version changes and the list is re-sorted") {
    val before = prelude ++
      """projects:
        |  a:
        |    dependencies:
        |    # pinned: 0.4 breaks the parser
        |    - com.lihaoyi::fastparse:3.0.0
        |    - com.lihaoyi::fansi:0.3.1
        |""".stripMargin
    val after = prelude ++
      """projects:
        |  a:
        |    dependencies:
        |    - com.lihaoyi::fansi:0.3.1
        |    - com.lihaoyi::fastparse:3.1.1
        |""".stripMargin
    val printed = rewrite(before, after)
    assert(printed.orphans === Nil)
    assert(
      printed.yaml === prelude ++
        """projects:
          |  a:
          |    dependencies:
          |    - com.lihaoyi::fansi:0.3.1
          |    # pinned: 0.4 breaks the parser
          |    - com.lihaoyi::fastparse:3.1.1
          |""".stripMargin
    )
  }

  test("a dependency hoisted into a template takes its comment along, once") {
    val before = prelude ++
      """projects:
        |  a:
        |    dependencies:
        |    # needed for colours
        |    - com.lihaoyi::fansi:0.3.1
        |  b:
        |    dependencies:
        |    # needed for colours
        |    - com.lihaoyi::fansi:0.3.1
        |""".stripMargin
    val after = prelude ++
      """projects:
        |  a:
        |    extends: common
        |  b:
        |    extends: common
        |templates:
        |  common:
        |    dependencies: com.lihaoyi::fansi:0.3.1
        |""".stripMargin
    // a single dependency prints as a scalar rather than a one-item list, so it has no item to carry the comment
    val printed = rewrite(before, after)
    assert(printed.orphans.map(_.comments.render) === List("# needed for colours", "# needed for colours"))

    val afterTwo = prelude ++
      """projects:
        |  a:
        |    extends: common
        |  b:
        |    extends: common
        |templates:
        |  common:
        |    dependencies:
        |    - com.lihaoyi::fansi:0.3.1
        |    - com.lihaoyi::pprint:0.9.6
        |""".stripMargin
    val printedTwo = rewrite(before, afterTwo)
    assert(printedTwo.orphans === Nil)
    assert(
      printedTwo.yaml === prelude ++
        """projects:
          |  a:
          |    extends: common
          |  b:
          |    extends: common
          |templates:
          |  common:
          |    dependencies:
          |    # needed for colours
          |    - com.lihaoyi::fansi:0.3.1
          |    - com.lihaoyi::pprint:0.9.6
          |""".stripMargin
    )
  }

  test("comment lines indented under an item continue that item, and follow it when sorted") {
    val before = prelude ++
      """projects:
        |  a:
        |    dependencies:
        |    - org.scalameta:svm-subs:101.0.0
        |      # note: weird binary incompatibility
        |      # when bumping this for scala3
        |    # about slf4j
        |    - org.slf4j:slf4j-api:2.0.17
        |    - com.lihaoyi::fansi:0.3.1
        |""".stripMargin
    val printed = rewrite(before, before)
    assert(printed.orphans === Nil)
    assert(
      printed.yaml === prelude ++
        """projects:
          |  a:
          |    dependencies:
          |    - com.lihaoyi::fansi:0.3.1
          |    - org.scalameta:svm-subs:101.0.0 # note: weird binary incompatibility
          |                                     # when bumping this for scala3
          |    # about slf4j
          |    - org.slf4j:slf4j-api:2.0.17
          |""".stripMargin
    )
  }

  test("a comment whose place is gone comes back as an orphan") {
    val before = prelude ++
      """projects:
        |  a: {}
        |  # b is going away
        |  b: {}
        |""".stripMargin
    val after = prelude ++
      """projects:
        |  a: {}
        |""".stripMargin
    val printed = rewrite(before, after)
    assert(printed.yaml === after)
    assert(printed.orphans.map(o => (o.anchor.render, o.comments.render)) === List(("projects/b (key)", "# b is going away")))
  }

  test("bleep's own bleep.yaml keeps every comment through normalize") {
    val existing = BuildLoader.find(FileUtils.cwd) match {
      case e: BuildLoader.Existing => e
      case other                   => fail(s"no bleep.yaml above ${FileUtils.cwd}: $other")
    }
    val source = Files.readString(existing.bleepYaml)
    val buildPaths = BuildPaths(FileUtils.cwd, existing, model.BuildVariant.Normal)
    val normalized = normalizeBuild(model.Build.FileBacked(existing.buildFile.forceGet.orThrow), buildPaths)

    val printed = yaml.encodeShortenedKeepingComments(normalized.file, source)
    def commentLines(s: String): List[String] =
      YamlComments.extract(s).byAnchor.values.toList.flatMap(c => c.block ++ c.inLine ++ c.end).collect {
        case YamlComments.Line.Block(text)  => text
        case YamlComments.Line.InLine(text) => text
      }

    assert(printed.orphans === Nil)
    assert(commentLines(printed.yaml).sorted === commentLines(source).sorted)
    // the comments are the only difference from the printer without them
    assert(yaml.parse(printed.yaml) === yaml.parse(yaml.encodeShortened(normalized.file)))
    // idempotent: writing the result over itself changes nothing
    assert(yaml.encodeShortenedKeepingComments(normalized.file, printed.yaml).yaml === printed.yaml)
  }
}
