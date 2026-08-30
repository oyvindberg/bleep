package bleep

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

/** Renders `docs/appendix/test-framework-matrix.mdx` from [[TestFrameworkFixture]], and fails when the checked-in page no longer matches.
  *
  * The page is documentation *about the test suite*, so deriving it from the suite's own data is the only way it stays true. Every fact on it — which
  * frameworks exist, which platforms each supports, which versions are swept, whether a skip is expressible, how much of a failure reaches the report — is a
  * field the matrix itself reads when it runs. There is no second list to keep in step.
  *
  * That matters because the page it replaces claimed to be "generated from something that runs" while being maintained by hand, and had already drifted:
  * `kotlin.test` runs on Kotlin/JS and Kotlin/Native, and the table said JVM only.
  *
  * Run with `-Dbleep.regenerate.docs=true` (or `BLEEP_REGENERATE_DOCS=1`) to rewrite the page after changing a fixture.
  */
class TestFrameworkMatrixDocTest extends AnyFunSuite with Matchers {

  private val docPath: Path =
    Paths.get(System.getProperty("user.dir")).resolve("docs/appendix/test-framework-matrix.mdx")

  private val regenerate: Boolean =
    sys.props.get("bleep.regenerate.docs").contains("true") || sys.env.get("BLEEP_REGENERATE_DOCS").contains("1")

  /** The execution targets, in the order the page presents them.
    *
    * The JVM is split by source language rather than shown as one column. It is one runner, but that is bleep's view, not the reader's: someone arriving at
    * this page writes Java, or Kotlin, or Scala, and wants the frameworks available to *them*. The split also carries real information — how a framework
    * behaves under a failure is a property of the framework, and the frameworks divide cleanly by language.
    */
  private case class Target(title: String, platformId: String, language: Option[FixtureLanguage], scalaBinaryVersions: List[Option[String]])

  private val ScalaVersions = List(Some("3"), Some("2.13"), Some("2.12"))

  private val targets: List[Target] = List(
    Target("JVM / Java", "jvm", Some(FixtureLanguage.Java), ScalaVersions),
    Target("JVM / Kotlin", "jvm", Some(FixtureLanguage.Kotlin), ScalaVersions),
    Target("JVM / Scala", "jvm", Some(FixtureLanguage.Scala), ScalaVersions),
    Target("Scala.js", "js", None, ScalaVersions),
    Target("Kotlin/JS", "kotlin-js", None, List(None)),
    Target("Scala Native", "native", None, ScalaVersions),
    Target("Kotlin/Native", "kotlin-native", None, List(None))
  )

  private def supports(fixture: TestFrameworkFixture, target: Target): Boolean =
    target.language.forall(_ == fixture.language) &&
      target.scalaBinaryVersions.exists(sbv => fixture.supports(target.platformId, sbv, fixture.currentVersion))

  private def yesNo(b: Boolean): String = if (b) "✅" else ""

  /** The recording of a real run for this framework, played back on the page.
    *
    * Recorded by `asciinema` into `bleep-site/static/demos`, the same place and format the site's other demos use, so the colours a framework emits are shown
    * rather than described. A transcript with the escapes stripped would have been easier and would have thrown away the thing worth showing.
    */
  private def castSlug(f: TestFrameworkFixture, t: Target): String = s"${f.name.replace('.', '-')}-${t.platformId}"

  private def castFile(f: TestFrameworkFixture, t: Target): Path =
    Paths.get(System.getProperty("user.dir")).resolve(s"bleep-site/static/demos/testfw-${castSlug(f, t)}.cast")

  private def hasCast(f: TestFrameworkFixture, t: Target): Boolean = Files.exists(castFile(f, t))

  /** The terminal size a recording was made at, read from the recording itself.
    *
    * The player has to be told, and telling it a size the cast was not recorded at letterboxes the terminal or crops it. Hardcoding one size for all of them is
    * what cropped the build summary off the end of every video: these runs are 52 to 204 lines long, and a height that suits the shortest cuts the longest in
    * half. Each is now recorded at the height its own summary needs, so the number lives in the file and this reads it back rather than guessing.
    */
  private def castSize(f: TestFrameworkFixture, t: Target): (Int, Int) = {
    val header = Files.lines(castFile(f, t)).findFirst().orElseThrow(() => new RuntimeException(s"empty cast: ${castFile(f, t)}"))
    def field(name: String): Int =
      s""""$name"\\s*:\\s*(\\d+)""".r
        .findFirstMatchIn(header)
        .map(_.group(1).toInt)
        .getOrElse(sys.error(s"cast ${castFile(f, t)} has no '$name' in its header: $header"))
    (field("width"), field("height"))
  }

  /** The MDX import name for a recording. `kotlin.test` on `kotlin-js` becomes `kotlinTestKotlinJsCast`, since a JavaScript identifier carries neither dots nor
    * hyphens.
    */
  private def castVar(f: TestFrameworkFixture, t: Target): String =
    castSlug(f, t).split("[.-]").toList match {
      case head :: tail => s"$head${tail.map(_.capitalize).mkString}Cast"
      case Nil          => "cast"
    }

  private def render(): String = {
    val sb = new StringBuilder

    sb.append("""---
                |id: test-framework-matrix
                |title: Test framework support matrix
                |---
                |
                |{/* Generated by bleep.TestFrameworkMatrixDocTest from bleep-tests. Do not edit by hand — run the test to regenerate. */}
                |
                |import { AsciinemaPlayer } from "@site/src/components/AsciinemaPlayer";
                |import PlainDetails from "@site/src/components/PlainDetails";
                |{{IMPORTS}}
                |
                |Every cell below is executed. Each is an end-to-end `bleep test` run against a
                |generated project — resolve, compile, link, discover, run — asserted on the JUnit XML
                |that came back. A framework cannot appear here without a test running it, because the
                |matrix suites are generated from the same data this page is.
                |
                |""".stripMargin)

    sb.append("## Every framework, and where it runs\n\n")
    sb.append("| Framework | Version | Also tested |")
    targets.foreach(t => sb.append(s" ${t.title} |"))
    sb.append("\n|---|---|---|")
    targets.foreach(_ => sb.append(":-:|"))
    sb.append("\n")
    TestFrameworkFixture.all.foreach { f =>
      val others = f.versions.filterNot(_ == f.currentVersion)
      sb.append(s"| `${f.name}` | `${f.currentVersion}` | ${if (others.isEmpty) "—" else others.map(v => s"`$v`").mkString(", ")} |")
      targets.foreach(t => sb.append(s" ${yesNo(supports(f, t))} |"))
      sb.append("\n")
    }
    sb.append("\nA blank cell means the framework publishes nothing for that target, not that bleep\n")
    sb.append("fails there. **Version** is what every build checks and what the recordings below show.\n")
    sb.append("**Also tested** are covered by the same assertions when the version sweep is run\n")
    sb.append("(`bleep test bleep-tests --only-tag matrix`), so a break in one of those could sit\n")
    sb.append("unnoticed for longer. Anything not listed is untested rather than known-broken;\n")
    sb.append("nothing here restricts what you may depend on.\n")

    sb.append("\n## What each run asserts\n\n")
    sb.append("Per framework, per target, from the JUnit XML:\n\n")
    sb.append("- every passing test is reported, under its own name\n")
    sb.append("- the failing test is reported as a failure, and the throwing test as not passing\n")
    sb.append("- the counts on the `<testsuite>` element agree with the cases beneath it\n")
    sb.append("- a second suite in the same project is **not** run when one suite was selected\n")
    sb.append("- the test's own stdout reaches `<system-out>`\n")
    sb.append("- an uncaught exception's message reaches the report\n")
    sb.append("- skipped tests are reported as skipped, where the framework can express one\n")
    sb.append("- a third suite, whose tests all pass, is selected alongside the other two and comes\n")
    sb.append("  back **passing** — a run in which every suite is red cannot check that\n\n")
    sb.append("Two phrases in the tables below are worth pinning down, because they are not the same\n")
    sb.append("thing:\n\n")
    sb.append("- **nothing attached.** The framework decided the test failed without ever throwing,\n")
    sb.append("  so no exception exists. Nothing was lost in transit; there was never anything to\n")
    sb.append("  carry.\n")
    sb.append("- **an empty stand-in.** The framework built a throwaway exception when it reported\n")
    sb.append("  the result, purely to fill the field. It has no message and its stack runs through\n")
    sb.append("  the framework's own reporter, so it looks like an answer and is not.\n\n")
    sb.append("In both cases the test is still correctly reported as failed and the reason is still\n")
    sb.append("captured — just not where you would look first, which is what the *where to read the\n")
    sb.append("reason* row is for.\n\n")
    sb.append("A suite that cannot be **constructed** is a failure mode of its own: its constructor\n")
    sb.append("or initializer throws, so there is no test to attach the failure to. What you are told\n")
    sb.append("varies more between frameworks than anything else on this page. Every framework is run\n")
    sb.append("with exactly that — a suite that throws while being built, alongside a working one —\n")
    sb.append("and in every case the working suite still ran and reported normally.\n\n")

    sb.append("\n## Every framework in detail\n\n")
    sb.append("One entry per framework. Open it for what bleep knows about that framework, then open\n")
    sb.append("a target inside it for a recording of a real run on that target — the actual terminal\n")
    sb.append("output, colours included.\n\n")
    sb.append("Recordings are of the version checked on every build. The other versions listed are\n")
    sb.append("covered by the same assertions when the sweep is run; they are not recorded, because a\n")
    sb.append("second recording of the same output teaches nothing.\n\n")
    TestFrameworkFixture.all.foreach { f =>
      val on = targets.filter(t => supports(f, t))
      val others = f.versions.filterNot(_ == f.currentVersion)
      sb.append(
        s"<PlainDetails summary={<><code>${f.name}</code> <code>${f.currentVersion}</code> — ${on.map(_.title).mkString(", ")}</>}>\n\n"
      )
      sb.append(s"${f.language}, via ${f.deps(f.currentVersion).map(d => s"`$d`").mkString(" and ")}.\n\n")
      if (others.isEmpty)
        sb.append(s"Only `${f.currentVersion}` is tested.\n\n")
      else
        sb.append(
          s"Recorded and checked on every build at `${f.currentVersion}`. Also tested against " +
            s"${others.map(v => s"`$v`").mkString(" and ")} when the sweep is run.\n\n"
        )
      sb.append("| | |\n|---|---|\n")
      sb.append(s"| Can express a skipped test | ${if (f.skippedTestName.isDefined) "yes" else "no — the framework has no such concept"} |\n")
      sb.append(s"| Failure carries a stack trace | ${if (f.reportsStackFrames) "yes" else "no"} |\n")
      sb.append("\n")
      // A framework that swallows construction failures entirely leaves a green build, which is worth saying next to the framework it is true of rather than
      // in a footnote somebody has to go and find.
      if (f.hasCtorErrorVariant && f.ctorFailureReport("jvm") == CtorFailureReport.NothingButSilence) {
        sb.append(s"**A warning about `${f.name}`.** A suite that throws while being constructed is reported as a suite that\n")
        sb.append("simply has no tests in it. Those two are indistinguishable from bleep's side, and a\n")
        sb.append("class with no tests in it is a perfectly ordinary thing to have — so bleep cannot fail\n")
        sb.append("the build on it without failing every unwritten test class too. The consequence is\n")
        sb.append("worth knowing: a test class that blows up on construction leaves a green build. That\n")
        sb.append("is a bug in the framework, not a policy choice by bleep.\n\n")
      }
      on.foreach { t =>
        def describe(k: ThrowableKind) = k match {
          case ThrowableKind.Real       => "the real exception"
          case ThrowableKind.Fabricated => "an empty stand-in"
          case ThrowableKind.Absent     => "nothing attached"
        }
        val ctor =
          if (!f.hasCtorErrorVariant) "not applicable — the discovered class is a suite declaration, not a test class"
          else
            f.ctorFailureReport(t.platformId) match {
              case CtorFailureReport.NamesTheCause       => "reported with the thrown exception"
              case CtorFailureReport.FailureWithoutCause => "reported, but without what was thrown"
              case CtorFailureReport.NothingButSilence   => "**not reported at all** — the framework swallows it"
              case CtorFailureReport.Hangs               => "**hangs** until the idle timeout, then reports a timeout"
            }

        // One column per distinct answer, not per Scala version: ScalaTest on Scala.js reports differently under Scala 3 than under 2.13 and 2.12, and
        // collapsing that to whichever version happened to sort first is how the page was wrong about `kotlin.test` before it was generated. Order follows
        // the versions themselves, so the newest is the first column.
        val supported = t.scalaBinaryVersions.filter(sbv => f.supports(t.platformId, sbv, f.currentVersion))
        val columns = supported.foldLeft(List.empty[(FailureReporting, List[Option[String]])]) { (acc, sbv) =>
          val r = f.failureReporting(t.platformId, sbv)
          acc.indexWhere(_._1 == r) match {
            case -1 => acc :+ (r -> List(sbv))
            case i  => acc.updated(i, r -> (acc(i)._2 :+ sbv))
          }
        }
        val heading = columns.map { case (_, sbvs) => s"Scala ${sbvs.flatten.mkString(", ")}" }

        sb.append(s"""<PlainDetails summary="on ${t.title}">\n\n""")
        if (columns.sizeIs > 1) sb.append(heading.mkString("| | ", " | ", " |\n|---|") + columns.map(_ => "---|").mkString + "\n")
        else sb.append("| | |\n|---|---|\n")
        def row(label: String, cells: List[String]): Unit = sb.append(cells.mkString(s"| $label | ", " | ", " |\n"))
        row("A failed assertion reports", columns.map { case (r, _) => describe(r.assertionFailure) })
        row("An uncaught exception reports", columns.map { case (r, _) => describe(r.uncaughtException) })
        row("A suite that fails to construct", columns.map(_ => ctor))
        if (columns.exists { case (r, _) => r.explanation != ExplanationAt.OnCase })
          row(
            "Where to read the reason",
            columns.map { case (r, _) =>
              r.explanation match {
                case ExplanationAt.CapturedStdout => "the suite's captured output, not the failing test"
                case ExplanationAt.CapturedStderr => "the suite's captured **error** output, not the failing test"
                case ExplanationAt.Nowhere        => "nowhere bleep can reach"
                case ExplanationAt.OnCase         => "the failing test"
              }
            }
          )
        // And why it is that way, which is the part that cannot be a table cell. Only for a column that falls short of handing over the real exception: where
        // a framework reports in full there is nothing to explain, and the explanation for the column beside it already covers both.
        columns
          .collect { case (r, sbvs) if r != FailureReporting.Full => f.fidelityCause(t.platformId, sbvs.head) }
          .filter(_.nonEmpty)
          .distinct
          .foreach(cause => sb.append(s"\n$cause\n"))
        if (hasCast(f, t)) {
          val (cols, rows) = castSize(f, t)
          sb.append(s"\n<AsciinemaPlayer src={${castVar(f, t)}} cols={$cols} rows={$rows} fit=\"width\" idleTimeLimit={1} />\n")
        }
        sb.append("\n</PlainDetails>\n\n")
      }
      sb.append("</PlainDetails>\n\n")
    }

    val imports = (for {
      f <- TestFrameworkFixture.all
      t <- targets
      if supports(f, t) && hasCast(f, t)
    } yield s"""import ${castVar(f, t)} from "!!file-loader!@site/static/demos/testfw-${castSlug(f, t)}.cast";""").mkString("\n")
    sb.toString.replace("{{IMPORTS}}", imports)
  }

  test("the checked-in support matrix matches the fixtures it is generated from") {
    val expected = render()
    if (regenerate) {
      Files.createDirectories(docPath.getParent)
      Files.write(docPath, expected.getBytes(StandardCharsets.UTF_8))
      info(s"regenerated $docPath")
    } else {
      withClue(
        s"$docPath is out of date. Regenerate it with:\n" +
          "  BLEEP_REGENERATE_DOCS=1 bleep test bleep-tests --only bleep.TestFrameworkMatrixDocTest\n"
      ) {
        Files.exists(docPath) shouldBe true
        new String(Files.readAllBytes(docPath), StandardCharsets.UTF_8) shouldBe expected
      }
    }
  }
}
