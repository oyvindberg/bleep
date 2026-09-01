package bleep

import bleep.commands.LinkOptions
import org.scalatest.Assertion

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** One link, described end to end: what to build, how to link it, what the artifact should look like, and what it prints when run. */
case class LinkCase(
    name: String,
    projectYaml: String,
    options: LinkOptions,
    /** The artifact's file name has to satisfy this. For JS the extension is the toolchain's own statement of module kind. */
    expectedFileName: String => Boolean,
    fileNameDescription: String,
    mustContain: List[String],
    mustNotContain: List[String],
    /** Files that must appear beside the artifact — a source map, a `.d.ts`. Matched on the file name. */
    siblings: List[String],
    /** Assertions on a sibling's content, keyed by the same file-name match. */
    siblingMustContain: List[(String, String)],
    /** Some outputs cannot be run by `node <file>` alone — AMD needs a loader. Those are asserted on shape only, and say so. */
    runnable: Boolean
)

/** `bleep link` run end to end for every target and mode bleep supports, asserting on the artifact and then on what it prints.
  *
  * The counterpart to [[TestFrameworkMatrixIT]], which does the same for `bleep test`. Linking had per-piece tests — a release link minifies, a test link
  * honours `jsKind` — and nothing that took a declaration, linked it, and ran the result. That gap is where three defects lived at once: a `--release` Scala.js
  * link never ran Closure, a Kotlin/JS `es` link reported "linking failed" because the output lookup only knew `.js`, and a test link and a main link wrote to
  * two different directories.
  *
  * Every case asserts the path bleep *reported* (`BuildSummary.linkedOutputs`), not a path the test reconstructed, so a link that writes somewhere unexpected
  * fails here rather than being quietly found by a directory walk.
  */
abstract class LinkMatrixIT(platformName: String, cases: List[LinkCase], sourcePath: String, source: String) extends IntegrationTestHarness {

  private val myapp = model.CrossProjectName(model.ProjectName("myapp"), None)

  /** What every fixture prints, so "did it run" is one assertion for every target. */
  private val Marker = "linked-and-ran"

  cases.foreach { linkCase =>
    integrationTest(s"$platformName / ${linkCase.name}") { ws =>
      ws.yaml(s"projects:\n${linkCase.projectYaml}")
      ws.file(sourcePath, source)
      val (started, commands, _) = ws.start()

      val summary = commands.link(List(myapp), linkCase.options)
      val reported = summary.linkedOutputs
      assert(reported.map(_.project) == List(myapp), s"expected one linked output for myapp, got ${reported.map(_.project.value)}")
      val artifact = reported.head.mainArtifact
      assert(Files.exists(artifact), s"bleep reported an artifact that is not there: $artifact")

      val fileName = artifact.getFileName.toString
      assert(linkCase.expectedFileName(fileName), s"linked artifact is '$fileName', expected ${linkCase.fileNameDescription}")

      if (linkCase.mustContain.nonEmpty || linkCase.mustNotContain.nonEmpty) {
        val body = Files.readString(artifact)
        linkCase.mustContain.foreach(marker => assert(body.contains(marker), s"'$fileName' does not contain '$marker'"))
        linkCase.mustNotContain.foreach(marker => assert(!body.contains(marker), s"'$fileName' contains '$marker' and should not"))
      }

      val beside = Files.list(artifact.getParent).iterator().asScala.map(_.getFileName.toString).toList.sorted
      linkCase.siblings.foreach { wanted =>
        assert(beside.exists(_.endsWith(wanted)), s"expected a '$wanted' beside $fileName, found: ${beside.mkString(", ")}")
      }
      linkCase.siblingMustContain.foreach { case (wanted, marker) =>
        val sibling = beside.find(_.endsWith(wanted)).getOrElse(fail(s"no '$wanted' beside $fileName, found: ${beside.mkString(", ")}"))
        val body = Files.readString(artifact.resolveSibling(sibling))
        assert(body.contains(marker), s"'$sibling' does not contain '$marker'")
      }

      if (linkCase.runnable) {
        val out = LinkMatrixIT.run(started, artifact, ws)
        assert(out.contains(Marker), s"running '$fileName' did not print '$Marker'. Output:\n$out")
      } else {
        info(s"$fileName asserted on shape only — this module kind needs a loader to run")
      }
      succeed
    }
  }
}

object LinkMatrixIT {

  /** Run a linked artifact and hand back everything it printed.
    *
    * A JS module goes through node; a native binary is executed directly. Scala.js emits ES-module syntax into a file still called `main.js`, and node picks a
    * module system by extension — so that one case is copied to `.mjs` first, which is what a user deploying it has to do too.
    */
  def run(started: Started, artifact: Path, ws: Workspace): String = {
    val name = artifact.getFileName.toString
    val cmd =
      if (name.endsWith(".js") || name.endsWith(".mjs")) {
        val node = started.pre.fetchNode(model.Versions.Node).toAbsolutePath.toString
        val body = Files.readString(artifact)
        val isEsm = name.endsWith(".mjs") || body.linesIterator.exists(l => l.trim.startsWith("import ") || l.trim.startsWith("export "))
        if (isEsm && name.endsWith(".js")) {
          val asModule = artifact.resolveSibling(name.stripSuffix(".js") + ".mjs")
          Files.copy(artifact, asModule, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
          List(node, asModule.toAbsolutePath.toString)
        } else List(node, artifact.toAbsolutePath.toString)
      } else List(artifact.toAbsolutePath.toString)

    val logger = ryddig.Loggers.stdout(ryddig.LogPatterns.logFile, disableProgress = true).acquire().value
    cli(action = "run linked", cwd = ws.root, cmd = cmd, logger = logger, out = cli.Out.ViaLogger(logger)).stdout.mkString("\n")
  }
}
