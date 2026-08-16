package bleep.scripts.dev

import bleep.{model, BleepScript, Commands, Started}
import com.monovore.decline.CliDocsWalker
import com.monovore.decline.CliDocsWalker.{CommandDoc, FlagDoc}

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** Walk bleep's decline command tree and emit MDX into `docs/reference/cli/`.
  *
  * Layout:
  *   - Top-level commands without subcommands → `cli/<name>.mdx`
  *   - Top-level commands with subcommands → `cli/<name>/index.mdx` plus one `cli/<name>/<sub>.mdx` per direct subcommand. Sub-subcommands stay inline as
  *     sections within the per-subcommand page.
  *
  * Run with `bleep gen-cli-docs`. Idempotent — re-run any time `Main`'s opts tree changes; CI verifies via `git diff --exit-code -- docs/reference/cli/`.
  */
object GenCliDocs extends BleepScript("GenCliDocs") {

  override def run(started: Started, commands: Commands, args: List[String]): Unit = {
    val outDir = started.buildPaths.buildDir.resolve("docs").resolve("reference").resolve("cli")

    // Wipe the output dir (except _index.txt + nothing else) so deletions are
    // captured. Idempotent: if a top-level command goes away in Main.scala, its
    // page disappears here.
    if (Files.isDirectory(outDir)) deleteTreeContents(outDir)
    Files.createDirectories(outDir)

    val builder = List.newBuilder[CommandDoc]
    builder ++= CliDocsWalker.topLevelSubcommands(bleep.Main.hasBuildOpts(started))

    // Reach the no-build commands too — these include `import-maven`, `config`,
    // `install-tab-completions-*`, etc. Construct a non-existing build loader
    // pointed at a path we know has no bleep.yaml.
    val tmpCwd = Files.createTempDirectory("bleep-clidocs")
    val noBuildLoader = bleep.BuildLoader.nonExisting(tmpCwd)
    val noBuildPaths = bleep.BuildPaths(tmpCwd, noBuildLoader, model.BuildVariant.Normal)
    builder ++= CliDocsWalker.topLevelSubcommands(
      bleep.Main.noBuildOpts(started.logger, started.userPaths, noBuildPaths, noBuildLoader)
    )

    // Merge by name — when a top-level appears in both, prefer the
    // hasBuildOpts version (which is what the user sees in a built repo).
    val seen = scala.collection.mutable.LinkedHashMap.empty[String, CommandDoc]
    builder.result().foreach { c =>
      if (!seen.contains(c.name)) seen(c.name) = c
    }

    // Hide:
    // - underscore-prefixed internal commands (_complete, _complete-zsh)
    // - user-defined scripts from the current build's `scripts:` section —
    //   those are project-specific subcommands, not general CLI surface.
    val userScriptNames = started.build.scripts.keys.map(_.value).toSet
    val docs = seen.values
      .filter(c => !c.name.startsWith("_"))
      .filter(c => !userScriptNames.contains(c.name))
      .toList
      .sortBy(_.name)

    var pageCount = 0

    docs.foreach { cmd =>
      if (cmd.subcommands.isEmpty) {
        // Single-file leaf command
        val mdx = renderPage(cmd, parentPath = s"bleep ${cmd.name}", topLevel = true)
        val outFile = outDir.resolve(s"${cmd.name}.mdx")
        Files.writeString(outFile, withHandWritten(s"${cmd.name}.mdx", mdx))
        started.logger.info(s"wrote ${cmd.name}.mdx")
        pageCount += 1
      } else {
        // Directory: index + per-subcommand pages
        val subdir = outDir.resolve(cmd.name)
        Files.createDirectories(subdir)

        val indexMdx = renderIndex(cmd)
        Files.writeString(subdir.resolve("index.mdx"), withHandWritten(s"${cmd.name}/index.mdx", indexMdx))
        started.logger.info(s"wrote ${cmd.name}/index.mdx")
        pageCount += 1

        cmd.subcommands.foreach { sub =>
          val parentPath = s"bleep ${cmd.name} ${sub.name}"
          val mdx = renderPage(sub, parentPath, topLevel = false)
          Files.writeString(subdir.resolve(s"${sub.name}.mdx"), withHandWritten(s"${cmd.name}/${sub.name}.mdx", mdx))
          started.logger.info(s"wrote ${cmd.name}/${sub.name}.mdx")
          pageCount += 1
        }
      }
    }

    // Hand-written pages for surface that is not in the decline tree (build-local
    // scripts). They live here so regeneration recreates them instead of deleting them.
    extraPages.foreach { page =>
      val outFile = outDir.resolve(s"${page.name}.mdx")
      if (Files.exists(outFile))
        sys.error(s"extra page ${page.name}.mdx collides with a generated page — it is a real command now, drop the hand-written one")
      Files.writeString(outFile, page.mdx)
      started.logger.info(s"wrote ${page.name}.mdx (hand-written)")
      pageCount += 1
    }

    val generatedIndexEntries: List[(String, List[String])] =
      docs.map { c =>
        val lines =
          if (c.subcommands.isEmpty) List(s"- bleep ${c.name}")
          else s"- bleep ${c.name}" :: c.subcommands.map(s => s"  - bleep ${c.name} ${s.name}")
        (c.name, lines)
      }
    val extraIndexEntries: List[(String, List[String])] =
      extraPages.map(page => (page.name, List(s"- bleep ${page.name}")))

    val index = (generatedIndexEntries ++ extraIndexEntries).sortBy { case (name, _) => name }.flatMap { case (_, lines) => lines }.mkString("\n") + "\n"
    Files.writeString(outDir.resolve("_index.txt"), index)
    started.logger.info(s"wrote $pageCount pages to $outDir")
  }

  // ----------------------------------------------------------------
  // Hand-written extras
  // ----------------------------------------------------------------

  case class HandWritten(imports: String, body: String)

  /** Hand-written MDX injected into otherwise generated pages, keyed by output path relative to `cli/`. MDX requires imports at module scope, so they go right
    * after the frontmatter; the body is appended to the end of the page. By living in the generator, this content survives regeneration by construction.
    */
  private val seeRunHistoryGuide: HandWritten =
    HandWritten(
      imports = "",
      body = """## See also
               |
               |The [run history & diffs guide](/docs/usage/run-history) tells the whole story — what
               |gets recorded, diffing runs, the `--diff` edit loop, cross-worktree comparisons — with
               |a demo video.""".stripMargin
    )

  /** The importers are where a reader asks "can I get in — and back out?". Both answers, including the one that is "no", belong on those pages. */
  private val importExportMatrix: HandWritten =
    HandWritten(
      imports = "",
      body = """## Getting in and out
               |
               |Mechanical paths in: [`bleep import`](/docs/reference/cli/import/) (sbt) and
               |[`bleep import-maven`](/docs/reference/cli/import-maven/) (Maven). Mechanical path out:
               |[`bleep export-maven`](/docs/reference/cli/export-maven/), a proof-of-concept script — see the
               |[exit strategy](/docs/guides/exit-strategy).
               |
               |**Gradle has neither.** There is no Gradle importer and no Gradle exporter, so a Gradle build is
               |hand-ported in, and would be hand-ported out. There is no sbt exporter either.""".stripMargin
    )

  private val handWritten: Map[String, HandWritten] =
    Map(
      "import.mdx" -> importExportMatrix,
      "import-maven.mdx" -> importExportMatrix,
      "history/index.mdx" -> seeRunHistoryGuide,
      "history/show.mdx" -> seeRunHistoryGuide,
      "history/diff.mdx" -> seeRunHistoryGuide,
      "compile.mdx" -> seeRunHistoryGuide,
      "test.mdx" -> seeRunHistoryGuide
    )

  /** A whole page that is not in the decline tree. The generator wipes `docs/reference/cli/` on every run, so any page that must exist there has to be produced
    * here or it disappears on the next regeneration.
    *
    * The one case today is `export-maven`: it is a build-local script in bleep's own `bleep.yaml` (and therefore filtered out of the generated pages along with
    * every other user script), but readers looking for the documented way out of bleep expect to find it in the reference. The page says out loud that it is a
    * script, not a built-in command.
    */
  case class ExtraPage(name: String, mdx: String)

  private val exportMavenPage: ExtraPage = ExtraPage(
    name = "export-maven",
    mdx = """---
title: bleep export-maven
---

{/* HAND-WRITTEN, but still generated: this page lives in `extraPages` in
    scripts-dev/src/scala/bleep/scripts/dev/GenCliDocs.scala, because
    `bleep gen-cli-docs` wipes this directory. Edit it there, not here. */}

# `bleep export-maven`

<p>Walk the exploded build model and write a buildable Maven layout: an aggregator POM plus one POM per project.</p>

:::note Not a built-in command
`export-maven` is a [bleep script](/docs/concepts/bleep-scripts), not part of the bleep CLI. It ships in bleep's own repository as
`scripts/src/scala/bleep/scripts/ExportMaven.scala` and is registered under `scripts:` in bleep's `bleep.yaml`, so `bleep export-maven`
works in a checkout of bleep. To get it in your own build, copy that one file into a scripts project — it only uses the published
`bleep-core` / `bleep-model` API. It is a proof of concept: see [what it does not translate](#not-translated).
:::

## Synopsis

```bash
bleep export-maven <output-directory> [--skip-tests <test-project-name>]...
```

## Arguments

| Argument | Type |
|----------|------|
| `output-directory` | one, required — there is no default. Created if missing. POMs reference your sources by paths relative to each module, so put it inside the workspace |

## Flags

| Flag | Description |
|------|-------------|
| `--skip-tests <test-project-name>` (repeatable) | mark that test project's suite *execution* skipped in its generated POM, with a comment saying why; the suites still compile. Must name an exported test project, or the export fails |

## What it writes

- an aggregator `pom.xml` in the output directory, `packaging` `pom`, listing every exported project as a `<module>`
- per project a `<artifactId>/pom.xml` with coordinates, dependencies, source and resource directories (via `build-helper-maven-plugin`, since bleep's layout is not Maven's default) and compiler setup: `scala-maven-plugin` for Scala, `kotlin-maven-plugin` for Kotlin, `maven-compiler-plugin` for javac options
- for test projects: sources wired as Maven *test* sources, with `scalatest-maven-plugin` running the suites during `mvn test` / `mvn install`. Surefire is skipped (these are ScalaTest suites, not JUnit). Bleep's forked-test JVM options become `argLine`, bleep's fork working directory becomes `workingDirectory`
- for projects with `sourcegen:`, an `exec-maven-plugin` execution bound to `generate-sources` that runs the same `main` class bleep forks, on a classpath bleep's own resolver computed

Coordinates: `groupId` from the project's `publish.groupId`, else `build.bleep.exported`; `artifactId` from the cross project name; every module at a fixed version `0.1.0-SNAPSHOT`.

## Not translated

Silently: unmanaged `jars`, Scala `compilerPlugins`, and publish/assembly configuration. Loudly (the export fails): Kotlin compiler plugins, KSP symbol processing, test projects on a framework other than ScalaTest, and dependencies with a classifier or a configuration that has no Maven scope. Scala.js and Scala Native projects are skipped, and the skip cascades to their dependents. Suite discovery is narrowed to the `*Test` suffix, so `*IT` integration suites compile but do not run.

There is **no exporter for sbt or Gradle**, and no Gradle importer either — [`bleep import`](/docs/reference/cli/import/) covers sbt and [`bleep import-maven`](/docs/reference/cli/import-maven/) covers Maven.

## See also

The [exit strategy guide](/docs/guides/exit-strategy) runs this exporter against bleep's own build and shows the verification output — 25 modules, `mvn install`, tests executing.
"""
  )

  private val extraPages: List[ExtraPage] = List(exportMavenPage)

  private def withHandWritten(relPath: String, mdx: String): String =
    handWritten.get(relPath) match {
      case None        => mdx
      case Some(extra) =>
        val withImports =
          if (extra.imports.isEmpty) mdx
          else {
            val frontMatterEnd = "\n---\n\n"
            val idx = mdx.indexOf(frontMatterEnd)
            if (idx < 0) sys.error(s"$relPath: no frontmatter found to insert hand-written imports after")
            val insertAt = idx + frontMatterEnd.length
            mdx.substring(0, insertAt) + extra.imports + "\n\n" + mdx.substring(insertAt)
          }
        withImports + extra.body + "\n"
    }

  // ----------------------------------------------------------------
  // Rendering
  // ----------------------------------------------------------------

  /** Render a command's standalone page. `parentPath` is e.g. `"bleep build show"` for `bleep build show`; the page renders the synopsis, args, flags, and any
    * sub-subcommands as inline sections.
    */
  private def renderPage(cmd: CommandDoc, parentPath: String, topLevel: Boolean): String = {
    val sb = new StringBuilder
    val title = if (topLevel) s"bleep ${cmd.name}" else parentPath
    sb.append("---\n")
    sb.append(s"title: $title\n")
    sb.append("---\n\n")
    sb.append(autoGenBanner)
    sb.append(s"# `$title`\n\n")
    sb.append(renderDescription(cmd.description)).append("\n\n"): Unit
    renderBody(cmd, sb, parentPath, headingLevel = 2)
    sb.toString
  }

  /** Render the index page for a top-level command that has subcommands. Lists each subcommand with its description, linking to the per-subcommand page, plus
    * the parent's own flags (often shared / global).
    */
  private def renderIndex(cmd: CommandDoc): String = {
    val sb = new StringBuilder
    val parentPath = s"bleep ${cmd.name}"
    sb.append("---\n")
    sb.append(s"title: bleep ${cmd.name}\n")
    sb.append("---\n\n")
    sb.append(autoGenBanner)
    sb.append(s"# `$parentPath`\n\n")
    sb.append(renderDescription(cmd.description)).append("\n\n"): Unit

    sb.append("## Synopsis\n\n")
    sb.append("```bash\n")
    sb.append(s"$parentPath <subcommand> [args] [flags]\n")
    sb.append("```\n\n")

    // Parent-level flags (rare for command groups, but emit if any)
    val parentFlags = cmd.flags.filter(_.visibility == "Normal").filterNot(_.longName == "help")
    if (parentFlags.nonEmpty) {
      sb.append("## Flags\n\n")
      sb.append("| Flag | Description |\n")
      sb.append("|------|-------------|\n")
      parentFlags.foreach(f => sb.append(s"| ${formatFlagInvocation(f)} | ${escapeMdInline(f.description.trim)} |\n"))
      sb.append("\n")
    }

    sb.append("## Subcommands\n\n")
    cmd.subcommands.foreach { sub =>
      sb.append(s"- [`$parentPath ${sub.name}`](./${sub.name}): ${escapeMdInline(sub.description.trim)}\n")
    }
    sb.append("\n")

    sb.toString
  }

  // ----------------------------------------------------------------
  // Body rendering — synopsis + args/flags/sub-subcommands
  // ----------------------------------------------------------------

  private def renderBody(cmd: CommandDoc, sb: StringBuilder, parentPath: String, headingLevel: Int): Unit = {
    val h = "#" * headingLevel
    val argsPart = cmd.arguments.map(a => s"<${stripBrackets(a.metavar)}${if (a.repeated) "..." else ""}>").mkString(" ")
    val visibleFlags = cmd.flags.filter(_.visibility == "Normal").filterNot(_.longName == "help")
    val flagsPart = if (visibleFlags.nonEmpty) " [flags]" else ""
    val subPart = if (cmd.subcommands.nonEmpty) " <subcommand>" else ""

    if (argsPart.nonEmpty || visibleFlags.nonEmpty || cmd.subcommands.nonEmpty) {
      sb.append(s"$h Synopsis\n\n")
      sb.append("```bash\n")
      sb.append(parentPath)
      if (subPart.nonEmpty) sb.append(subPart)
      if (argsPart.nonEmpty) sb.append(" ").append(argsPart): Unit
      if (flagsPart.nonEmpty) sb.append(flagsPart)
      sb.append("\n```\n\n")
    }

    if (cmd.arguments.nonEmpty) {
      sb.append(s"$h Arguments\n\n")
      sb.append("| Argument | Type |\n")
      sb.append("|----------|------|\n")
      cmd.arguments.foreach { a =>
        sb.append(s"| `${stripBrackets(a.metavar)}` | ${if (a.repeated) "one or more" else "one"} |\n")
      }
      sb.append("\n")
    }

    if (visibleFlags.nonEmpty) {
      sb.append(s"$h Flags\n\n")
      sb.append("| Flag | Description |\n")
      sb.append("|------|-------------|\n")
      visibleFlags.foreach(f => sb.append(s"| ${formatFlagInvocation(f)} | ${escapeMdInline(f.description.trim)} |\n"))
      sb.append("\n")
    }

    if (cmd.subcommands.nonEmpty) {
      cmd.subcommands.foreach { sub =>
        val subPath = s"$parentPath ${sub.name}"
        sb.append(s"$h `$subPath`\n\n")
        if (sub.description.trim.nonEmpty) sb.append(renderDescription(sub.description)).append("\n\n"): Unit
        renderBody(sub, sb, subPath, headingLevel + 1)
      }
    }
  }

  // ----------------------------------------------------------------
  // Helpers
  // ----------------------------------------------------------------

  private val autoGenBanner: String =
    "{/* AUTO-GENERATED by `bleep gen-cli-docs`. Do not edit by hand.\n" +
      "    Edit the decline `Opts.subcommand(name, description)` calls in\n" +
      "    bleep-cli/src/scala/bleep/Main.scala instead. */}\n\n"

  /** Wrap description prose in `<p>` so MDX doesn't parse a leading `import` / `export` as an ES module statement.
    */
  private def renderDescription(desc: String): String =
    s"<p>${escapeForJsx(desc.trim)}</p>"

  private def escapeForJsx(s: String): String =
    s.replace("&", "&amp;")
      .replace("<", "&lt;")
      .replace(">", "&gt;")
      .replace("{", "&#123;")
      .replace("}", "&#125;")

  /** Light escape for inline markdown (used in subcommand list bullets). */
  private def escapeMdInline(s: String): String =
    s.replace("|", "\\|").replace("<", "&lt;").replace(">", "&gt;")

  private def stripBrackets(s: String): String =
    s.stripPrefix("<").stripSuffix(">").trim

  private def formatFlagInvocation(f: FlagDoc): String = {
    val long = s"--${f.longName}"
    val short = f.shortName.map(s => s", -$s").getOrElse("")
    val mv = f.metavar.map(m => s" <${stripBrackets(m)}>").getOrElse("")
    val rep = if (f.repeated) " (repeatable)" else ""
    s"`$long$short$mv`$rep"
  }

  /** Recursively delete every entry under `dir`, but keep `dir` itself. */
  private def deleteTreeContents(dir: Path): Unit = {
    val stream = Files.newDirectoryStream(dir)
    try
      stream.asScala.foreach { p =>
        if (Files.isDirectory(p)) {
          deleteTreeContents(p)
          Files.delete(p)
        } else {
          Files.delete(p)
        }
      }
    finally stream.close()
  }
}
