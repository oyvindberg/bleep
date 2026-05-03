package bleep.scripts.dev

import bleep.{model, BleepScript, Commands, Started}
import com.monovore.decline.CliDocsWalker
import com.monovore.decline.CliDocsWalker.{ArgumentDoc, CommandDoc, FlagDoc}

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
        Files.writeString(outFile, mdx)
        started.logger.info(s"wrote ${cmd.name}.mdx")
        pageCount += 1
      } else {
        // Directory: index + per-subcommand pages
        val subdir = outDir.resolve(cmd.name)
        Files.createDirectories(subdir)

        val indexMdx = renderIndex(cmd)
        Files.writeString(subdir.resolve("index.mdx"), indexMdx)
        started.logger.info(s"wrote ${cmd.name}/index.mdx")
        pageCount += 1

        cmd.subcommands.foreach { sub =>
          val parentPath = s"bleep ${cmd.name} ${sub.name}"
          val mdx = renderPage(sub, parentPath, topLevel = false)
          Files.writeString(subdir.resolve(s"${sub.name}.mdx"), mdx)
          started.logger.info(s"wrote ${cmd.name}/${sub.name}.mdx")
          pageCount += 1
        }
      }
    }

    val index = docs
      .flatMap { c =>
        if (c.subcommands.isEmpty) List(s"- bleep ${c.name}")
        else s"- bleep ${c.name}" :: c.subcommands.map(s => s"  - bleep ${c.name} ${s.name}")
      }
      .mkString("\n") + "\n"
    Files.writeString(outDir.resolve("_index.txt"), index)
    started.logger.info(s"wrote $pageCount pages to $outDir")
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
    sb.append(renderDescription(cmd.description)).append("\n\n")
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
    sb.append(renderDescription(cmd.description)).append("\n\n")

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
      parentFlags.foreach(f => sb.append(s"| ${formatFlagInvocation(f)} | ${escapePipes(f.description)} |\n"))
      sb.append("\n")
    }

    sb.append("## Subcommands\n\n")
    cmd.subcommands.foreach { sub =>
      sb.append(s"- [`$parentPath ${sub.name}`](./${sub.name}) &mdash; ${escapeMdInline(sub.description.trim)}\n")
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
      if (argsPart.nonEmpty) sb.append(" ").append(argsPart)
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
      visibleFlags.foreach(f => sb.append(s"| ${formatFlagInvocation(f)} | ${escapePipes(f.description)} |\n"))
      sb.append("\n")
    }

    if (cmd.subcommands.nonEmpty) {
      cmd.subcommands.foreach { sub =>
        val subPath = s"$parentPath ${sub.name}"
        sb.append(s"$h `$subPath`\n\n")
        if (sub.description.trim.nonEmpty) sb.append(renderDescription(sub.description)).append("\n\n")
        renderBody(sub, sb, subPath, headingLevel + 1)
      }
    }
  }

  // ----------------------------------------------------------------
  // Helpers
  // ----------------------------------------------------------------

  private val autoGenBanner: String =
    "{/* AUTO-GENERATED by `bleep gen-cli-docs`. Do not edit by hand —\n" +
      "    edit the decline `Opts.subcommand(name, description)` calls in\n" +
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

  private def escapePipes(s: String): String = s.replace("|", "\\|").trim

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
