package bleep.scripts.dev

import bleep.{BleepScript, Commands, Started}

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.matching.Regex

/** Rewrite inline `bleep <command>` mentions in `docs/**/*.{md,mdx}` (outside `docs/reference/cli/`) into Markdown links pointing at the auto-generated
  * reference pages.
  *
  * Idempotent: a mention that's already inside a Markdown link `[...](...)` is left alone, so re-running won't double-link.
  *
  * Run with `bleep linkify-cli-mentions`. Run after `bleep gen-cli-docs`.
  */
object LinkifyCliMentions extends BleepScript("LinkifyCliMentions") {

  override def run(started: Started, commands: Commands, args: List[String]): Unit = {
    val buildDir = started.buildPaths.buildDir
    val cliDir = buildDir.resolve("docs").resolve("reference").resolve("cli")
    val docsDir = buildDir.resolve("docs")

    if (!Files.isDirectory(cliDir)) {
      sys.error(s"Expected $cliDir — run `bleep gen-cli-docs` first.")
    }

    val cmdMap = buildCommandMap(cliDir)
    started.logger.info(s"loaded ${cmdMap.size} commands from $cliDir")

    var filesChanged = 0
    var linksAdded = 0
    walkDocsFiles(docsDir, cliDir).foreach { file =>
      val original = Files.readString(file)
      val (rewritten, count) = rewrite(original, cmdMap)
      if (count > 0) {
        Files.writeString(file, rewritten)
        filesChanged += 1
        linksAdded += count
        started.logger.info(s"$file: $count link(s)")
      }
    }
    started.logger.info(s"linked $linksAdded mention(s) across $filesChanged file(s)")
  }

  // --------------------------------------------------------------
  // Command map: derived from the cli/ directory layout
  // --------------------------------------------------------------

  /** Walks `cliDir` and returns a map from command path (e.g. `"build show"`) to its docs URL (e.g. `"/docs/reference/cli/build/show/"`).
    *
    * Three-level commands (sub-subcommands rendered as inline sections) are also included, with anchor-suffixed URLs.
    */
  private def buildCommandMap(cliDir: Path): Map[String, String] = {
    val builder = Map.newBuilder[String, String]

    val stream = Files.newDirectoryStream(cliDir)
    try
      stream.asScala.foreach { entry =>
        val name = entry.getFileName.toString
        if (Files.isDirectory(entry)) {
          // Top-level command with subcommands. Index page → /docs/reference/cli/<name>/
          builder += (name -> s"/docs/reference/cli/$name/")
          val subStream = Files.newDirectoryStream(entry)
          try
            subStream.asScala.foreach { subEntry =>
              val subFile = subEntry.getFileName.toString
              if (subFile.endsWith(".mdx") && subFile != "index.mdx") {
                val sub = subFile.stripSuffix(".mdx")
                val subUrl = s"/docs/reference/cli/$name/$sub/"
                builder += (s"$name $sub" -> subUrl)
                // Inline sub-subcommands: parse the file for "## `bleep <name> <sub> <subsub>`" headings
                val mdx = Files.readString(subEntry)
                val sectionRegex = ("""(?m)^##\s*`bleep """ + Regex.quote(s"$name $sub") + """ ([a-z][a-z0-9-]*)`""").r
                sectionRegex.findAllMatchIn(mdx).foreach { m =>
                  val subsub = m.group(1)
                  val anchor = s"bleep-$name-$sub-$subsub"
                  builder += (s"$name $sub $subsub" -> s"$subUrl#$anchor")
                }
              }
            }
          finally subStream.close()
        } else if (name.endsWith(".mdx") && name != "_index.txt") {
          val cmd = name.stripSuffix(".mdx")
          builder += (cmd -> s"/docs/reference/cli/$cmd/")
        }
      }
    finally stream.close()

    builder.result()
  }

  // --------------------------------------------------------------
  // Walking docs/ — collect everything except the cli/ tree itself
  // --------------------------------------------------------------

  private def walkDocsFiles(docsDir: Path, excludeDir: Path): List[Path] = {
    val result = List.newBuilder[Path]
    Files.walk(docsDir).iterator().asScala.foreach { p =>
      if (Files.isRegularFile(p) && !p.startsWith(excludeDir)) {
        val name = p.getFileName.toString
        if (name.endsWith(".md") || name.endsWith(".mdx")) result += p
      }
    }
    result.result()
  }

  // --------------------------------------------------------------
  // Rewriting — find inline `bleep ...` mentions, link them
  // --------------------------------------------------------------

  /** Inline-code `bleep <command>` mention, longest match preferred. The command portion is greedy so we'll catch `bleep build show` rather than just
    * `bleep build`.
    */
  private val mentionRegex: Regex =
    """`bleep ([a-z][a-z0-9-]*(?: [a-z][a-z0-9-]*){0,2})`""".r

  /** Rewrite a single file body. Returns (newBody, linkCount).
    *
    * Skips matches that are:
    *   - already inside a Markdown link (next char is `(`, or prev char is `[`)
    *   - inside a heading line (starts with `#`) — heading-level autolinks render with link styling and clash with unlinked sibling headings
    *   - inside a fenced code block (between ``` fences) — code-block content is verbatim; injecting `[...](...)` syntax breaks YAML/bash/etc. that happens to
    *     mention `bleep <command>` in a comment or string
    */
  def rewrite(body: String, cmdMap: Map[String, String]): (String, Int) = {
    val codeFenceRanges = findFencedCodeBlocks(body)
    var count = 0
    val rewritten = mentionRegex.replaceAllIn(
      body,
      m => {
        val matched = m.matched // includes the backticks
        val cmd = m.group(1) // e.g. "build show"
        val end = m.end
        val nextChar = if (end < body.length) body.charAt(end) else ' '
        val alreadyLinked = nextChar == '('
        val prevChar = if (m.start > 0) body.charAt(m.start - 1) else ' '
        val insideLink = prevChar == '['
        // Skip if this match sits on a heading line. Find the start of the line
        // and check whether the first non-whitespace char is `#`.
        val lineStart = body.lastIndexOf('\n', m.start - 1) + 1 // 0 if no \n
        val firstNonWs = {
          var i = lineStart
          while (i < m.start && (body.charAt(i) == ' ' || body.charAt(i) == '\t')) i += 1
          if (i < body.length) body.charAt(i) else ' '
        }
        val onHeadingLine = firstNonWs == '#'
        val inCodeFence = codeFenceRanges.exists { case (s, e) => m.start >= s && m.end <= e }

        if (alreadyLinked || insideLink || onHeadingLine || inCodeFence) Regex.quoteReplacement(matched)
        else
          cmdMap.get(cmd) match {
            case Some(url) =>
              count += 1
              Regex.quoteReplacement(s"[$matched]($url)")
            case None => Regex.quoteReplacement(matched)
          }
      }
    )
    (rewritten, count)
  }

  /** Find every (start, end) range covered by a triple-backtick fenced code
    * block. Both inclusive of the fence markers themselves so the check
    * `m.start >= s && m.end <= e` works.
    */
  private def findFencedCodeBlocks(body: String): List[(Int, Int)] = {
    val fenceRegex = """(?m)^```""".r
    val matches = fenceRegex.findAllMatchIn(body).map(_.start).toList
    matches.grouped(2).collect { case List(open, close) => (open, close + 3) }.toList
  }
}
