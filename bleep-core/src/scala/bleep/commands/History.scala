package bleep
package commands

import bleep.history.{Transcript, TranscriptDiff, TranscriptDiffRender, TranscriptFormat, TranscriptStore}

import java.nio.file.{Files, Path}
import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneId}

/** CLI surface over the per-workspace run history the daemon writes (`<workspace>/.bleep/builds/<variant>/history/`). Pure file reads — no daemon connection is
  * made, which is the point: history survives daemon restarts and can be inspected from a machine where no daemon runs at all.
  *
  * Output goes to stdout via println, not the logger: these commands emit data (JSON, aligned rows) meant for piping and grepping.
  */
object History {

  /** `bleep history` — list this workspace's recorded compile/test runs, oldest first. */
  case object ListEntries extends BleepBuildCommand {
    override def run(started: Started): Either[BleepException, Unit] = {
      val buildPaths = started.buildPaths
      val ids = TranscriptStore.list(buildPaths)
      if (ids.isEmpty)
        println(s"No history recorded in ${TranscriptStore.dir(buildPaths)}. Run a compile or test first.")
      else {
        val timestampFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss").withZone(ZoneId.systemDefault())
        ids.foreach { id =>
          val t = TranscriptStore.read(buildPaths, id)
          val when = timestampFormat.format(Instant.ofEpochMilli(t.timestampMs))
          println(s"#${t.id}  $when  ${t.mode.padTo(7, ' ')}  client=${t.client}  targets=${t.targets.mkString(",")}")
        }
      }
      Right(())
    }
  }

  /** `bleep history show [id]` — the full transcript of one history entry (latest when omitted), as JSON on stdout. */
  case class Show(
      id: Option[Long],
      project: Option[String],
      query: Option[String],
      limit: Option[Int],
      offset: Option[Int]
  ) extends BleepBuildCommand {
    override def run(started: Started): Either[BleepException, Unit] = {
      val buildPaths = started.buildPaths
      val transcript = id match {
        case Some(id) => TranscriptStore.read(buildPaths, id)
        case None     => TranscriptStore.readLatest(buildPaths)
      }
      println(TranscriptFormat.details(transcript, project, query, limit, offset).spaces2)
      Right(())
    }
  }

  /** `bleep history diff <base> <target>` — what changed between two history entries: rendered for humans by default, the underlying diff document with
    * `--output json`. `--timing` compares durations instead of logical outcome; `--base-dir` resolves the base id in another workspace (the copy-state
    * verification flow: parent's run vs this worktree's run).
    */
  case class Diff(
      base: Long,
      target: Long,
      timing: Boolean,
      limit: Option[Int],
      baseDir: Option[Path],
      output: OutputMode
  ) extends BleepBuildCommand {
    override def run(started: Started): Either[BleepException, Unit] = {
      val targetPaths = started.buildPaths
      val basePaths = baseDir match {
        case Some(dir) => History.workspacePaths(dir, what = "--base-dir")
        case None      => targetPaths
      }
      val baseTranscript: Transcript = TranscriptStore.read(basePaths, base)
      val targetTranscript: Transcript = TranscriptStore.read(targetPaths, target)
      val json =
        if (timing) TranscriptDiff.timing(baseTranscript, targetTranscript, limit.getOrElse(TranscriptDiff.DefaultTimingLimit))
        else TranscriptDiff.mechanical(baseTranscript, targetTranscript)
      output match {
        case OutputMode.Json => println(json.spaces2)
        case OutputMode.Text =>
          println(TranscriptDiffRender.text(json))
          if (!timing) {
            val baseDirArg = baseDir.fold("")(dir => s" --base-dir $dir")
            println(s"timing: bleep history diff $base $target$baseDirArg --timing")
          }
        case OutputMode.Raw => throw new BleepException.Text("--output raw is not supported for diffs; use text or json")
      }
      Right(())
    }
  }

  /** Resolve a workspace's BuildPaths from a directory inside it, Normal variant — the variant the CLI and MCP write. Fails loudly when the directory does not
    * exist or holds no bleep build: pointing the transcript reads at a wrong path must not degrade into "no history entry with that id". Shared by `--base-dir`
    * here and the MCP `directory`/`baseDirectory` arguments; `what` names the argument in error messages.
    */
  def workspacePaths(dir: Path, what: String): BuildPaths = {
    val abs = dir.toAbsolutePath.normalize()
    if (!Files.isDirectory(abs)) throw new BleepException.Text(s"$what is not an existing directory: $abs")
    val buildLoader = BuildLoader.find(abs)
    buildLoader.existing match {
      case Left(be) => throw be
      case Right(_) => ()
    }
    BuildPaths(abs, buildLoader, model.BuildVariant.Normal)
  }
}
