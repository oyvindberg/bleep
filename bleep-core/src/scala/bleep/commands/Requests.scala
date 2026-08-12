package bleep
package commands

import bleep.requests.{RequestDiff, Transcript, TranscriptFormat, TranscriptStore}

import java.nio.file.{Files, Path}
import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneId}

/** CLI surface over the per-workspace request transcripts the daemon writes (`<workspace>/.bleep/builds/<variant>/requests/`). Pure file reads — no daemon
  * connection is made, which is the point: history survives daemon restarts and can be inspected from a machine where no daemon runs at all.
  *
  * Output goes to stdout via println, not the logger: these commands emit data (JSON, aligned rows) meant for piping and grepping.
  */
object Requests {

  /** `bleep requests` — list recorded request transcripts in this workspace, oldest first. */
  case object ListRequests extends BleepBuildCommand {
    override def run(started: Started): Either[BleepException, Unit] = {
      val buildPaths = started.buildPaths
      val ids = TranscriptStore.list(buildPaths)
      if (ids.isEmpty)
        println(s"No requests recorded in ${TranscriptStore.dir(buildPaths)}. Run a compile or test first.")
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

  /** `bleep details [id]` — the full transcript of one request (latest when omitted), as JSON on stdout. */
  case class Details(
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

  /** `bleep diff <base> <target>` — what changed between two requests, as JSON on stdout. `--timing` compares durations instead of logical outcome;
    * `--base-dir` resolves the base id in another workspace (the copy-state verification flow: parent's run vs this worktree's run).
    */
  case class Diff(
      base: Long,
      target: Long,
      timing: Boolean,
      limit: Option[Int],
      baseDir: Option[Path]
  ) extends BleepBuildCommand {
    override def run(started: Started): Either[BleepException, Unit] = {
      val targetPaths = started.buildPaths
      val basePaths = baseDir match {
        case Some(dir) => Diff.workspacePaths(dir)
        case None      => targetPaths
      }
      val baseTranscript: Transcript = TranscriptStore.read(basePaths, base)
      val targetTranscript: Transcript = TranscriptStore.read(targetPaths, target)
      val json =
        if (timing) RequestDiff.timing(baseTranscript, targetTranscript, limit.getOrElse(RequestDiff.DefaultTimingLimit))
        else RequestDiff.mechanical(baseTranscript, targetTranscript)
      println(json.spaces2)
      Right(())
    }
  }

  object Diff {

    /** Resolve another workspace's BuildPaths the way the daemon's copy-state endpoint does: from its root directory, Normal variant. */
    def workspacePaths(dir: Path): BuildPaths = {
      val abs = dir.toAbsolutePath.normalize()
      if (!Files.isDirectory(abs)) throw new BleepException.Text(s"--base-dir is not an existing directory: $abs")
      val buildLoader = BuildLoader.find(abs)
      BuildPaths(abs, buildLoader, model.BuildVariant.Normal)
    }
  }
}
