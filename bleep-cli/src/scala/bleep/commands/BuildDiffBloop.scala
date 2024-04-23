package bleep
package commands

import bloop.config.{Config, ConfigCodecs}
import cats.Show
import cats.syntax.show.*
import com.github.plokhotnyuk.jsoniter_scala.core.writeToString
import diffson.*
import diffson.circe.jsonyCirce
import diffson.jsonpatch.*
import diffson.jsonpatch.lcsdiff.remembering.JsonDiffDiff
import diffson.lcs.Patience
import fansi.Color.{Green, Red}
import fansi.Str
import io.circe.*

import scala.collection.immutable.SortedSet
import scala.util.control.NonFatal

case class BuildDiffBloop(opts: BuildDiff.Options, projects: Array[model.CrossProjectName]) extends BleepBuildCommand {
  implicit val lcs: Patience[Json] = new Patience
  implicit val showStr: Show[Str] = _.toString()

  override def run(started: Started): Either[BleepException, Unit] = {
    val revision = opts.revision.getOrElse("HEAD")

    val oldBuildStr: Lazy[Either[BleepException, String]] = Lazy {
      try
        Right(scala.sys.process.Process(List("git", "show", s"$revision:${BuildLoader.BuildFileName}"), started.buildPaths.buildDir.toFile).!!)
      catch {
        case NonFatal(th) => Left(new BleepException.Cause(th, s"couldn't load ${BuildLoader.BuildFileName}} from $revision"))
      }
    }

    val oldStarted = bootstrap
      .from(
        Prebootstrapped(
          logger = started.logger.withPath("loading old build"),
          userPaths = started.userPaths,
          buildPaths = started.buildPaths,
          existingBuild = BuildLoader.Existing(started.buildPaths.bleepYamlFile, oldBuildStr),
          ec = started.executionContext
        ),
        genBloopFiles = GenBloopFiles.InMemory,
        rewrites = Nil,
        config = started.config,
        resolverFactory = (_, _, _) => started.resolver
      )
      .orThrow

    val oldProjects = oldStarted.bloopFiles
    val newProjects = started.bloopFiles
    val allProjectNames = SortedSet.empty[model.CrossProjectName] ++ oldProjects.keys ++ newProjects.keys
    val filteredAllProjectNames = if (projects.isEmpty) allProjectNames else allProjectNames.intersect(projects.toSet)

    filteredAllProjectNames.foreach { projectName =>
      val old = oldProjects.get(projectName).map(circeJsonFor)
      val new_ = newProjects.get(projectName).map(circeJsonFor)

      def printHeader(status: Str): Unit =
        println(s"${fansi.Bold.On(projectName.value)}: $status")

      (old, new_) match {
        case (None, None) => sys.error("unexpected")
        case (Some(_), None) =>
          printHeader("Removed")
        case (None, Some(_)) =>
          printHeader("Added")
        case (Some(old), Some(new_)) =>
          val diffs: List[Operation[Json]] = diff(old, new_).ops

          val formatted = diffs.flatMap(render)

          if (formatted.nonEmpty) {
            printHeader("Changed:")
            formatted.foreach(println)
          } else {
            printHeader("Unchanged")
          }
      }
    }
    Right(())
  }

  def circeJsonFor(f: Lazy[Config.File]): Json =
    io.circe.parser.parse(writeToString(f.forceGet.project)(ConfigCodecs.codecProject)).orThrowWithError("couldn't parse bloop json")

  def render(op: Operation[Json]): Option[Str] =
    op match {
      case Add(path, value) =>
        Some(show"Added ($path) ${Green(value.spaces2)}")
      case Remove(path, Some(old)) =>
        Some(show"Removed ($path) ${Red(old.spaces2)}")
      case Remove(path, None) =>
        Some(show"Removed ($path)")
      case Replace(path, value, Some(old)) =>
        Some(show"Replaced ($path) ${Red(old.spaces2)} => ${Green(value.spaces2)}")
      case Replace(path, value, None) =>
        Some(show"Replaced ($path). New value: ${Green(value.spaces2)}")
      case Move(from, path) =>
        Some(show"Moved $from to $path")
      case Copy(from, path) =>
        Some(show"Copied $from to $path")
      case jsonpatch.Test(_, _) =>
        None
    }
}
