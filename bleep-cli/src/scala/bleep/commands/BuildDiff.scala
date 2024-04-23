package bleep
package commands

import com.monovore.decline.Opts

import scala.collection.immutable.SortedSet
import scala.util.control.NonFatal

case class BuildDiff(opts: BuildDiff.Options, projects: Array[model.CrossProjectName]) extends BleepBuildCommand {
  override def run(started: Started): Either[BleepException, Unit] = {
    val revision = opts.revision.getOrElse("HEAD")

    val oldBuildStr: Lazy[Either[BleepException, String]] = Lazy {
      try
        Right(scala.sys.process.Process(List("git", "show", s"$revision:${BuildLoader.BuildFileName}"), started.buildPaths.buildDir.toFile).!!)
      catch {
        case NonFatal(th) => Left(new BleepException.Cause(th, s"couldn't load ${BuildLoader.BuildFileName}} from $revision"))
      }
    }

    BuildLoader.Existing(started.buildPaths.bleepYamlFile, oldBuildStr).buildFile.forceGet.map { build =>
      val oldProjects = model.Build.FileBacked(build).explodedProjects
      val newProjects = started.build.explodedProjects
      val allProjectNames = SortedSet.empty[model.CrossProjectName] ++ oldProjects.keys ++ newProjects.keys
      val filteredAllProjectNames = if (projects.isEmpty) allProjectNames else allProjectNames.intersect(projects.toSet)
      filteredAllProjectNames.foreach { projectName =>
        val header = s"Project ${fansi.Bold.On(projectName.value)}"
        val old = oldProjects.getOrElse(projectName, model.Project.empty)
        val new_ = newProjects.getOrElse(projectName, model.Project.empty)
        val maybeRemoved = Some(old.removeAll(new_)).filterNot(_.isEmpty).map(yaml.encodeShortened(_)).map(fansi.Color.Red(_))
        val maybeAdded = Some(new_.removeAll(old)).filterNot(_.isEmpty).map(yaml.encodeShortened(_)).map(fansi.Color.Green(_))
        (maybeRemoved, maybeAdded) match {
          case (None, None)                 => ()
          case (Some(removed), None)        => println(s"$header: \n$removed")
          case (None, Some(added))          => println(s"$header: \n$added")
          case (Some(removed), Some(added)) => println(s"$header: \n$removed$added")
        }
      }
      ()
    }
  }
}

object BuildDiff {
  case class Options(
      revision: Option[String]
  )

  val revision: Opts[Option[String]] = Opts
    .option[String](
      "revision",
      "git revision to diff build against",
      "r"
    )
    .orNone

  val opts: Opts[Options] =
    revision.map(Options.apply)

}
