package bleep
package commands

import bleep.internal.asYamlString
import com.monovore.decline.Opts

import scala.collection.immutable.SortedSet
import scala.util.control.NonFatal

case class BuildDiff(started: Started, opts: BuildDiff.Options) extends BleepCommand {
  override def run(): Either[BuildException, Unit] = {
    val revision = opts.revision.getOrElse("HEAD")

    val oldBuildStr: Lazy[Either[BuildException, String]] = Lazy {
      try
        Right(scala.sys.process.Process(List("git", "show", s"$revision:${constants.BuildFileName}"), started.buildPaths.buildDir.toFile).!!)
      catch {
        case NonFatal(th) => Left(new BuildException.Cause(th, "couldn't load build"))
      }
    }

    BuildLoader.Existing(started.buildPaths.bleepYamlFile, oldBuildStr).build.forceGet.map { build =>
      val oldProjects = ExplodedBuild.of(build).projects
      val newProjects = started.build.projects
      val allProjectNames = SortedSet.empty[model.CrossProjectName] ++ oldProjects.keys ++ newProjects.keys
      allProjectNames.foreach { projectName =>
        val p = s"Project ${fansi.Bold.On(projectName.value)}"
        val old = oldProjects.getOrElse(projectName, model.Project.empty)
        val new_ = newProjects.getOrElse(projectName, model.Project.empty)
        val maybeRemoved = Some(old.removeAll(new_)).filterNot(_.isEmpty).map(asYamlString(_)).map(fansi.Color.Red(_))
        val maybeAdded = Some(new_.removeAll(old)).filterNot(_.isEmpty).map(asYamlString(_)).map(fansi.Color.Green(_))
        (maybeRemoved, maybeAdded) match {
          case (None, None)                 => ()
          case (Some(removed), None)        => println(s"$p: \n$removed")
          case (None, Some(added))          => println(s"$p: \n$added")
          case (Some(removed), Some(added)) => println(s"$p: \n$removed$added")
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
