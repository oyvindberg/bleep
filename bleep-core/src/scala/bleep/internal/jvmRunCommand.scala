package bleep
package internal

import bloop.config.Config

import java.io.File

object jvmRunCommand {
  def apply(
      started: Started,
      project: model.CrossProjectName,
      overrideMainClass: Option[String],
      args: List[String]
  ): Either[BleepException, List[String]] = {
    val bloopProject = started.bloopProjects(project)
    bloopProject.platform match {
      case Some(jvm: Config.Platform.Jvm) =>
        val cp = fixedClasspath(bloopProject)
        overrideMainClass.orElse(bloopProject.platform.flatMap(_.mainClass)) match {
          case Some(main) =>
            Right(
              List[List[String]](
                List(started.jvmCommand.toString),
                jvm.runtimeConfig.getOrElse(jvm.config).options,
                List("-classpath", cp.mkString(File.pathSeparator), main),
                args
              ).flatten
            )
          case None => Left(new BleepException.Text(project, "No main found"))
        }
      case _ => Left(new BleepException.Text(project, "This codepath can only run JVM projects"))
    }
  }
}
