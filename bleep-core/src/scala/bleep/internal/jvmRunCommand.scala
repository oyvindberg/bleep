package bleep
package internal

import bloop.config.Config

import java.io.File
import java.nio.file.Path

object jvmRunCommand {
  def apply(
      bloopProject: Config.Project,
      resolvedJvm: Lazy[ResolvedJvm],
      project: model.CrossProjectName,
      overrideMainClass: Option[String],
      args: List[String]
  ): Either[BleepException, List[String]] =
    bloopProject.platform match {
      case Some(jvm: Config.Platform.Jvm) =>
        val cp = fixedClasspath(bloopProject, false)
        overrideMainClass.orElse(bloopProject.platform.flatMap(_.mainClass)) match {
          case Some(main) =>
            val jvmOptions = jvm.runtimeConfig.getOrElse(jvm.config).options
            Right(cmd(resolvedJvm.forceGet, jvmOptions, cp, main, args))
          case None => Left(new BleepException.Text(project, "No main found"))
        }
      case _ => Left(new BleepException.Text(project, "This codepath can only run JVM projects"))
    }

  def cmd(resolvedJvm: ResolvedJvm, jvmOptions: List[String], cp: List[Path], main: String, args: List[String]): List[String] =
    List(resolvedJvm.javaBin.toString) ++ cmdArgs(jvmOptions, cp, main, args)

  def cmdArgs(jvmOptions: List[String], cp: List[Path], main: String, args: List[String]): List[String] =
    List[List[String]](
      jvmOptions,
      List("-classpath", cp.mkString(File.pathSeparator), main),
      args
    ).flatten
}
