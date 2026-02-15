package bleep
package internal

import java.io.File
import java.nio.file.Path

object jvmRunCommand {

  /** JVM options to suppress Scala 3 LazyVals sun.misc.Unsafe warnings */
  val scala3CompatOptions: List[String] = List(
    "--add-opens=java.base/sun.misc=ALL-UNNAMED"
  )

  def apply(
      project: ResolvedProject,
      resolvedJvm: Lazy[ResolvedJvm],
      projectName: model.CrossProjectName,
      overrideMainClass: Option[String],
      args: List[String]
  ): Either[BleepException, List[String]] =
    project.platform match {
      case Some(jvm: ResolvedProject.Platform.Jvm) =>
        val cp = fixedClasspath(project)
        val mainClass = overrideMainClass.orElse(jvm.mainClass)
        mainClass match {
          case Some(main) =>
            val jvmOptions = if (jvm.runtimeOptions.nonEmpty) jvm.runtimeOptions else jvm.options
            Right(cmd(resolvedJvm.forceGet, jvmOptions, cp, main, args))
          case None => Left(new BleepException.Text(projectName, "No main found"))
        }
      case _ => Left(new BleepException.Text(projectName, "This codepath can only run JVM projects"))
    }

  def cmd(resolvedJvm: ResolvedJvm, jvmOptions: List[String], cp: List[Path], main: String, args: List[String]): List[String] =
    List(resolvedJvm.javaBin.toString) ++ cmdArgs(jvmOptions, cp, main, args)

  def cmdArgs(jvmOptions: List[String], cp: List[Path], main: String, args: List[String]): List[String] =
    List[List[String]](
      scala3CompatOptions,
      jvmOptions,
      List("-classpath", cp.mkString(File.pathSeparator), main),
      args
    ).flatten
}
