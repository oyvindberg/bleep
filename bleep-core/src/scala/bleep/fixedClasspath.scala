package bleep

import bloop.config.Config

import java.nio.file.Path

object fixedClasspath {
  def apply(bloopProject: Config.Project, includeResources: Boolean): List[Path] =
    List(
      List(bloopProject.classesDir),
      if (includeResources) bloopProject.resources.getOrElse(Nil) else Nil,
      bloopProject.classpath
    ).flatten
}
