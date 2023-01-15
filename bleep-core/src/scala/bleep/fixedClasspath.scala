package bleep

import bloop.config.Config

import java.nio.file.Path

object fixedClasspath {
  def apply(bloopProject: Config.Project): List[Path] =
    List(
      List(bloopProject.classesDir),
      bloopProject.resources.getOrElse(Nil),
      bloopProject.classpath
    ).flatten
}
