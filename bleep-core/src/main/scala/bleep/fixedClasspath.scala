package bleep

import bloop.config.{Config => b}

import java.nio.file.Path

object fixedClasspath {
  def apply(bloopProject: b.Project): List[Path] =
    List(
      List(bloopProject.classesDir),
      bloopProject.resources.getOrElse(Nil),
      bloopProject.classpath
    ).flatten
}
