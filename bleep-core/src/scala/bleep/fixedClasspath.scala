package bleep

import java.nio.file.Path

object fixedClasspath {
  def apply(project: ResolvedProject): List[Path] =
    List(
      List(project.classesDir),
      project.resources.getOrElse(Nil),
      project.classpath
    ).flatten
}
