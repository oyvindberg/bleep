package bleep

import java.nio.file.Path

object fixedClasspath {
  def apply(project: ResolvedProject): List[Path] =
    project.classesDir :: project.resources(Usage.Runtime) ::: project.classpath(Usage.Runtime)
}
