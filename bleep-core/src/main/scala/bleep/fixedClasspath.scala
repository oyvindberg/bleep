package bleep

import bloop.config.{Config => b}

import java.nio.file.{Path, Paths}

/** The way we invoke bloop for now means that the class files are not put where we say we want them. that is only to be done if the issuer of the BSP compile
  * command "owns" the directory.
  *
  * We'll research the BSP story going forward, for now just adjust directories.
  *
  * Note: When intellij issues the "compile" commands the class files end up elsewhere. As long as we always send "compile" through bloop CLI before doing
  * anything which requires the class files this should work.
  */
object fixedClasspath {
  private val classes = Paths.get("classes")

  def apply(bloopProject: b.Project): List[Path] = {
    def bloopCliClassesDir(projName: String): Path =
      bloopProject.workspaceDir.get / Defaults.BleepBloopFolder / projName / "bloop-bsp-clients-classes/classes-bloop-cli"

    val projectName = model.ProjectName(bloopProject.name)

    val projectClassPath = bloopCliClassesDir(projectName.value)

    val patched = bloopProject.classpath.map {
      case path if path.endsWith(classes) => bloopCliClassesDir(path.getParent.getParent.getFileName.toString)
      case path                           => path
    }

    List(
      List(projectClassPath),
      bloopProject.resources.getOrElse(Nil),
      patched
    ).flatten
  }
}
