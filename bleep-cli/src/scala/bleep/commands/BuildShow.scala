package bleep
package commands

import bleep.BleepException
import bleep.internal.asYamlString
import bleep.model.{JsonMap, JsonSet}
import bloop.config.ConfigCodecs
import cats.data.NonEmptyList

object BuildShow {
  case class Short(started: Started, projects: NonEmptyList[model.ProjectName]) extends BleepCommand {
    override def run(): Either[BleepException, Unit] = {
      projects.toList.foreach { projectName =>
        val p = started.rawBuild.projects.value(projectName)
        println(fansi.Color.Red(projectName.value))
        println(asYamlString(p))
      }

      Right(())
    }
  }
  case class Exploded(started: Started, projects: List[model.CrossProjectName]) extends BleepCommand {
    override def run(): Either[BleepException, Unit] = {
      projects.foreach { crossProjectName =>
        val p0 = started.build.projects(crossProjectName)
        // we don't currently do these cleanups to be able to go back to short version
        val p = p0.copy(cross = JsonMap.empty, `extends` = JsonSet.empty)
        println(fansi.Color.Red(crossProjectName.value))
        println(asYamlString(p))
      }

      Right(())
    }
  }

  case class Bloop(started: Started, projects: List[model.CrossProjectName]) extends BleepCommand {
    override def run(): Either[BleepException, Unit] = {
      projects.foreach { crossProjectName =>
        val f = started.bloopFiles(crossProjectName).forceGet
        println(fansi.Color.Red(crossProjectName.value))
        println(ConfigCodecs.toStr(f))
      }

      Right(())
    }
  }
}
