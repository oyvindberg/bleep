package scripts

import bleepscript.{BleepScript, Commands, Started}

import scala.jdk.CollectionConverters.*

class HelloScript extends BleepScript("hello") {
  override def run(started: Started, commands: Commands, args: java.util.List[String]): Unit = {
    val projectCount = started.build.explodedProjects.size
    started.logger.info(s"This build has $projectCount projects")
    args.asScala.toList match {
      case Nil => started.logger.info("Hello, world!")
      case xs  => started.logger.info(s"Hello, ${xs.mkString(" ")}!")
    }
  }
}
