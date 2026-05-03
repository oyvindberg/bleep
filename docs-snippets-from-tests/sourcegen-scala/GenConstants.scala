package scripts

import bleepscript.{BleepCodegenScript, CodegenTarget, Commands, Started}

import java.nio.file.Files
import scala.jdk.CollectionConverters.*

class GenConstants extends BleepCodegenScript("GenConstants") {
  override def run(started: Started, commands: Commands, targets: java.util.List[CodegenTarget], args: java.util.List[String]): Unit =
    targets.asScala.foreach { target =>
      val file = target.sources.resolve("generated/Constants.scala")
      Files.createDirectories(file.getParent)
      Files.writeString(file,
        "package generated\n" +
        "\n" +
        "object Constants:\n" +
        "  val ANSWER: Int = 42\n"
      )
    }
}
