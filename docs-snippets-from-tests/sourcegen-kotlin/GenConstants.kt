package scripts

import bleepscript.BleepCodegenScript
import bleepscript.CodegenTarget
import bleepscript.Commands
import bleepscript.Started
import java.nio.file.Files

class GenConstants : BleepCodegenScript("GenConstants") {
  override fun run(started: Started, commands: Commands, targets: List<CodegenTarget>, args: List<String>) {
    for (target in targets) {
      val file = target.sources().resolve("generated/Constants.kt")
      Files.createDirectories(file.parent)
      Files.writeString(file,
        "package generated\n\n" +
        "object Constants {\n" +
        "  const val ANSWER: Int = 42\n" +
        "}\n"
      )
    }
  }
}
