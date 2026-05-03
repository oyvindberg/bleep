package scripts;

import bleepscript.BleepCodegenScript;
import bleepscript.CodegenTarget;
import bleepscript.Commands;
import bleepscript.Started;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

public final class GenConstants extends BleepCodegenScript {
  public GenConstants() {
    super("GenConstants");
  }

  @Override
  public void run(Started started, Commands commands, List<CodegenTarget> targets, List<String> args) {
    for (CodegenTarget target : targets) {
      Path file = target.sources().resolve("generated/Constants.java");
      try {
        Files.createDirectories(file.getParent());
        Files.writeString(file,
            "package generated;\n" +
            "\n" +
            "public final class Constants {\n" +
            "  public static final int ANSWER = 42;\n" +
            "}\n");
      } catch (IOException e) {
        throw new RuntimeException(e);
      }
    }
  }
}
