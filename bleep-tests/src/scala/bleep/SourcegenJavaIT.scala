package bleep

class SourcegenJavaIT extends IntegrationTestHarness {
  integrationTest("sourcegen-java workspace compiles") { ws =>
    ws.yaml(
      content = s"""projects:
                   |  myapp:
                   |    java:
                   |      version: "${model.Jvm.graalvm.majorVersion}"
                   |    platform:
                   |      name: jvm
                   |      mainClass: com.example.Main
                   |    sourcegen:
                   |      project: scripts
                   |      main: scripts.GenConstants
                   |  scripts:
                   |    dependencies:
                   |      - build.bleep:bleepscript:$${BLEEP_VERSION}
                   |    java:
                   |      version: "${model.Jvm.graalvm.majorVersion}"
                   |    platform:
                   |      name: jvm
                   |""".stripMargin,
      snippet = "sourcegen-java/bleep.yaml"
    )

    ws.file(
      "myapp/src/java/com/example/Main.java",
      content = """package com.example;
                  |
                  |import generated.Constants;
                  |
                  |public final class Main {
                  |  public static void main(String[] args) {
                  |    System.out.println("answer: " + Constants.ANSWER);
                  |  }
                  |}
                  |""".stripMargin,
      snippet = "sourcegen-java/Main.java"
    )

    ws.file(
      "scripts/src/java/scripts/GenConstants.java",
      content = """package scripts;
                  |
                  |import bleepscript.BleepCodegenScript;
                  |import bleepscript.CodegenTarget;
                  |import bleepscript.Commands;
                  |import bleepscript.Started;
                  |import java.io.IOException;
                  |import java.nio.file.Files;
                  |import java.nio.file.Path;
                  |import java.util.List;
                  |
                  |public final class GenConstants extends BleepCodegenScript {
                  |  public GenConstants() {
                  |    super("GenConstants");
                  |  }
                  |
                  |  @Override
                  |  public void run(Started started, Commands commands, List<CodegenTarget> targets, List<String> args) {
                  |    for (CodegenTarget target : targets) {
                  |      Path file = target.sources().resolve("generated/Constants.java");
                  |      try {
                  |        Files.createDirectories(file.getParent());
                  |        Files.writeString(file,
                  |            "package generated;\n" +
                  |            "\n" +
                  |            "public final class Constants {\n" +
                  |            "  public static final int ANSWER = 42;\n" +
                  |            "}\n");
                  |      } catch (IOException e) {
                  |        throw new RuntimeException(e);
                  |      }
                  |    }
                  |  }
                  |}
                  |""".stripMargin,
      snippet = "sourcegen-java/GenConstants.java"
    )

    ws.compileAll()
    succeed
  }
}
