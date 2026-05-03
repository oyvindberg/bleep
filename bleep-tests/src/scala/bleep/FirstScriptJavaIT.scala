package bleep

class FirstScriptJavaIT extends IntegrationTestHarness {
  integrationTest("first-script-java workspace compiles") { ws =>
    ws.yaml(
      snippet = "first-script-java/bleep.yaml",
      content = """projects:
                  |  myapp:
                  |    platform:
                  |      name: jvm
                  |      mainClass: com.example.Main
                  |  scripts:
                  |    dependencies:
                  |      - build.bleep:bleepscript:${BLEEP_VERSION}
                  |    java:
                  |      options: -proc:none --release 17
                  |    platform:
                  |      name: jvm
                  |    sources: ./src/main/java
                  |scripts:
                  |  hello:
                  |    main: scripts.HelloScript
                  |    project: scripts
                  |""".stripMargin
    )

    ws.file(
      "myapp/src/java/com/example/Main.java",
      snippet = "first-script-java/Main.java",
      content = """package com.example;
                  |
                  |public class Main {
                  |  public static void main(String[] args) {
                  |    System.out.println("Hello from bleep!");
                  |  }
                  |}
                  |""".stripMargin
    )

    ws.file(
      "scripts/src/main/java/scripts/HelloScript.java",
      snippet = "first-script-java/HelloScript.java",
      content = """package scripts;
                  |
                  |import bleepscript.BleepScript;
                  |import bleepscript.Commands;
                  |import bleepscript.Started;
                  |import java.util.List;
                  |
                  |public final class HelloScript extends BleepScript {
                  |  public HelloScript() {
                  |    super("hello");
                  |  }
                  |
                  |  @Override
                  |  public void run(Started started, Commands commands, List<String> args) {
                  |    int projectCount = started.build().explodedProjects().size();
                  |    started.logger().info("This build has " + projectCount + " projects");
                  |    if (args.isEmpty()) {
                  |      started.logger().info("Hello, world!");
                  |    } else {
                  |      started.logger().info("Hello, " + String.join(" ", args) + "!");
                  |    }
                  |  }
                  |}
                  |""".stripMargin
    )

    ws.compileAll()
    succeed
  }
}
