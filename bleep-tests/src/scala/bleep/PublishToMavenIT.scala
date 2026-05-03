package bleep

class PublishToMavenIT extends IntegrationTestHarness {
  integrationTest("publish-to-maven workspace compiles") { ws =>
    ws.yaml(
      snippet = "publish-to-maven/bleep.yaml",
      content = """projects:
                  |  mylib:
                  |    platform:
                  |      name: jvm
                  |    publish:
                  |      groupId: io.github.myusername
                  |      url: https://github.com/myusername/mylib
                  |      description: A useful library that does useful things.
                  |      developers:
                  |        - id: myusername
                  |          name: My Name
                  |          url: https://github.com/myusername
                  |      licenses:
                  |        - name: Apache-2.0
                  |          url: https://www.apache.org/licenses/LICENSE-2.0.txt
                  |      sonatypeCredentialHost: central.sonatype.com
                  |""".stripMargin
    )

    ws.file(
      "mylib/src/java/com/example/Greeter.java",
      snippet = "publish-to-maven/Greeter.java",
      content = """package com.example;
                  |
                  |public final class Greeter {
                  |  private Greeter() {}
                  |
                  |  public static String hello(String name) {
                  |    return "Hello, " + name + "!";
                  |  }
                  |}
                  |""".stripMargin
    )

    ws.compileAll()
    val (started, _, _) = ws.start()
    val publishable = started.build.explodedProjects.values.count(_.publish.isDefined)
    assert(publishable == 1)
  }
}
