package bleep

import bleep.commands.Publish
import bleep.packaging.ManifestCreator

class PublishToHttpMavenRepoIT extends IntegrationTestHarness {

  integrationTest("publish to a plain HTTP Maven repo with Basic Auth") { ws =>
    HttpMavenRepoServer.withServer(user = "alice", password = "s3cret") { server =>
      val repoUri = server.repoUri

      ws.yaml(
        s"""resolvers:
           |  - name: company-releases
           |    type: maven
           |    uri: $repoUri
           |
           |projects:
           |  mylib:
           |    platform:
           |      name: jvm
           |    publish:
           |      groupId: com.test
           |""".stripMargin
      )

      // Snippet shown in the docs uses a realistic Artifactory URL, not the test's localhost port.
      ws.attachSnippetYaml(
        snippet = "artifactory/bleep.yaml",
        userYaml = """resolvers:
                     |  - name: company-releases
                     |    type: maven
                     |    uri: https://artifactory.company.com/artifactory/libs-release
                     |  - name: company-snapshots
                     |    type: maven
                     |    uri: https://artifactory.company.com/artifactory/libs-snapshot
                     |
                     |projects:
                     |  mylib:
                     |    platform:
                     |      name: jvm
                     |    publish:
                     |      groupId: com.mycompany
                     |""".stripMargin
      )

      ws.file(
        "mylib/src/java/com/test/Greeter.java",
        snippet = "artifactory/Greeter.java",
        content = """package com.test;
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

      ws.setStaticAuth(repoUri, "alice", "s3cret")

      val (started, commands, _) = ws.start()
      val mylib = model.CrossProjectName(model.ProjectName("mylib"), None)

      commands.publish(
        Publish.Options(
          versionOverride = Some("1.0.0"),
          versionFallback = None,
          assertRelease = false,
          dryRun = false,
          target = Publish.Target.Resolver(model.ResolverName("company-releases")),
          projectNames = Array(mylib),
          manifestCreator = ManifestCreator.default
        )
      )

      val files = HttpMavenRepoServer.storedFiles(server)
      val expected = List(
        "com/test/mylib/1.0.0/mylib-1.0.0.jar",
        "com/test/mylib/1.0.0/mylib-1.0.0.pom",
        "com/test/mylib/1.0.0/mylib-1.0.0-sources.jar",
        "com/test/mylib/1.0.0/mylib-1.0.0-javadoc.jar"
      )
      expected.foreach(p => assert(files.contains(p), s"expected $p in $files"))
      assert(files.exists(_.endsWith(".pom.md5")), s"expected checksums in $files")
      assert(files.exists(_.endsWith(".jar.sha1")), s"expected checksums in $files")
      assert(server.unauthorizedRequests.isEmpty, s"no request should have been sent without auth, got ${server.unauthorizedRequests}")

      val pomBytes = java.nio.file.Files.readAllBytes(server.storedFile("com/test/mylib/1.0.0/mylib-1.0.0.pom"))
      val pomStr = new String(pomBytes, "UTF-8")
      assert(pomStr.contains("<groupId>com.test</groupId>"), s"pom missing groupId:\n$pomStr")
      assert(pomStr.contains("<artifactId>mylib</artifactId>"), s"pom missing artifactId:\n$pomStr")
      assert(pomStr.contains("<version>1.0.0</version>"), s"pom missing version:\n$pomStr")
      val _ = started
      succeed
    }
  }

  integrationTest("publish without configured auth: server rejects with 401, publish fails") { ws =>
    HttpMavenRepoServer.withServer(user = "alice", password = "s3cret") { server =>
      val repoUri = server.repoUri

      ws.yaml(
        s"""resolvers:
             |  - name: company-releases
             |    type: maven
             |    uri: $repoUri
             |
             |projects:
             |  mylib:
             |    platform:
             |      name: jvm
             |    publish:
             |      groupId: com.test
             |""".stripMargin
      )
      ws.file(
        "mylib/src/java/com/test/Greeter.java",
        """package com.test;
            |public final class Greeter {
            |  public static String hello() { return "hi"; }
            |}
            |""".stripMargin
      )

      // Note: NO setStaticAuth call. Publish will go out without an Authorization header.

      val (_, commands, _) = ws.start()
      val mylib = model.CrossProjectName(model.ProjectName("mylib"), None)

      val ex = intercept[Throwable] {
        commands.publish(
          Publish.Options(
            versionOverride = Some("1.0.0"),
            versionFallback = None,
            assertRelease = false,
            dryRun = false,
            target = Publish.Target.Resolver(model.ResolverName("company-releases")),
            projectNames = Array(mylib),
            manifestCreator = ManifestCreator.default
          )
        )
      }
      val msg = Option(ex.getMessage).getOrElse("") + Option(ex.getCause).map(_.getMessage).getOrElse("")
      assert(msg.contains("HTTP 401") || msg.contains("Failed to publish"), s"unexpected error: $msg")
      assert(server.unauthorizedRequests.nonEmpty, "server should have recorded the unauth attempt")
      succeed
    }
  }
}
