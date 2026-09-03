package bleep

/** Maven BOM import, end to end: a project lists `boms:`, and every dependency resolution it participates in is pinned to the versions that BOM's
  * `dependencyManagement` implies — including dependencies written with NO version of their own, whose version the BOM supplies. This is the Maven workflow
  * (`<dependencyManagement>` + version-less `<dependency>`) that bleep used to lack.
  *
  * Two things are pinned here, both against a real published BOM (`com.fasterxml.jackson:jackson-bom`):
  *   1. a version-less dependency resolves to the BOM's version, and
  *   2. that constraint travels along `dependsOn` — a library declaring the BOM constrains a consumer that declares none, so the whole subtree resolves against
  *      one version. That is what lets a platform BOM (Quarkus, Spring Boot, the AWS SDK) govern an app's entire dependency universe from one line.
  */
class BomIT extends IntegrationTestHarness {

  private val bomVersion = "2.17.2"

  private def jars(started: Started, project: String, contains: String): List[String] =
    started
      .resolvedProject(model.CrossProjectName(model.ProjectName(project), None))
      .classpath
      .map(_.toString)
      .filter(_.contains(contains))
      .toList

  integrationTest("a version-less dependency resolves to the version its BOM manages") { ws =>
    ws.yaml(
      s"""projects:
         |  app:
         |    platform:
         |      name: jvm
         |    boms:
         |      - com.fasterxml.jackson:jackson-bom:$bomVersion
         |    dependencies:
         |      - com.fasterxml.jackson.core:jackson-databind
         |""".stripMargin
    )
    val (started, _, _) = ws.start()

    val databind = jars(started, "app", "jackson-databind")
    assert(
      databind.exists(_.contains(s"jackson-databind-$bomVersion")),
      s"expected jackson-databind-$bomVersion from the BOM, got:\n${databind.mkString("\n")}"
    )
    // The transitives the BOM also manages land at the BOM's version too — Maven's pin-the-whole-subtree semantics, not just the direct request.
    val core = jars(started, "app", "jackson-core")
    assert(
      core.exists(_.contains(s"jackson-core-$bomVersion")),
      s"expected jackson-core-$bomVersion (a transitive the BOM manages), got:\n${core.mkString("\n")}"
    )
  }

  integrationTest("a BOM declared on a library constrains a consumer that declares none") { ws =>
    ws.yaml(
      s"""projects:
         |  lib:
         |    platform:
         |      name: jvm
         |    boms:
         |      - com.fasterxml.jackson:jackson-bom:$bomVersion
         |  app:
         |    platform:
         |      name: jvm
         |    dependsOn: lib
         |    dependencies:
         |      - com.fasterxml.jackson.core:jackson-databind
         |""".stripMargin
    )
    val (started, _, _) = ws.start()

    // `app` names no BOM and no version, yet its version-less databind resolves to the library's BOM version — the constraint travelled down `dependsOn`.
    val databind = jars(started, "app", "jackson-databind")
    assert(
      databind.exists(_.contains(s"jackson-databind-$bomVersion")),
      s"expected the library's BOM to pin the consumer's version-less databind to $bomVersion, got:\n${databind.mkString("\n")}"
    )
  }
}
