package bleep

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

/** Writes one runnable project per (framework, platform) so the real `bleep` CLI can be run against each and its output captured verbatim.
  *
  * Temporary: exists to produce the evidence report, not to be part of the suite.
  */
class ZzExportScenariosTest extends IntegrationTestHarness with PlatformFrameworkHarness {

  private val root: Path = Paths.get(sys.env.getOrElse("SCENARIO_EXPORT_DIR", "/tmp/bleep-scenarios"))

  private val exportPrelude: String =
    s"""$$schema: https://raw.githubusercontent.com/oyvindberg/bleep/master/schema.json
       |$$version: 1.0.0-M11
       |jvm:
       |  name: ${model.Jvm.graalvm.name}
       |""".stripMargin

  private val platforms: List[FixturePlatform] = List(
    FixturePlatform.Jvm(model.Versions.Scala3),
    FixturePlatform.Jvm(model.Versions.Scala213),
    FixturePlatform.Js(model.Versions.Scala3, model.Versions.ScalaJs1, model.Versions.Node),
    FixturePlatform.Js(model.Versions.Scala213, model.Versions.ScalaJs1, model.Versions.Node),
    FixturePlatform.Native(model.Versions.Scala3, model.Versions.ScalaNative05),
    FixturePlatform.Native(model.Versions.Scala213, model.Versions.ScalaNative05),
    FixturePlatform.KotlinJs(model.Versions.Kotlin24, model.Versions.Node),
    FixturePlatform.KotlinNative(model.Versions.Kotlin24)
  )

  integrationTest("export every supported scenario") { _ =>
    var count = 0
    platforms.foreach { platform =>
      TestFrameworkFixture.pinnedFor(platform.id, platform.scalaBinaryVersion).foreach { case (fixture, version) =>
        val slug = s"${platform.id}-${platform.scalaBinaryVersion.getOrElse("na")}-${fixture.name}".replace('.', '_')
        val dir = root.resolve(slug)
        Files.createDirectories(dir)

        def write(rel: String, content: String): Unit = {
          val p = dir.resolve(rel)
          Files.createDirectories(p.getParent)
          Files.write(p, content.getBytes(StandardCharsets.UTF_8)): Unit
        }

        write("bleep.yaml", exportPrelude + yamlFor(fixture, version, platform))
        write(s"$projectName/src/${fixture.language.sourceDir}/${fixture.relPath}", fixture.source)
        if (fixture.hasCtorErrorVariant)
          write(s"$projectName/src/${fixture.language.sourceDir}/${fixture.ctorErrorRelPath}", fixture.ctorErrorSource)
        if (fixture.hasGreenVariant)
          write(s"$projectName/src/${fixture.language.sourceDir}/${fixture.greenRelPath}", fixture.greenSource)
        fixture.extraFiles.foreach { case (rel, content) => write(s"$projectName/$rel", content) }
        // Metadata for the report generator, so it does not have to re-derive any of this.
        write(
          "scenario.properties",
          List(
            s"framework=${fixture.name}",
            s"version=$version",
            s"platform=${platform.id}",
            s"platformTitle=${platform.describe}",
            s"scala=${platform.scalaBinaryVersion.getOrElse("")}",
            s"suite=${fixture.suiteFqn}",
            s"ctorSuite=${if (fixture.hasCtorErrorVariant) fixture.ctorErrorSuiteFqn else ""}",
            s"greenSuite=${if (fixture.hasGreenVariant) fixture.greenSuiteFqn else ""}",
            s"language=${fixture.language}",
            s"skips=${fixture.skippedTestName.isDefined}",
            s"assertionKind=${fixture.failureReporting(platform.id, platform.scalaBinaryVersion).assertionFailure}",
            s"exceptionKind=${fixture.failureReporting(platform.id, platform.scalaBinaryVersion).uncaughtException}",
            s"explanation=${fixture.failureReporting(platform.id, platform.scalaBinaryVersion).explanation}",
            s"ctorFailure=${if (fixture.hasCtorErrorVariant) fixture.ctorFailureReport(platform.id).toString else "NotApplicable"}"
          ).mkString("\n")
        )
        count += 1
      }
    }
    info(s"exported $count scenarios to $root")
    assert(count > 0)
  }
}
