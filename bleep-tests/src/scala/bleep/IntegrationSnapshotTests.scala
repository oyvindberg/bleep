package bleep

import bleep.internal.FileUtils
import bleep.testing.{GenBloopFiles, ImportRoundtrip, SnapshotTest, TemplateStats}
import io.circe.syntax.EncoderOps
import org.scalatest.Assertion

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.concurrent.ExecutionContext

class IntegrationSnapshotTests extends SnapshotTest {
  absolutePaths.sortedValues.foreach(println)
  val userPaths = UserPaths.fromAppDirs

  /** [[absolutePaths]] with the machine-specific side normalized to `/`, so filling a placeholder never introduces a backslash.
    *
    * `bloopFileStrings` holds whole bloop JSON documents as JSON strings, so a filled value is spliced into text that is itself parsed as JSON later. On
    * Windows the raw `D:\a\bleep\bleep` turned `"directory": "<BLEEP_GIT>/..."` into `"directory": "D:\a\bleep\bleep/..."`, where `\a` and `\b` are not valid
    * JSON escapes — jsoniter then failed with `expected '}' or ','`. Forward slashes are accepted everywhere: `Path.of("D:/a/b")` is a normal Windows path.
    *
    * No-op on Unix, where these values contain no backslashes to begin with. Only `fill` is affected; `templatize` still needs the platform's real spelling to
    * match, though regenerating snapshots on Windows has a separate unfixed problem (`Replacements.Replacer.path` calls `Path.of` on a templated string).
    */
  private val fillPaths: model.Replacements =
    model.Replacements.ofReplacements(absolutePaths.sortedValues.map { case (abs, placeholder) => (abs.replace('\\', '/'), placeholder) })

  /** What the input and the imported build are templatized with: [[absolutePaths]] without the os and architecture masks. Both are read back and resolved, and
    * a mask cannot be undone: play depends on netty for `linux-aarch_64`, which came back as `<MASKED_OS>-aarch_64`. The masks are for what is only compared,
    * bloop files and reports. Inputs recorded before keep their masks, which reading still fills
    */
  private val inputPaths: model.Replacements =
    model.Replacements.ofReplacements(absolutePaths.sortedValues.filterNot { case (_, placeholder) => placeholder.startsWith("<MASKED_") })

  /** Substitute `<BLEEP_GIT>`-style placeholders into the parsed snapshot JSON, before it is decoded into [[sbtimport.ImportInputData]].
    *
    * `ImportInputData` decodes many fields to `java.nio.file.Path`, so decoding first and substituting afterwards only ever worked by accident: `<` is a legal
    * filename character on Unix, so the intermediate `Path.of("<BLEEP_GIT>/...")` was nonsense but legal. Windows rejects `<` outright, so decoding threw
    * InvalidPathException and every snapshot test failed before it could read its input at all.
    *
    * Done on the parsed AST rather than the raw text on purpose: splicing into the text would corrupt JSON string escapes. `contents` of a `GeneratedFile` is
    * skipped to preserve the previous `replace(fill, rewriteGeneratedFiles = false)` semantics — those stay templatized on read.
    */
  private def fillPlaceholders(json: io.circe.Json): io.circe.Json = {
    import io.circe.Json
    def go(json: Json, fillStrings: Boolean): Json =
      json.fold(
        Json.Null,
        Json.fromBoolean,
        Json.fromJsonNumber,
        str => if (fillStrings) Json.fromString(fillPaths.fill.string(str)) else json,
        arr => Json.fromValues(arr.map(go(_, fillStrings))),
        obj => Json.fromFields(obj.toIterable.map { case (k, v) => (fillPaths.fill.string(k), go(v, fillStrings && k != "contents")) })
      )
    go(json, fillStrings = true)
  }

  // picked because it exists for apple arm64, and is old
  private val jvm8: model.Jvm = model.Jvm("liberica:8.0.452", None)

  test("tapir") {
    // coursier refuses to resolve it: jackson-module-guice 2.14.3 wants guice [4.0,5.0), inject-core 24.2.0 wants guice-assistedinject 5.1.0 and with it
    // guice 5.1.0. sbt picks guice 5.1.0 anyway
    val finatraServer = model.ProjectName("tapir-finatra-server")
    testIn(
      "tapir",
      "https://github.com/softwaremill/tapir.git",
      "918a074",
      xmx = "12g",
      filtering = sbtimport.ImportFiltering.empty.copy(excludeProjects = Set(finatraServer))
    )
  }

  test("doobie") {
    testIn("doobie", "https://github.com/tpolecat/doobie.git", "e3adc5d")
  }

  test("http4s") {
    testIn("http4s", "https://github.com/http4s/http4s.git", "f4419aa")
  }

  test("bloop") {
    // depends on `compilation`, a project of another sbt build (`ProjectRef(BenchmarkBridgeProject.build, "compilation")`), which an import cannot have
    val benchmarks = model.ProjectName("benchmarks")
    testIn("bloop", "https://github.com/scalacenter/bloop.git", "051ce0a", filtering = sbtimport.ImportFiltering.empty.copy(excludeProjects = Set(benchmarks)))
  }

  test("sbt") {
    testIn(
      "sbt",
      "https://github.com/sbt/sbt.git",
      "0045c34",
      jvm = jvm8,
      postCheckoutCallback = { sbtBuildDir =>
        // Fix Scala 3 wildcard import syntax issue
        val sonaFile = sbtBuildDir / "main-actions" / "src" / "main" / "scala" / "sbt" / "internal" / "sona" / "Sona.scala"
        if (Files.exists(sonaFile)) {
          val content = Files.readString(sonaFile)
          val fixedContent = content.replace(
            "import sbt.internal.sona.codec.JsonProtocol.{ *, given }",
            "import sbt.internal.sona.codec.JsonProtocol.*"
          )
          Files.writeString(sonaFile, fixedContent)
        }

        // Also fix build.sbt to prevent OOM during compilation
        val buildSbt = sbtBuildDir / "build.sbt"
        if (Files.exists(buildSbt)) {
          Files.writeString(
            buildSbt,
            Files.readString(buildSbt).linesIterator.filterNot(_.contains("scalafmtOnCompile := ")).mkString("\n")
          )
        }
      }
    )
  }

  test("scalameta") {
    testIn("scalameta", "https://github.com/scalameta/scalameta.git", "0e19b94", jvm = jvm8, xmx = "12g")
  }

  // a large build of java and scala modules
  test("pekko") {
    testIn("pekko", "https://github.com/apache/pekko.git", "c2bc0f1cc1", xmx = "12g")
  }

  // cross built for jvm, js and native with sbt-crossproject, with directories of its own for scala versions
  test("zio") {
    // both pin zio-json below what their other dependencies want with `dependencyOverrides`, which bleep has no way to say, and sbt's export doesn't show
    val overridingDependencies = Set(model.ProjectName("zio-docs"), model.ProjectName("docs_make_zio_app_configurable"))
    testIn(
      "zio",
      "https://github.com/zio/zio.git",
      "18e9a9185b2",
      xmx = "12g",
      filtering = sbtimport.ImportFiltering.empty.copy(excludeProjects = overridingDependencies)
    )
  }

  // java and scala apis side by side, and an sbt plugin. the last revision before it cross built its sbt projects for sbt 2, which bleep does not support
  test("playframework") {
    testIn("playframework", "https://github.com/playframework/playframework.git", "193d60338", xmx = "12g")
  }

  test("converter") {
    testIn("converter", "https://github.com/ScalablyTyped/Converter.git", "37a3db3", jvm = jvm8)
  }

  def testIn(
      project: String,
      repo: String,
      sha: String,
      jvm: model.Jvm = model.Jvm.graalvm,
      xmx: String = "4g",
      filtering: sbtimport.ImportFiltering = sbtimport.ImportFiltering.empty,
      postCheckoutCallback: Path => Unit = _ => ()
  ): Assertion = {
    val logger = logger0.withPath(project)
    val testFolder = outFolder / project
    val sbtBuildDir = testFolder / "sbt-build"
    val inputDataPath = testFolder / "input.json.gz"
    val importedPath = testFolder / "imported"
    val bootstrappedPath = testFolder / "bootstrapped"

    val importerOptions = sbtimport.ImportOptions(
      ignoreWhenInferringTemplates = Set.empty,
      skipSbt = false,
      skipGeneratedResourcesScript = false,
      jvm = jvm,
      sbtPath = None,
      xmx = Some(xmx),
      buildJvm = None,
      filtering = filtering
    )

    val inputData: sbtimport.ImportInputData =
      if (!Files.exists(inputDataPath)) {
        val cliOut = cli.Out.ViaLogger(logger)
        if (!Files.exists(sbtBuildDir)) {
          Files.createDirectories(testFolder)
          cli(action = "git clone", cwd = testFolder, cmd = List("git", "clone", repo, sbtBuildDir.getFileName.toString), logger = logger, out = cliOut)
            .discard()
        } else {
          cli(action = "git fetch", cwd = sbtBuildDir, cmd = List("git", "fetch"), logger = logger, out = cliOut).discard()
        }
        cli(action = "git reset", cwd = sbtBuildDir, cmd = List("git", "reset", "--hard", sha), logger = logger, out = cliOut).discard()
        cli(action = "git submodule init", cwd = sbtBuildDir, cmd = List("git", "submodule", "init"), logger = logger, out = cliOut).discard()
        cli(action = "git submodule update", sbtBuildDir, List("git", "submodule", "update"), logger = logger, out = cliOut).discard()

        // Apply any post-checkout fixes
        postCheckoutCallback(sbtBuildDir)

        val sbtBuildLoader = BuildLoader.inDirectory(sbtBuildDir)
        val sbtDestinationPaths = BuildPaths(cwd = FileUtils.TempDir, sbtBuildLoader, model.BuildVariant.Normal)
        val cacheLogger = new BleepCacheLogger(logger)
        // unpacking a jdk logs through slf4j
        bleep.internal.Slf4jBridge.install(logger)
        val fetchJvm = new FetchJvm(Some(userPaths.resolveJvmCacheDir), cacheLogger, ExecutionContext.global)
        val fetchedJvm = fetchJvm(jvm)
        sbtimport.runSbt(logger, sbtBuildDir, sbtDestinationPaths, fetchedJvm, None, Some(xmx), importerOptions.filtering)

        val inputData = sbtimport.ImportInputData.collectFromFileSystem(sbtDestinationPaths, logger)
        FileUtils.writeGzippedBytes(
          inputDataPath,
          inputData
            // remove machine-specific paths inside bloop files files
            .replace(inputPaths.templatize, rewriteGeneratedFiles = true)
            .asJson
            .spaces2
            .getBytes(StandardCharsets.UTF_8)
        )
        inputData
      } else {
        val jsonText = new String(FileUtils.readGzippedBytes(inputDataPath), StandardCharsets.UTF_8)
        io.circe.parser.parse(jsonText).flatMap(json => fillPlaceholders(json).as[sbtimport.ImportInputData]) match {
          case Left(circeError) => throw new BleepException.InvalidJson(inputDataPath, circeError)
          case Right(inputData) => inputData
        }
      }

    val importedBuildLoader = BuildLoader.inDirectory(importedPath)
    val importedDestinationPaths = BuildPaths(cwd = FileUtils.TempDir, importedBuildLoader, model.BuildVariant.Normal)

    // generate a build file and store it
    val buildFiles: Map[Path, String] =
      sbtimport.generateBuild(
        sbtBuildDir,
        importedDestinationPaths,
        logger,
        importerOptions,
        model.BleepVersion.dev,
        inputData,
        bleepTasksVersion = model.BleepVersion("1.0.0-M14"),
        maybeExistingBuildFile = None
      )

    writeAndCompare(
      importedDestinationPaths.buildDir,
      buildFiles.map { case (p, s) => (p, inputPaths.templatize.string(s)) },
      logger
    ).discard()

    val bootstrappedDestinationPaths = BuildPaths(cwd = FileUtils.TempDir, BuildLoader.inDirectory(bootstrappedPath), model.BuildVariant.Normal)
    val existingImportedBuildLoader = BuildLoader.Existing(importedBuildLoader.bleepYaml)

    TestResolver.withFactory(isCi, testFolder, absolutePaths) { testResolver =>
      val ec = ExecutionContext.global
      val pre = Prebootstrapped(logger, userPaths, bootstrappedDestinationPaths, existingImportedBuildLoader, ec)
      val started = bootstrap.from(pre, ResolveProjects.InMemory, rewrites = Nil, model.BleepConfig.default, testResolver).orThrow

      // will produce templated bloop files we use to overwrite the bloop files already written by bootstrap
      val generatedBloopFiles: Map[Path, String] =
        GenBloopFiles.encodedFiles(GenBloopFiles.defaultBloopFilePath(bootstrappedDestinationPaths), started.resolvedProjects)

      // what template inference achieved, and what the import lost or changed compared to sbt's own bloop export. checked in, so every change is reviewed
      val roundtrip = ImportRoundtrip.report(
        inputData,
        started.resolvedProjects.collect { case (crossName, lazyResolved) if crossName.name.value != "scripts" => (crossName, lazyResolved.forceGet) },
        started.buildPaths.buildDir
      )
      val report = List("# template inference", TemplateStats.render(buildFiles(importedDestinationPaths.bleepYamlFile)), "", "# roundtrip", roundtrip)
      val reportPath = testFolder / "import-report.txt"
      writeAndCompare(reportPath, Map(reportPath -> absolutePaths.templatize.string(report.mkString("", "\n", "\n"))), logger).discard()

      // flush templated bloop files to disk if local, compare to checked in if test is running in CI
      // note, keep last. locally it "succeeds" with a `pending`
      writeAndCompare(
        bootstrappedPath,
        generatedBloopFiles.map { case (p, s) => (p, absolutePaths.templatize.string(s)) },
        logger
      )
    }
  }
}
