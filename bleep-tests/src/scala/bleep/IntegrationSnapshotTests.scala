package bleep

import bleep.internal.FileUtils
import bleep.testing.SnapshotTest
import bloop.config.Config
import io.circe.parser.decode
import io.circe.syntax.EncoderOps
import org.scalatest.Assertion

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.concurrent.ExecutionContext
import scala.jdk.CollectionConverters.IteratorHasAsScala

class IntegrationSnapshotTests extends SnapshotTest {
  absolutePaths.sortedValues.foreach(println)
  val userPaths = UserPaths.fromAppDirs

  test("tapir") {
    testIn("tapir", "https://github.com/softwaremill/tapir.git", "9057697")
  }

  test("doobie") {
    testIn("doobie", "https://github.com/tpolecat/doobie.git", "5d0957d", jvm = model.Jvm("zulu-jre:16.0.0", None))
  }

  test("http4s") {
    testIn("http4s", "https://github.com/http4s/http4s.git", "bc06627")
  }

  test("bloop") {
    testIn("bloop", "https://github.com/scalacenter/bloop.git", "cc04a06")
  }

  test("sbt") {
    testIn("sbt", "https://github.com/sbt/sbt.git", "1f29c90")
  }

  test("scalameta") {
    testIn("scalameta", "https://github.com/scalameta/scalameta.git", "e2cba51")
  }

  test("converter") {
    testIn("converter", "https://github.com/ScalablyTyped/Converter.git", "3a4686a")
  }

  def testIn(project: String, repo: String, sha: String, jvm: model.Jvm = model.Jvm.graalvm): Assertion = {
    val logger = logger0.withPath(project)
    val testFolder = outFolder / project
    val sbtBuildDir = testFolder / "sbt-build"
    val inputDataPath = testFolder / "input.json.gz"
    val importedPath = testFolder / "imported"
    val bootstrappedPath = testFolder / "bootstrapped"

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
        // fix OOM for sbt build
        Files.writeString(
          sbtBuildDir / "build.sbt",
          Files.readString(sbtBuildDir / "build.sbt").split("\n").filterNot(_.contains("scalafmtOnCompile := ")).mkString("\n")
        )
        cli(action = "git submodule init", cwd = sbtBuildDir, cmd = List("git", "submodule", "init"), logger = logger, out = cliOut).discard()
        cli(action = "git submodule update", sbtBuildDir, List("git", "submodule", "update"), logger = logger, out = cliOut).discard()

        val sbtBuildLoader = BuildLoader.inDirectory(sbtBuildDir)
        val sbtDestinationPaths = BuildPaths(cwd = FileUtils.TempDir, sbtBuildLoader, model.BuildVariant.Normal)
        val cacheLogger = new BleepCacheLogger(logger)
        val fetchJvm = new FetchJvm(Some(userPaths.resolveJvmCacheDir), cacheLogger, ExecutionContext.global)
        val fetchedJvm = fetchJvm(jvm)
        sbtimport.runSbt(logger, sbtBuildDir, sbtDestinationPaths, fetchedJvm, None)

        val inputData = sbtimport.ImportInputData.collectFromFileSystem(sbtDestinationPaths, logger)
        FileUtils.writeGzippedBytes(
          inputDataPath,
          inputData
            // remove machine-specific paths inside bloop files files
            .replace(absolutePaths.templatize, rewriteGeneratedFiles = true)
            .asJson
            .spaces2
            .getBytes(StandardCharsets.UTF_8)
        )
        inputData
      } else {
        decode[sbtimport.ImportInputData](new String(FileUtils.readGzippedBytes(inputDataPath), StandardCharsets.UTF_8)) match {
          case Left(circeError) => throw new BleepException.InvalidJson(inputDataPath, circeError)
          case Right(inputData) => inputData.replace(absolutePaths.fill, rewriteGeneratedFiles = false)
        }
      }

    val importedBuildLoader = BuildLoader.inDirectory(importedPath)
    val importedDestinationPaths = BuildPaths(cwd = FileUtils.TempDir, importedBuildLoader, model.BuildVariant.Normal)
    val importerOptions =
      sbtimport.ImportOptions(ignoreWhenInferringTemplates = Set.empty, skipSbt = false, skipGeneratedResourcesScript = false, jvm = jvm, sbtPath = None)

    // generate a build file and store it
    val buildFiles: Map[Path, String] =
      sbtimport.generateBuild(
        sbtBuildDir,
        importedDestinationPaths,
        logger,
        importerOptions,
        model.BleepVersion.dev,
        inputData,
        bleepTasksVersion = model.BleepVersion("0.0.1-M20"),
        maybeExistingBuildFile = None
      )

    writeAndCompare(
      importedDestinationPaths.buildDir,
      buildFiles.map { case (p, s) => (p, absolutePaths.templatize.string(s)) },
      logger
    ).discard()

    val bootstrappedDestinationPaths = BuildPaths(cwd = FileUtils.TempDir, BuildLoader.inDirectory(bootstrappedPath), model.BuildVariant.Normal)
    val existingImportedBuildLoader = BuildLoader.Existing(importedBuildLoader.bleepYaml)

    TestResolver.withFactory(isCi, testFolder, absolutePaths) { testResolver =>
      val ec = ExecutionContext.global
      val pre = Prebootstrapped(logger, userPaths, bootstrappedDestinationPaths, existingImportedBuildLoader, ec)
      val started = bootstrap.from(pre, GenBloopFiles.InMemory, rewrites = Nil, model.BleepConfig.default, testResolver).orThrow

      // will produce templated bloop files we use to overwrite the bloop files already written by bootstrap
      val generatedBloopFiles: Map[Path, String] =
        GenBloopFiles.encodedFiles(bootstrappedDestinationPaths, started.bloopFiles)

      // further property checks to see that we haven't made any illegal rewrites
      assertSameIshBloopFiles(inputData, started).discard()

      // flush templated bloop files to disk if local, compare to checked in if test is running in CI
      // note, keep last. locally it "succeeds" with a `pending`
      writeAndCompare(
        bootstrappedPath,
        generatedBloopFiles.map { case (p, s) => (p, absolutePaths.templatize.string(s)) },
        logger
      )
    }
  }

  // compare some key properties before and after import
  def assertSameIshBloopFiles(inputProjects: sbtimport.ImportInputData, started: Started): Assertion = {
    started.bloopFiles.foreach {
      case (crossProjectName, _) if crossProjectName.value == "scripts" => ()
      case (crossProjectName, lazyOutputFile) =>
        val output = lazyOutputFile.forceGet.project
        val input = inputProjects.projects(crossProjectName).bloopFile.project

        // todo: this needs further work,
        //      assert(
        //        output.platform == input.platform,
        //        crossProjectName.value
        //      )

        // scalacOptions are the same, modulo ordering, duplicates, target directory and semanticdb
        def patchedOptions(project: Config.Project, targetDir: Path): (Int, List[String]) = {
          val replacements = model.Replacements.targetDir(targetDir) ++
            model.Replacements.ofReplacements(List(("sbt-build", "bootstrapped")))
          val original = project.scala.map(_.options).getOrElse(Nil)
          val all = model.Options.parse(original, Some(replacements)).values.toList.sorted.flatMap {
            case opt if opt.toString.contains("semanticdb") => Nil
            case opt                                        => opt.render
          }
          // compiler plugins are also resolved by ivy in sbt, so paths are completely different
          val (compilerPlugins, rest) = all.partition(_.startsWith(constants.ScalaPluginPrefix))
          (compilerPlugins.length, rest)
        }

        val originalTargetDir = sbtimport.findOriginalTargetDir.force(crossProjectName, input)
        assert(
          patchedOptions(output, output.out) == patchedOptions(input, originalTargetDir),
          crossProjectName.value
        ).discard()

        // assert that all source folders are conserved. currently bleep may add some. also we drop folders for generated stuff
        val target = Path.of("target")
        val lostSources = input.sources
          .map(_.normalize())
          .filterNot(_.startsWith(originalTargetDir))
          .filterNot(output.sources.contains)
          .filter(_.isAbsolute)
          .sorted
        assert(lostSources.isEmpty, crossProjectName.value).discard()

        // assert that all resource folders are conserved. currently bleep may add some
        val lostResources = input.resources
          .getOrElse(Nil)
          .filterNot(_.iterator().asScala.contains(target))
          .filterNot(output.resources.getOrElse(Nil).contains)
          .filter(_.isAbsolute)
          .sorted
        assert(lostResources.isEmpty, crossProjectName.value).discard()

        /** @param classesDirs
          *   classes directories are completely different in bleep. we may also drop projects, so no comparisons are done for these, unfortunately.
          *
          * For instance:
          *   - ~/bleep/snapshot-tests/converter/.bleep/.bloop/phases/classes
          *   - ~/bleep/snapshot-tests/converter/.bleep/import/bloop/2.12/phases/scala-2.12/classes,
          *
          * @param scalaJars
          *   paths expected to be completely different because in sbt they may be resolved by launcher/ivy
          *
          * For instance:
          *   - ~/.sbt/boot/scala-2.12.14/lib/scala-reflect.jar
          *   - ~/.cache/coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-reflect/2.12.2/scala-reflect-2.12.2.jar,
          *
          * @param restJars
          *   the remaining set of differing jars should be empty
          */
        case class AnalyzedClassPathDiff(classesDirs: Set[Path], scalaJars: Set[Path], restJars: Set[Path])
        object AnalyzedClassPathDiff {
          val transitiveResources: Set[Path] =
            started.build.transitiveDependenciesFor(crossProjectName).flatMap { case (name, _) => started.bloopProject(name).resources }.flatten.toSet

          def from(paths: Set[Path]): AnalyzedClassPathDiff = {
            val (classes, jars) = paths
              // the bloop build has this added for all projects. no idea where it comes from and what to do with it
              .filterNot(_.getFileName.toString == "tools.jar")
              // drop classes and resources directories
              .partition(p => p.endsWith("classes") || p.endsWith("test-classes") || p.endsWith("it-classes") || transitiveResources(p))
            // note that paths are difficult here, we may receive files from sbt launcher in boot folder
            val (scalaJars, restJars) = jars.partition(p => p.getFileName.toString.startsWith("scala"))
            AnalyzedClassPathDiff(classes, scalaJars, restJars)
          }
        }

        val added = AnalyzedClassPathDiff.from(output.classpath.toSet -- input.classpath)
        val removed = AnalyzedClassPathDiff.from(input.classpath.toSet -- output.classpath)

        def render(paths: Iterable[Path]): String =
          if (paths.isEmpty) "nothing"
          else paths.mkString("\n", ",\n", "\n")

        if (added.scalaJars.size != removed.scalaJars.size) {
          System.err.println {
            List(
              crossProjectName.value,
              ": Expected there to be equal number of scala jars. added :",
              render(added.scalaJars),
              ", removed: ",
              render(removed.scalaJars)
            ).mkString("")
          }
        }
        if (added.restJars.nonEmpty || removed.restJars.nonEmpty) {
          started.logger.warn(s"${crossProjectName.value}: Added ${render(added.restJars)} to classPath, Removed ${render(removed.restJars)} from classPath")
        }
    }
    succeed
  }
}
