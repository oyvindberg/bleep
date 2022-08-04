package bleep

import bleep.commands.Import
import bleep.commands.Import.Options
import bleep.internal.{findGeneratedFiles, FileUtils, GeneratedFile, ImportInputProjects, ReadSbtExportFile}
import bleep.testing.SnapshotTest
import bloop.config.Config
import coursier.paths.CoursierPaths
import io.circe.parser.decode
import io.circe.syntax.EncoderOps
import org.scalatest.Assertion

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.IteratorHasAsScala

class IntegrationSnapshotTests extends SnapshotTest {
  val logger = logging.stdout(LogPatterns.logFile).untyped
  val inFolder = Paths.get("snapshot-tests-in").toAbsolutePath
  object testResolver extends CoursierResolver.Factory {
    override def apply(pre: Prebootstrapped, config: BleepConfig, build: model.Build): CoursierResolver = {
      val cachePath =
        if (enforceUpToDate) CoursierPaths.cacheDirectory().toPath / "sneaky-bleep-cache"
        else pre.userPaths.coursierCacheDir

      CoursierResolver(
        repos = build.resolvers.values,
        logger = logger,
        downloadSources = false,
        cacheIn = cachePath,
        authentications = None,
        wantedBleepVersion = None
      )
    }
  }

  test("tapir") {
    testIn("tapir")
  }

  test("doobie") {
    testIn("doobie")
  }

  test("http4s") {
    testIn("http4s")
  }

  test("bloop") {
    // todo: cache result of looking up existing sources in a json file as well
    cli(List("git", "submodule", "init"), logger, "git submodule init")(inFolder / "bloop")
    cli(List("git", "submodule", "update"), logger, "git submodule update")(inFolder / "bloop")
    testIn("bloop")
  }

  test("sbt") {
    testIn("sbt")
  }

  test("scalameta") {
    testIn("scalameta")
  }

  test("converter") {
    testIn("converter")
  }

  def testIn(project: String): Assertion = {
    val sbtBuildDir = inFolder / project
    val buildLoader = BuildLoader.inDirectory(outFolder / project)
    val destinationPaths = BuildPaths(cwd = FileUtils.TempDir, buildLoader, BuildPaths.Mode.Normal)
    val importerOptions = Options(ignoreWhenInferringTemplates = Set.empty, skipSbt = false, skipGeneratedResourcesScript = false)
    val importer = commands.Import(existingBuild = None, sbtBuildDir, destinationPaths, logger, importerOptions, model.BleepVersion.dev)

    // if this directory exists, assume it has all files in good condition, but with paths not filled in
    if (!Files.exists(destinationPaths.bleepImportDir)) {
      // if not, generate all bloop and dependency files
      importer.generateBloopAndDependencyFiles()

      // remove machine-specific paths inside bloop files files
      Import.findGeneratedJsonFiles(destinationPaths.bleepImportBloopDir).foreach { bloopFilePath =>
        val contents = Files.readString(bloopFilePath)
        val templatedContents = absolutePaths.templatize.string(contents)
        FileUtils.writeString(bloopFilePath, templatedContents)
      }
    }

    val sbtExportFiles = Import.findGeneratedJsonFiles(destinationPaths.bleepImportSbtExportDir).map { path =>
      val contents = Files.readString(path)
      (path, contents, ReadSbtExportFile.parse(path, contents))
    }

    val importedBloopFiles: Iterable[(Path, String, Config.File)] =
      Import
        .findGeneratedJsonFiles(destinationPaths.bleepImportBloopDir)
        .map { bloopFilePath =>
          val originalContents = Files.readString(bloopFilePath)
          val importedBloopFile = {
            val templatedContents = absolutePaths.fill.string(originalContents)
            GenBloopFiles.parseBloopFile(templatedContents)
          }
          (bloopFilePath, originalContents, importedBloopFile)
        }

    // cache contents of all generated files in a json file which is checked in.
    // these files will be machine and time dependent, but it's fine. just thread the content through, and update when sbt imports are refreshed
    val generatedFilesJsonFile = destinationPaths.buildDir / "generated-files.json"
    val generatedFilesCached: Option[Map[model.CrossProjectName, Vector[GeneratedFile]]] = {
      val jsonFile = generatedFilesJsonFile

      if (FileUtils.exists(jsonFile))
        decode[Map[model.CrossProjectName, Vector[GeneratedFile]]](Files.readString(jsonFile)) match {
          case Left(th)     => throw th
          case Right(value) => Some(value)
        }
      else None
    }

    val inputProjects = ImportInputProjects(
      importedBloopFiles.map { case (_, _, file) => file },
      sbtExportFiles.map { case (_, _, sbtExportFile) => sbtExportFile },
      forceInclude = generatedFilesCached match {
        case Some(map) => map.keySet
        case None      => Set.empty
      }
    )
    val generatedFiles = generatedFilesCached.getOrElse {
      findGeneratedFiles(inputProjects)
    }

    // generate a build file and store it
    val buildFiles: Map[Path, String] =
      importer.generateBuild(inputProjects, bleepTasksVersion = model.BleepVersion("0.0.1-M14"), generatedFiles, maybeExistingBleepJson = None)

    // write build files, and produce an (in-memory) exploded build plus new bloop files
    writeAndCompareEarly(destinationPaths.buildDir, buildFiles)

    val started = bootstrap.from(
      Prebootstrapped(destinationPaths, logger, BuildLoader.Existing(buildLoader.bleepYaml)),
      GenBloopFiles.InMemory,
      rewrites = Nil,
      Lazy(BleepConfig.default),
      testResolver
    ) match {
      case Left(th)       => throw th
      case Right(started) => started
    }

    // will produce templated bloop files we use to overwrite the bloop files already written by bootstrap
    val generatedBloopFiles: Map[Path, String] =
      GenBloopFiles.encodedFiles(destinationPaths, started.bloopFiles).map { case (p, s) => (p, absolutePaths.templatize.string(s)) }

    val allFiles: Map[Path, String] =
      buildFiles ++
        generatedBloopFiles ++
        importedBloopFiles.map { case (p, s, _) => (p, s) } ++
        sbtExportFiles.map { case (p, s, _) => (p, s) } ++ Map(generatedFilesJsonFile -> generatedFiles.asJson.noSpaces)

    // further property checks to see that we haven't made any illegal rewrites
    assertSameIshBloopFiles(inputProjects, started)

    // flush templated bloop files to disk if local, compare to checked in if test is running in CI
    // note, keep last. locally it "succeeds" with a `pending`
    writeAndCompare(destinationPaths.buildDir, allFiles)
  }

  // compare some key properties before and after import
  def assertSameIshBloopFiles(inputProjects: ImportInputProjects, started: Started): Assertion = {
    started.bloopProjects.foreach {
      case (crossProjectName, _) if crossProjectName.value == "scripts" => ()
      case (crossProjectName, output) =>
        val input = inputProjects.values(crossProjectName).bloopFile.project

        // todo: this needs further work,
        //      assert(
        //        output.platform == input.platform,
        //        crossProjectName.value
        //      )

        // scalacOptions are the same, modulo ordering, duplicates, target directory and semanticdb
        def patchedOptions(project: Config.Project, targetDir: Path): List[String] = {
          val replacements = model.Replacements.targetDir(targetDir) ++
            model.Replacements.ofReplacements(List(("snapshot-tests-in", "snapshot-tests")))
          val original = project.scala.map(_.options).getOrElse(Nil)
          model.Options.parse(original, Some(replacements)).values.toList.sorted.flatMap {
            case opt if opt.toString.contains("semanticdb") => Nil
            case opt                                        => opt.render
          }
        }

        val originalTargetDir = internal.findOriginalTargetDir.force(crossProjectName, input)
        assert(
          patchedOptions(output, output.out) == patchedOptions(input, originalTargetDir),
          crossProjectName.value
        )

        // assert that all source folders are conserved. currently bleep may add some. also we drop folders for generated stuff
        val target = Path.of("target")
        val lostSources = input.sources
          .map(_.normalize())
          .filterNot(_.startsWith(originalTargetDir))
          .filterNot(output.sources.contains)
          .filter(_.isAbsolute)
          .sorted
        assert(lostSources.isEmpty, crossProjectName.value)

        // assert that all resource folders are conserved. currently bleep may add some
        val lostResources = input.resources
          .getOrElse(Nil)
          .filterNot(_.iterator().asScala.contains(target))
          .filterNot(output.resources.getOrElse(Nil).contains)
          .filter(_.isAbsolute)
          .sorted
        assert(lostResources.isEmpty, crossProjectName.value)

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
          def from(paths: Set[Path]): AnalyzedClassPathDiff = {
            val (classes, jars) = paths
              // the bloop build has this added for all projects. no idea where it comes from and what to do with it
              .filterNot(_.getFileName.toString == "tools.jar")
              .partition(p => p.endsWith("classes") || p.endsWith("test-classes") || p.endsWith("it-classes"))
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
          System.err.println(s"${crossProjectName.value}: Added ${render(added.restJars)} to classPath, Removed ${render(removed.restJars)} from classPath")
        }
    }
    succeed
  }
}
