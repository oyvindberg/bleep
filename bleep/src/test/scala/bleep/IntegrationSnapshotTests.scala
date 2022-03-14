package bleep

import bleep.CoursierResolver.Authentications
import bleep.commands.Import.Options
import bleep.internal.generateBloopFiles.dependencyOrdering
import bleep.internal.{importBloopFilesFromSbt, FileUtils, Replacements}
import bloop.config.Config
import coursier.core._
import coursier.paths.CoursierPaths
import org.scalatest.Assertion

import java.nio.file.{Files, Path, Paths}

class IntegrationSnapshotTests extends SnapshotTest {
  val logger = logging.stdout(LogPatterns.logFile).untyped
  val inFolder = Paths.get("snapshot-tests-in").toAbsolutePath
  val outFolder = Paths.get("snapshot-tests").toAbsolutePath
  val resolver: CoursierResolver =
    if (isCi) {
      // the github action we use in CI caches coursier directory. let's piggyback on that for now
      val cachePath = Some(CoursierPaths.cacheDirectory().toPath / "sneaky-bleep-cache")
      CoursierResolver(Nil, logger, downloadSources = false, cacheIn = cachePath, Authentications.empty)
    } else {
      // if not, might as well use normal cache location
      val directories = UserPaths.fromAppDirs
      CoursierResolver(Nil, logger, downloadSources = false, cacheIn = Some(directories.cacheDir), Authentications.empty)
    }

  case class TestPaths(project: String) extends BuildPaths {
    override val cwd = Paths.get("/tmp")
    override val buildDir: Path = inFolder / project

    val outTarget: Path = outFolder / project

    override lazy val bleepImportDir: Path = outTarget / "import"
    override lazy val dotBleepDir: Path = outTarget / "generated"
    override lazy val dotBleepModeDir: Path = dotBleepDir
    override lazy val bleepJsonFile: Path = dotBleepDir / constants.BuildFileName
  }

  test("tapir") {
    testIn(TestPaths("tapir"))
  }

  test("doobie") {
    testIn(TestPaths("doobie"))
  }

  test("http4s") {
    testIn(TestPaths("http4s"))
  }

  test("converter") {
    testIn(TestPaths("converter"))
  }

  def testIn(paths: TestPaths): Assertion = {
    val importer = commands.Import(paths, logger, Options(ignoreWhenInferringTemplates = Set.empty, skipSbt = false))

    // if this directory exists, assume it has all files in good condition, but with paths not filled in
    if (!Files.exists(paths.bleepImportDir)) {
      // if not, generate all bloop files
      importer.generateBloopFiles()

      // and remove paths inside those files
      importer.findGeneratedBloopFiles().foreach { bloopFilePath =>
        val contents = Files.readString(bloopFilePath)
        val templatedContents = absolutePaths.templatize.string(contents)
        FileUtils.writeString(bloopFilePath, templatedContents)
      }
    }

    val importedBloopFiles: Iterable[Config.File] =
      importer.findGeneratedBloopFiles().map { bloopFilePath =>
        val importedBloopFile = {
          val contents = Files.readString(bloopFilePath)
          val templatedContents = absolutePaths.fill.string(contents)
          parseBloopFile(templatedContents)
        }

        val preResolvedModules: List[Config.Module] = {
          val fromResolution = importedBloopFile.project.resolution.fold(List.empty[Config.Module])(_.modules)
          val fromCompilerPlugins = importedBloopFile.project.scala.toList.flatMap(_.options).collect { case CompilerPlugin(dep) => dep }

          fromResolution ++ fromCompilerPlugins
        }

        // if not all artifacts exist locally then resolve everything.
        // This is a common case, since we generate bloop files on one computer and then share them
        if (!preResolvedModules.flatMap(_.artifacts.map(_.path)).forall(FileUtils.exists)) {
          val modulesAsDeps = preResolvedModules.map { case mod @ Config.Module(org, name, version, maybeConfig, _) =>
            val attrs: Map[String, String] = if (importBloopFilesFromSbt.checkIsSbtPlugin(mod)) Dep.SbtPluginAttrs else Map.empty

            Dependency(Module(Organization(org), ModuleName(name), attrs), version)
              .withConfiguration(maybeConfig match {
                case Some(value) => Configuration(value)
                case None        => Configuration.empty
              })
          }

          logger.warn(s"resolving dependencies for ${importedBloopFile.project.name}")

          resolver(JsonSet.fromIterable(modulesAsDeps)) match {
            case Left(coursierError) => throw coursierError
            case Right(_)            => ()
          }
        }

        importedBloopFile
      }

    // generate a build file and store it
    importer.generateBuildAndPersistFrom(importedBloopFiles)

    // read that build file, and produce an (in-memory) exploded build plus new bloop files
    val Right(started) = bootstrap.from(Prebootstrapped(paths, logger), rewrites = Nil)

    // will produce templated bloop files we use to overwrite the bloop files already written by bootstrap
    val generatedBloopFiles: Map[RelPath, String] =
      bootstrap.bloopFileMap(started.bloopFiles).map { case (f, s) => (f, absolutePaths.templatize.string(s)) }

    val generatedFiles: Map[RelPath, String] =
      generatedBloopFiles.updated(RelPath.relativeTo(paths.dotBleepDir, paths.bleepJsonFile), Files.readString(paths.bleepJsonFile))

    // further property checks to see that we haven't made any illegal rewrites
    assertSameIshBloopFiles(importedBloopFiles, started)

    // flush templated bloop files to disk if local, compare to checked in if test is running in CI
    // note, keep last. locally it "succeeds" with a `pending`
    writeAndCompare(paths.dotBleepDir, generatedFiles)
  }

  def assertSameIshBloopFiles(importedBloopFiles: Iterable[Config.File], started: Started): Assertion = {
    // compare some key properties before and after import
    val inputProjects: Map[(model.ProjectName, Option[String], Option[String]), Config.Project] =
      importedBloopFiles.map { case Config.File(_, p) =>
        ((importBloopFilesFromSbt.projectName(p.name), p.platform.map(_.name), p.scala.map(_.version)), p)
      }.toMap

    started.bloopFiles.foreach { case (crossProjectName, lazyFile) =>
      val output = lazyFile.forceGet.project
      val input = inputProjects((crossProjectName.name, output.platform.map(_.name), output.scala.map(_.version)))

      // todo: this needs further work,
      //      assert(
      //        output.platform == input.platform,
      //        crossProjectName.value
      //      )

      // scalacOptions are the same, modulo ordering, duplicates and target directory
      def patchedOptions(project: Config.Project, targetDir: Path): List[String] = {
        val replacements = Replacements.targetDir(targetDir)
        val original = project.scala.map(_.options).getOrElse(Nil)
        original.map(replacements.templatize.string).sorted.distinct
      }
      assert(
        patchedOptions(output, output.out) == patchedOptions(input, internal.findOriginalTargetDir(logger, input).get),
        crossProjectName.value
      )

      // assert that all source folders are conserved. currently bleep may add some
      assert(
        input.sources.sorted.forall(output.sources.contains),
        crossProjectName.value
      )
      // assert that all resource folders are conserved. currently bleep may add some
      assert(
        input.resources.getOrElse(Nil).sorted.forall(output.resources.getOrElse(Nil).contains),
        crossProjectName.value
      )

    // todo: this is hard to write a better test for. for instance:
    // - paths may be different from being resolved from coursier or ivy
    // - filename may also be different as sbt may resolve scala jars in ~/.sbt/boot, and there they won't have an embedded version number
    //      if (output.classpath.length != input.classpath.length)
    //        assert(
    //          output.classpath.sorted.mkString("\n") == input.classpath.sorted.mkString("\n"),
    //          crossProjectName.value
    //        )
    }
    succeed
  }

  object CompilerPlugin {
    def unapply(scalacOption: String): Option[Config.Module] =
      // -Xplugin:<COURSIER>/https/repo1.maven.org/maven2/org/typelevel/kind-projector_2.12.15/0.13.2/kind-projector_2.12.15-0.13.2.jar
      scalacOption.split(":") match {
        case Array(constants.ScalaPluginPrefix, path) =>
          val pathFragments = path.split("/").toList

          pathFragments.indexOf("maven2") match {
            case -1 => sys.error(s"tests only support compiler plugins from a repository which has a maven2/ path fragment. feel free to patch this")
            case n =>
              pathFragments.drop(n + 1).reverse match {
                case (fileName @ _) :: version :: artifactName :: reverseOrgs =>
                  Some(
                    Config.Module(
                      reverseOrgs.reverse.mkString("."),
                      artifactName,
                      version,
                      None,
                      List(Config.Artifact(artifactName, None, None, Paths.get(path)))
                    )
                  )
                case _ => None
              }
          }
        case _ => None
      }

  }
}
