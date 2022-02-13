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
  val logger = logging.stdout(LogPatterns.logFile)
  val inFolder = Paths.get("snapshot-tests-in").toAbsolutePath
  val outFolder = Paths.get("snapshot-tests").toAbsolutePath
  val resolver: CoursierResolver =
    if (isCi) {
      // the github action we use in CI caches coursier directory. let's piggyback on that for now
      val cachePath = Some(CoursierPaths.cacheDirectory().toPath / "sneaky-bleep-cache")
      CoursierResolver(logger, downloadSources = false, cacheIn = cachePath, Authentications.empty)
    } else {
      // if not, might as well use normal cache location
      val directories = UserPaths.fromAppDirs
      CoursierResolver(logger, downloadSources = false, cacheIn = Some(directories.cacheDir), Authentications.empty)
    }

  val absolutePaths: Replacements =
    Replacements.ofReplacements(
      List(
        (CoursierPaths.cacheDirectory().toString, "<COURSIER>"),
        (System.getProperty("user.dir"), "<PROJECT>"),
        (System.getProperty("user.home"), "<HOME>")
      )
    )

  case class TestPaths(project: String) extends BuildPaths {
    override val buildDir: Path = inFolder / project

    val outTarget: Path = outFolder / project

    override val bleepImportDir: Path = outTarget / "import"

    override val dotBleepDir: Path = outTarget / "generated"
    override val bleepJsonFile: Path = dotBleepDir / Defaults.BuildFileName
    override val bleepBloopDir: Path = dotBleepDir / ".bloop"
    override val digestFile: Path = dotBleepDir / "digest"
    override val bspBleepJsonFile: Path = dotBleepDir / "bsp-bleep.json"
  }

  test("tapir") {
    testIn(TestPaths("tapir"))
  }

  test("doobie") {
    testIn(TestPaths("doobie"))
  }

  def testIn(paths: TestPaths): Assertion = {
    val importer = commands.Import(paths, logger, Options(ignoreWhenInferringTemplates = Set.empty, skipSbt = false))

    // if this directory exists, assume it has all files in good condition, but with paths not filled in
    if (!Files.exists(paths.bleepImportDir)) {
      // if not, generate all bloop files
      importer.generateBloopFiles()

      // and remove paths inside those files
      importer.findGeneratedBloopFiles().map { bloopFilePath =>
        val contents = Files.readString(bloopFilePath)
        val templatedContents = absolutePaths.templatize.string(contents)
        Files.writeString(bloopFilePath, templatedContents)
      }
    }

    val importedBloopFiles: Iterable[Config.File] =
      importer.findGeneratedBloopFiles().map { bloopFilePath =>
        val importedBloopFiles = {
          val contents = Files.readString(bloopFilePath)
          val templatedContents = absolutePaths.fill.string(contents)
          parseBloopFile(templatedContents)
        }

        val preResolvedModules: List[Config.Module] = {
          val fromResolution = importedBloopFiles.project.resolution.fold(List.empty[Config.Module])(_.modules)
          val fromCompilerPlugins = importedBloopFiles.project.scala.toList.flatMap(_.options).collect { case CompilerPlugin(dep) => dep }

          fromResolution ++ fromCompilerPlugins
        }

        // if not all artifacts exist locally then resolve everything.
        // This is a common case, since we generate bloop files on one computer and then share them
        if (!preResolvedModules.flatMap(_.artifacts.map(_.path)).forall(FileUtils.exists)) {
          val modulesAsDeps = preResolvedModules.map { case Config.Module(org, name, version, maybeConfig, _) =>
            Dependency(Module(Organization(org), ModuleName(name), Map.empty), version)
              .withConfiguration(maybeConfig match {
                case Some(value) => Configuration(value)
                case None        => Configuration.empty
              })
          }

          logger.warn(s"resolving dependencies for ${importedBloopFiles.project.name}")

          resolver(JsonSet.fromIterable(modulesAsDeps), JsonSet.empty) match {
            case Left(coursierError) => throw coursierError
            case Right(_)            => ()
          }
        }

        importedBloopFiles
      }

    // generate a build file and store it
    importer.generateBuildAndPersistFrom(importedBloopFiles)

    // read that build file, and produce an (in-memory) exploded build plus new bloop files
    val started = bootstrap.unsafeFrom(logger, cwd = paths.buildDir, buildPaths = paths)

    // will produce templated bloop files we use to overwrite the bloop files already written by bootstrap
    val generatedBloopFiles: Map[RelPath, String] =
      bootstrap
        .bloopFileMap(started.bloopFiles)
        .map { case (f, s) => (f, absolutePaths.templatize.string(s)) }
        .updated(RelPath.relativeTo(paths.dotBleepDir, paths.bleepJsonFile), Files.readString(paths.bleepJsonFile))

    // further property checks to see that we haven't made any illegal rewrites
    assertSameIshBloopFiles(importedBloopFiles, started)

    // flush templated bloop files to disk if local, compare to checked in if test is running in CI
    // note, keep last. locally it "succeeds" with a `pending`
    writeAndCompare(paths.dotBleepDir, generatedBloopFiles)
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

      // scalacOptions are the same, module order ordering and duplicates
      assert(
        output.scala.map(s => s.options.sorted.distinct) == input.scala.map(s => s.options.sorted.distinct),
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
    def unapply(str: String): Option[Config.Module] =
      // -Xplugin:<COURSIER>/https/repo1.maven.org/maven2/org/typelevel/kind-projector_2.12.15/0.13.2/kind-projector_2.12.15-0.13.2.jar
      str.split(":") match {
        case Array("-Xplugin", path) =>
          val pathFragments = path.split("/").toList

          pathFragments.indexOf("maven2") match {
            case -1 => sys.error(s"tests only suppport compiler plugins from a repository which has a maven2/ path fragment. feel free to patch this")
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
