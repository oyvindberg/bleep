package bleep

import bleep.rewrites.{keepSelectedProjects, BuildRewrite}
import bleep.testing.SnapshotTest

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

class RewriteSnapshotTest extends SnapshotTest {
  // todo: yes, this means that this test has a dependency upon the other tests. find a better way
  val inFolder = Path.of("snapshot-tests").toAbsolutePath
  override val outFolder: Path = Path.of("snapshot-tests-rewrites").toAbsolutePath

  object DropFeatureFlag extends BuildRewrite {
    override val name = model.BuildRewriteName("drop-feature-flag")

    override protected def newExplodedProjects(oldBuild: model.Build, buildPaths: BuildPaths): Map[model.CrossProjectName, model.Project] =
      oldBuild.explodedProjects.map { case (name, p) =>
        val newP = p.copy(scala = p.scala.map { s =>
          s.copy(options = s.options.copy(values = s.options.values - model.Options.Opt.Flag("-feature")))
        })
        (name, newP)
      }
  }

  val rewrites: List[BuildRewrite] = List(
    DropFeatureFlag,
    keepSelectedProjects(List("jvm213"))
  )

  private val foundBuilds: List[BuildLoader.Existing] =
    Files
      .list(inFolder)
      .filter(Files.isDirectory(_))
      .map(testFolder => BuildLoader.inDirectory(testFolder / "imported"))
      .iterator()
      .asScala
      .collect { case x: BuildLoader.Existing => x }
      .toList

  def mkTest(rewrite: BuildRewrite, existingBuild: BuildLoader.Existing) = {
    val buildName = existingBuild.bleepYaml.getParent.getParent.getFileName.toString

    val testName = s"rewrite ${rewrite.name} for $buildName"
    test(testName) {
      val logger = logger0.withPath(testName)
      val build = model.Build.FileBacked(existingBuild.buildFile.forceGet.orThrow)
      val rewritten = rewrite(build, null)
      val destinationPath = outFolder / rewrite.name.value / buildName / s"bleep.yaml"
      val rewrittenYamlString = yaml.encodeShortened(rewritten.file)
      writeAndCompare(destinationPath, Map(destinationPath -> rewrittenYamlString), logger)
    }
  }

  for {
    rewrite <- rewrites
    existingBuild <- foundBuilds
  } mkTest(rewrite, existingBuild)
}
