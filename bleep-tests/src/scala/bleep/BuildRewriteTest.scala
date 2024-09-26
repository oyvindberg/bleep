package bleep

import bleep.rewrites.BuildRewrite
import org.scalactic.TripleEqualsSupport
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.Path

class BuildRewriteTest extends AnyFunSuite with TripleEqualsSupport {
  val prelude = """$schema: https://raw.githubusercontent.com/oyvindberg/bleep/master/schema.json
                 |$version: dev
                 |""".stripMargin
  def rewriteTest(testName: String, preYaml: String)(f: model.Build.FileBacked => model.Build.FileBacked)(expectedYaml: String): Unit =
    test(testName) {
      val existing = BuildLoader.Existing(Path.of("bleep.yaml"), Lazy(Right(prelude ++ preYaml)))
      val preBuild = model.Build.FileBacked(existing.buildFile.forceGet.orThrow)
      val postBuild = f(preBuild)
      val postYaml = yaml.encodeShortened(postBuild.file)
      assert(postYaml === (prelude ++ expectedYaml))
    }

  rewriteTest(
    "empty identity",
    """projects:
      |  a: {}
      |""".stripMargin
  )(identity)(
    """projects:
      |  a: {}
      |""".stripMargin
  )

  rewriteTest(
    "empty identity cross",
    """projects:
      |  a:
      |    cross:
      |      jvm212: {}
      |""".stripMargin
  )(identity)(
    """projects:
      |  a:
      |    cross:
      |      jvm212: {}
      |""".stripMargin
  )

  rewriteTest(
    "change value",
    """projects:
      |  a:
      |    folder: ./a
      |""".stripMargin
  ) { build =>
    val newProjects = build.explodedProjects.map { case (crossName, p) =>
      val newP = p.copy(folder = p.folder.map(f => RelPath.of(f.segments.map(_.replace('a', 'b'))*)))
      (crossName, newP)
    }
    BuildRewrite.withProjects(build, newProjects)
  }(
    """projects:
      |  a:
      |    folder: ./b
      |""".stripMargin
  )

  rewriteTest(
    "change value cross",
    """projects:
      |  a:
      |    cross:
      |      jvm212:
      |        folder: ./a
      |""".stripMargin
  ) { build =>
    val newProjects = build.explodedProjects.map { case (crossName, p) =>
      val newP = p.copy(folder = p.folder.map(f => RelPath.of(f.segments.map(_.replace('a', 'b'))*)))
      (crossName, newP)
    }
    BuildRewrite.withProjects(build, newProjects)
  }(
    """projects:
      |  a:
      |    cross:
      |      jvm212:
      |        folder: ./b
      |""".stripMargin
  )

  rewriteTest(
    "drop folder in a cross project",
    """projects:
      |  a:
      |    cross:
      |      jvm212:
      |        folder: ./a
      |      jvm213:
      |        folder: ./a
      |""".stripMargin
  ) { build =>
    val newProjects = build.explodedProjects.map {
      case (crossName @ model.CrossProjectName(model.ProjectName("a"), Some(model.CrossId("jvm213"))), p) =>
        (crossName, p.copy(folder = None))
      case unchanged => unchanged
    }
    BuildRewrite.withProjects(build, newProjects)
  }(
    """projects:
      |  a:
      |    cross:
      |      jvm212:
      |        folder: ./a
      |      jvm213: {}
      |""".stripMargin
  )

  rewriteTest(
    "drop scala option in cross template",
    """projects:
      |  a:
      |    extends: t
      |  b:
      |    extends: t
      |templates:
      |  t:
      |    cross:
      |      jvm212:
      |        scala:
      |          options: -flaff
      |      jvm213:
      |        scala:
      |          options: -flaff
      |""".stripMargin
  ) { build =>
    val newProjects = build.explodedProjects.map {
      case (crossName @ model.CrossProjectName(model.ProjectName("a"), Some(model.CrossId("jvm213"))), p) =>
        (crossName, p.copy(scala = p.scala.map(s => s.copy(options = model.Options.empty))))
      case unchanged => unchanged
    }
    BuildRewrite.withProjects(build, newProjects)
  }(
    """projects:
      |  a:
      |    extends: t
      |  b:
      |    cross:
      |      jvm213:
      |        scala:
      |          options: -flaff
      |    extends: t
      |templates:
      |  t:
      |    cross:
      |      jvm212:
      |        scala:
      |          options: -flaff
      |      jvm213: {}
      |""".stripMargin
  )

  rewriteTest(
    "drop inherited cross project",
    """projects:
      |  a:
      |    extends: t
      |  b:
      |    extends: t
      |templates:
      |  t:
      |    cross:
      |      jvm212:
      |        scala:
      |          options: -flaff
      |      jvm213:
      |        scala:
      |          options: -flaff
      |""".stripMargin
  ) { build =>
    val newProjects = build.explodedProjects.filter {
      case (model.CrossProjectName(model.ProjectName("a"), Some(model.CrossId("jvm212"))), _) => false
      case _                                                                                  => true
    }
    BuildRewrite.withProjects(build, newProjects)
  }(
    """projects:
      |  a:
      |    extends: t
      |  b:
      |    cross:
      |      jvm212:
      |        scala:
      |          options: -flaff
      |    extends: t
      |templates:
      |  t:
      |    cross:
      |      jvm213:
      |        scala:
      |          options: -flaff
      |""".stripMargin
  )
}
