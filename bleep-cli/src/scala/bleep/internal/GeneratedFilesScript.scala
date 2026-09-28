package bleep
package internal

import java.nio.file.Path

/** Files sbt or Maven generated in the build we import from are kept as they are, in the scripts project, and one sourcegen script copies them into the
  * generated sources and resources of the projects they were generated for. It is a stand-in which keeps the build working until it is replaced by code which
  * generates the files.
  *
  * They are kept in `scripts/resources/<cross project>/sources` and `scripts/resources/<cross project>/resources`. Being resources of the scripts project makes
  * them inputs of the script, so sourcegen runs again when one is edited.
  */
object GeneratedFilesScript {
  val className = "CopyImportedGeneratedFiles"
  val projectName: model.CrossProjectName = model.CrossProjectName(model.ProjectName("scripts"), None)
  private val keptIn = "resources"

  case class Generated(
      /** the scripts project, which holds the script and the files it copies */
      scriptsProject: model.Project,
      /** the script and the files it copies */
      files: Map[Path, String],
      /** the projects which need the script as sourcegen */
      projects: Set[model.ProjectName],
      scriptDef: model.ScriptDef
  )

  def apply(
      destinationPaths: BuildPaths,
      bleepVersion: model.BleepVersion,
      /** every cross project of the imported build */
      crossProjects: Set[model.CrossProjectName],
      generatedFiles: Map[model.CrossProjectName, Vector[GeneratedFile]]
  ): Generated = {
    val scriptsProject = model.Project.empty.copy(
      dependencies = model.JsonSet(model.Dep.Java("build.bleep", "bleepscript", bleepVersion.value)),
      platform = Some(model.Platform.Jvm(model.Options.empty, None, model.Options.empty)),
      resources = model.JsonSet(RelPath.force(keptIn))
    )
    val scriptsPaths = destinationPaths.project(projectName, scriptsProject, scriptsProject.platform.flatMap(_.name).toSet)
    val scriptsDir = scriptsPaths.dir
    val sourcesDir = scriptsPaths.sourcesDirs.fromSourceLayout.toList match {
      case List(one) => one
      case other     => throw new BleepException.Text(s"Expected the scripts project to have one source directory, it has ${other.mkString(", ")}")
    }

    val kept: Map[model.CrossProjectName, Vector[GeneratedFile]] =
      generatedFiles.filter { case (crossName, files) => files.nonEmpty && crossProjects(crossName) }

    val keptFiles: Map[Path, String] =
      kept.toList.flatMap { case (crossName, files) =>
        files.map { file =>
          val kind = if (file.isResource) "resources" else "sources"
          scriptsDir / keptIn / crossName.fileSafeValue / kind / file.toRelPath.toString -> file.contents
        }
      }.toMap

    val script =
      s"""package scripts;
         |
         |import bleepscript.BleepCodegenScript;
         |import bleepscript.CodegenTarget;
         |import bleepscript.Commands;
         |import bleepscript.Started;
         |import java.io.IOException;
         |import java.io.UncheckedIOException;
         |import java.nio.file.Files;
         |import java.nio.file.Path;
         |import java.nio.file.StandardCopyOption;
         |import java.util.List;
         |import java.util.stream.Stream;
         |
         |/**
         | * Copies the files sbt or Maven generated when this build was imported, which are kept in
         | * `scripts/$keptIn/<cross project>`. A stand-in until it is replaced by code which generates them.
         | */
         |public final class $className extends BleepCodegenScript {
         |  public $className() {
         |    super("$className");
         |  }
         |
         |  @Override
         |  public void run(Started started, Commands commands, List<CodegenTarget> targets, List<String> args) {
         |    started.logger().warn("Copying files generated when this build was imported. Replace this script with code which generates them");
         |    Path kept = started.buildPaths().buildDir().resolve("${RelPath.relativeTo(destinationPaths.buildDir, scriptsDir / keptIn)}");
         |    for (CodegenTarget target : targets) {
         |      Path project = kept.resolve(target.project().asString().replace('/', '-'));
         |      copy(project.resolve("sources"), target.sources());
         |      copy(project.resolve("resources"), target.resources());
         |    }
         |  }
         |
         |  private static void copy(Path from, Path to) {
         |    if (!Files.isDirectory(from)) return;
         |    try (Stream<Path> files = Files.walk(from)) {
         |      for (Path file : (Iterable<Path>) files.filter(Files::isRegularFile)::iterator) {
         |        Path dest = to.resolve(from.relativize(file).toString());
         |        Files.createDirectories(dest.getParent());
         |        Files.copy(file, dest, StandardCopyOption.REPLACE_EXISTING);
         |      }
         |    } catch (IOException e) {
         |      throw new UncheckedIOException(e);
         |    }
         |  }
         |}
         |""".stripMargin

    Generated(
      scriptsProject = scriptsProject,
      files = keptFiles.updated(sourcesDir / s"scripts/$className.java", script),
      projects = kept.keySet.map(_.name),
      scriptDef = model.ScriptDef.Main(projectName, s"scripts.$className", model.JsonSet.empty, model.JsonSet.empty)
    )
  }
}
