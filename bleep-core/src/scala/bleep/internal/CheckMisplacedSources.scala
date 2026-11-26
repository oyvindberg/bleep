package bleep
package internal

import ryddig.Logger

import java.nio.file.{Files, Path}

/** Warns when a project has source files under sbt/Maven-style `src/{main,test}/{scala,java,kotlin}/` paths that bleep is not picking up.
  *
  * Bleep's default source layout resolves to `src/<lang>/` (because `sbt-scope` defaults to empty), so files placed under `src/main/<lang>/` or
  * `src/test/<lang>/` are silently invisible to the build unless the user explicitly sets `sources:` or `sbt-scope:`. This check surfaces that situation early
  * with an actionable message instead of letting the user discover it via a missing-class error at runtime.
  */
object CheckMisplacedSources {

  private case class Suspect(scope: String, lang: String, extension: String)

  private val suspects: List[Suspect] = for {
    scope <- List("main", "test")
    (lang, ext) <- List("scala" -> ".scala", "java" -> ".java", "kotlin" -> ".kt")
  } yield Suspect(scope, lang, ext)

  def apply(logger: Logger, buildPaths: BuildPaths, build: model.Build): Unit =
    build.explodedProjects.foreach { case (crossName, project) =>
      val projectPaths = buildPaths.project(crossName, project)
      val coveredDirs: Set[Path] = projectPaths.sourcesDirs.all.toSet ++ projectPaths.resourcesDirs.all.toSet

      // A project that has either an explicit `sources:` list OR an explicit `sbt-scope:` has opted into custom layout — assume the user knows what they're
      // doing and skip the heuristic entirely.
      val hasUserOverride = project.sources.values.nonEmpty || project.`sbt-scope`.isDefined
      if (!hasUserOverride) {
        suspects.foreach { suspect =>
          val candidate = projectPaths.dir / "src" / suspect.scope / suspect.lang
          if (Files.isDirectory(candidate) && !coveredDirs.contains(candidate) && containsSourceFile(candidate, suspect.extension)) {
            logger
              .withContext("project", crossName.value)
              .withContext("path", candidate.toString)
              .warn(
                s"found ${suspect.lang} sources under sbt/Maven-style 'src/${suspect.scope}/${suspect.lang}/' that bleep is NOT picking up. " +
                  s"Either move them to 'src/${suspect.lang}/' (bleep's default), set `sources: ./src/${suspect.scope}/${suspect.lang}` " +
                  s"on this project, or set `sbt-scope: ${suspect.scope}` to opt into the sbt-style layout."
              )
          }
        }
      }
    }

  private def containsSourceFile(dir: Path, extension: String): Boolean = {
    val stream = Files.walk(dir)
    try stream.anyMatch(p => Files.isRegularFile(p) && p.toString.endsWith(extension))
    finally stream.close()
  }
}
