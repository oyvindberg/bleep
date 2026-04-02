package bleep

import java.nio.file.{Files, Path}

/** Resolves `build.bleep::*` artifacts from compiled class dirs instead of Coursier when the version starts with "dev:".
  *
  * This is needed for forked sourcegen JVMs: they call `bootstrap.forScript()` which re-resolves dependencies via Coursier, but `build.bleep::*:dev` artifacts
  * aren't published to any repository. Instead, we resolve them from the compiled class dirs in the .bleep directory.
  *
  * Two resolution strategies:
  *   1. From buildDir: `<buildDir>/.bleep/builds/normal/.bloop/<name>/classes`
  *   2. From JVM classpath: scan `java.class.path` for matching entries (used by forked sourcegen JVMs where buildDir points to a consumer build, not bleep's
  *      own build)
  *
  * Lives in prod scope because forked sourcegen JVMs load it.
  */
object BleepDevDeps {
  val DevPrefix = "dev:"

  def isDevVersion(version: String): Boolean = version.startsWith(DevPrefix)
  def devBuildDir(version: String): Path = Path.of(version.stripPrefix(DevPrefix))

  private def cpn(name: String): model.CrossProjectName =
    model.CrossProjectName(model.ProjectName(name), None)

  /** Hardcoded mapping: artifact base name -> CrossProjectName.
    *
    * Verified by BleepDevDepsTest to match bleep's own build.
    */
  val artifacts: Map[String, model.CrossProjectName] = Map(
    "bleep-core" -> cpn("bleep-core"),
    "bleep-model" -> cpn("bleep-model"),
    "bleep-nosbt" -> cpn("bleep-nosbt"),
    "bleep-bsp-protocol" -> cpn("bleep-bsp-protocol"),
    "bleep-plugin-ci-release" -> cpn("bleep-plugin-ci-release"),
    "bleep-plugin-dynver" -> cpn("bleep-plugin-dynver"),
    "bleep-plugin-git-versioning" -> cpn("bleep-plugin-git-versioning"),
    "bleep-plugin-jni" -> cpn("bleep-plugin-jni"),
    "bleep-plugin-mdoc" -> cpn("bleep-plugin-mdoc"),
    "bleep-plugin-native-image" -> cpn("bleep-plugin-native-image"),
    "bleep-plugin-pgp" -> cpn("bleep-plugin-pgp"),
    "bleep-plugin-scalafix" -> cpn("bleep-plugin-scalafix"),
    "bleep-plugin-sonatype" -> cpn("bleep-plugin-sonatype")
  )

  /** Hardcoded transitive `build.bleep::*` dependencies for each artifact.
    *
    * When we intercept a dev dep, we also need class dirs for its transitive bleep dependencies since Coursier won't discover them (no POM to read).
    *
    * Verified by BleepDevDepsTest to match bleep's own build.
    */
  val transitiveBleepDeps: Map[String, Set[String]] = Map(
    "bleep-model" -> Set.empty,
    "bleep-nosbt" -> Set.empty,
    "bleep-bsp-protocol" -> Set("bleep-model"),
    "bleep-core" -> Set("bleep-model", "bleep-nosbt", "bleep-bsp-protocol"),
    "bleep-plugin-dynver" -> Set("bleep-core", "bleep-model", "bleep-nosbt", "bleep-bsp-protocol"),
    "bleep-plugin-git-versioning" -> Set("bleep-core", "bleep-model", "bleep-nosbt", "bleep-bsp-protocol"),
    "bleep-plugin-jni" -> Set("bleep-core", "bleep-model", "bleep-nosbt", "bleep-bsp-protocol"),
    "bleep-plugin-mdoc" -> Set("bleep-core", "bleep-model", "bleep-nosbt", "bleep-bsp-protocol"),
    "bleep-plugin-native-image" -> Set("bleep-core", "bleep-model", "bleep-nosbt", "bleep-bsp-protocol"),
    "bleep-plugin-pgp" -> Set("bleep-core", "bleep-model", "bleep-nosbt", "bleep-bsp-protocol"),
    "bleep-plugin-scalafix" -> Set("bleep-core", "bleep-model", "bleep-nosbt", "bleep-bsp-protocol"),
    "bleep-plugin-sonatype" -> Set("bleep-core", "bleep-model", "bleep-nosbt", "bleep-bsp-protocol"),
    "bleep-plugin-ci-release" -> Set(
      "bleep-plugin-dynver",
      "bleep-plugin-pgp",
      "bleep-plugin-sonatype",
      "bleep-core",
      "bleep-model",
      "bleep-nosbt",
      "bleep-bsp-protocol"
    )
  )

  /** Compute the class dir path for a CrossProjectName within a build directory.
    *
    * Uses the same path convention as BuildPaths: `<buildDir>/.bleep/builds/normal/.bloop/<name>/<crossId>/classes`
    */
  def classDir(buildDir: Path, crossName: model.CrossProjectName): Path = {
    val bleepBloopDir = buildDir.resolve(".bleep").resolve("builds").resolve("normal").resolve(".bloop")
    val crossPart = crossName.crossId.fold("")(_.value)
    bleepBloopDir.resolve(crossName.name.value).resolve(crossPart).resolve("classes")
  }

  /** Resolve all class dirs for a `build.bleep::*` dep with a `dev:` version.
    *
    * First tries to resolve from the build dir encoded in the version. If those class dirs don't exist (e.g., forked JVM running in a consumer build's temp
    * dir), falls back to scanning the JVM's own classpath for matching entries.
    *
    * Returns class dirs for the artifact itself plus all its transitive bleep deps. Returns empty list if the artifact is not known or the version is not dev.
    */
  def resolveAllClassDirs(dep: model.Dep): List[Path] = {
    if (!isDevVersion(dep.version)) return Nil
    val buildDir = devBuildDir(dep.version)
    val baseName = dep.baseModuleName.value
    artifacts.get(baseName) match {
      case None => Nil
      case Some(selfCpn) =>
        val selfDir = classDir(buildDir, selfCpn)
        if (Files.isDirectory(selfDir)) {
          // Class dirs exist at the expected path — use them directly
          val transitiveDirs = transitiveBleepDeps.getOrElse(baseName, Set.empty).toList.flatMap { transName =>
            artifacts.get(transName).map(tn => classDir(buildDir, tn))
          }
          selfDir :: transitiveDirs
        } else {
          // Class dirs don't exist at buildDir (forked JVM in a different build).
          // The forked JVM was launched with all needed classes on its classpath
          // by SourceGenRunner.buildClasspath, so scan java.class.path.
          resolveFromJvmClasspath(baseName)
        }
    }
  }

  /** Scan the JVM's classpath for class dirs matching the given artifact and its transitive deps.
    *
    * SourceGenRunner launches forked JVMs with `-cp <classpath>` that includes all needed class dirs. We match entries ending with `/<projectName>/classes` or
    * `/<projectName>/<crossId>/classes`.
    */
  private def resolveFromJvmClasspath(artifactName: String): List[Path] = {
    val allNames = Set(artifactName) ++ transitiveBleepDeps.getOrElse(artifactName, Set.empty)
    val rawCp = System.getProperty("java.class.path", "")
    val cpEntries = rawCp
      .split(java.io.File.pathSeparator)
      .filter(_.nonEmpty)
      .map(Path.of(_))

    allNames.toList.flatMap { name =>
      artifacts.get(name).flatMap { crossName =>
        val crossPart = crossName.crossId.fold("")(_.value)
        cpEntries
          .find { entry =>
            val s = entry.toString
            s.endsWith("/classes") && {
              val parent = entry.getParent
              if (parent == null) false
              else if (crossPart.isEmpty)
                // No cross ID: match .../bleep-core/classes
                parent.getFileName.toString == crossName.name.value
              else
                // With cross ID: match .../bleep-core/jvm3/classes
                parent.getFileName.toString == crossPart && {
                  val grandparent = parent.getParent
                  grandparent != null && grandparent.getFileName.toString == crossName.name.value
                }
            }
          }
          .map(_.toAbsolutePath)
      }
    }
  }
}
