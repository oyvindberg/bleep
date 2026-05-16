package bleep

import java.nio.file.{Files, Path}

/** Resolves `build.bleep::*` artifacts from compiled class dirs instead of Coursier when the version starts with "dev:".
  *
  * This is needed for forked sourcegen JVMs: they call `bootstrap.forScript()` which re-resolves dependencies via Coursier, but `build.bleep::*:dev` artifacts
  * aren't published to any repository. Instead, we resolve them from the compiled class dirs in the .bleep directory.
  *
  * Two resolution strategies:
  *   1. From buildDir: `<buildDir>/.bleep/projects/<name>/builds/normal/classes` (plus the project's `src/resources/` when present, so SPI files end up on the
  *      dev-mode classpath the same way they would on a published JAR)
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
    "bleep-plugin-dynver" -> cpn("bleep-plugin-dynver"),
    "bleep-plugin-git-versioning" -> cpn("bleep-plugin-git-versioning"),
    "bleep-plugin-jni" -> cpn("bleep-plugin-jni"),
    "bleep-plugin-mdoc" -> cpn("bleep-plugin-mdoc"),
    "bleep-plugin-native-image" -> cpn("bleep-plugin-native-image"),
    "bleep-plugin-scalafix" -> cpn("bleep-plugin-scalafix"),
    "bleep-test-ksp-processor" -> cpn("bleep-test-ksp-processor"),
    "bleep-test-runner" -> cpn("bleep-test-runner")
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
    "bleep-core" -> Set("bleep-model", "bleep-nosbt", "bleep-bsp-protocol", "bleep-plugin-dynver", "bleep-test-runner"),
    "bleep-plugin-dynver" -> Set.empty,
    "bleep-plugin-git-versioning" -> Set("bleep-core", "bleep-model", "bleep-nosbt", "bleep-bsp-protocol", "bleep-plugin-dynver", "bleep-test-runner"),
    "bleep-plugin-jni" -> Set("bleep-core", "bleep-model", "bleep-nosbt", "bleep-bsp-protocol", "bleep-plugin-dynver", "bleep-test-runner"),
    "bleep-plugin-mdoc" -> Set("bleep-core", "bleep-model", "bleep-nosbt", "bleep-bsp-protocol", "bleep-plugin-dynver", "bleep-test-runner"),
    "bleep-plugin-native-image" -> Set("bleep-core", "bleep-model", "bleep-nosbt", "bleep-bsp-protocol", "bleep-plugin-dynver", "bleep-test-runner"),
    "bleep-plugin-scalafix" -> Set("bleep-core", "bleep-model", "bleep-nosbt", "bleep-bsp-protocol", "bleep-plugin-dynver", "bleep-test-runner"),
    "bleep-test-ksp-processor" -> Set.empty,
    "bleep-test-runner" -> Set.empty
  )

  /** Compute the class dir path for a CrossProjectName within a build directory.
    *
    * Uses the same path convention as BuildPaths: `<buildDir>/.bleep/projects/<crossName>/builds/normal/classes`.
    */
  def classDir(buildDir: Path, crossName: model.CrossProjectName): Path =
    buildDir
      .resolve(".bleep")
      .resolve("projects")
      .resolve(crossName.value)
      .resolve("builds")
      .resolve("normal")
      .resolve("classes")

  /** Best-effort source-resources directories for a dev-resolved artifact, so dev consumers see resources the same way published consumers do (resources are
    * inside the JAR for published artifacts). Probes the two shapes used in bleep's own build: `<projectName>/src/resources` (kotlin / scala source-layout) and
    * `<projectName>/src/main/resources` (sbt-matrix). Returns whichever exists. Used so that test fixtures (e.g. `bleep-test-ksp-processor`'s META-INF/services
    * SPI file) land on the dev-mode classpath.
    */
  def resourceDirs(buildDir: Path, crossName: model.CrossProjectName): List[Path] =
    List(
      buildDir.resolve(crossName.name.value).resolve("src").resolve("resources"),
      buildDir.resolve(crossName.name.value).resolve("src").resolve("main").resolve("resources")
    ).filter(p => Files.isDirectory(p))

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
      case None          => Nil
      case Some(selfCpn) =>
        val selfDir = classDir(buildDir, selfCpn)
        if (Files.isDirectory(selfDir)) {
          // Class dirs exist at the expected path — use them directly
          val transitiveCps = transitiveBleepDeps.getOrElse(baseName, Set.empty).toList.flatMap(artifacts.get)
          val transitiveDirs = transitiveCps.map(tn => classDir(buildDir, tn))
          val resourcesDirs = (selfCpn :: transitiveCps).flatMap(cn => resourceDirs(buildDir, cn))
          selfDir :: transitiveDirs ++ resourcesDirs
        } else {
          // Class dirs don't exist at buildDir (forked JVM in a different build).
          // The forked JVM was launched with all needed classes on its classpath
          // by SourceGenRunner.buildClasspath, so scan java.class.path.
          resolveFromJvmClasspath(baseName)
        }
    }
  }

  /** Scan the JVM's classpath for class dirs (and `src/resources` / `src/main/resources` dirs) matching the given artifact and its transitive deps.
    *
    * SourceGenRunner launches forked JVMs with `-cp <classpath>` that includes class dirs AND source-resource dirs for each bleep project on the classpath. We
    * match entries ending with `/<projectName>/classes`, `/<projectName>/<crossId>/classes`, `/<projectName>/src/resources`, and
    * `/<projectName>/src/main/resources`. Resources are included so dev-resolved consumers see the same files a published JAR would expose (e.g.
    * META-INF/services).
    */
  private def resolveFromJvmClasspath(artifactName: String): List[Path] = {
    val allNames = Set(artifactName) ++ transitiveBleepDeps.getOrElse(artifactName, Set.empty)
    val cpEntries = System
      .getProperty("java.class.path", "")
      .split(java.io.File.pathSeparator)
      .iterator
      .filter(_.nonEmpty)
      .map(Path.of(_))
      .toArray

    def matchesClassesDir(entry: Path, crossName: model.CrossProjectName): Boolean =
      entry.toString.endsWith("/classes") && {
        val parent = entry.getParent
        if (parent == null) false
        else {
          // v1 layout: `<projectName>/classes` (cross-less) or `<projectName>/<crossId>/classes` (cross-built).
          // v2 layout: `.bleep/projects/<crossName>/builds/<variant>/classes` — parent is `<variant>`, grandparent `builds`, ancestor `<crossName>`.
          def v1Match: Boolean = crossName.crossId match {
            case None      => parent.getFileName.toString == crossName.name.value
            case Some(cid) =>
              parent.getFileName.toString == cid.value && {
                val grandparent = parent.getParent
                grandparent != null && grandparent.getFileName.toString == crossName.name.value
              }
          }
          def v2Match: Boolean = {
            val grandparent = parent.getParent
            if (grandparent == null || grandparent.getFileName == null || grandparent.getFileName.toString != "builds") false
            else {
              val crossDir = grandparent.getParent
              crossDir != null && crossDir.getFileName != null && crossDir.getFileName.toString == crossName.value
            }
          }
          v1Match || v2Match
        }
      }

    def matchesResourcesDir(entry: Path, crossName: model.CrossProjectName): Boolean = {
      val s = entry.toString
      // Match `<workspace>/<projectName>/src/resources` (kotlin/scala source-layout) or `<workspace>/<projectName>/src/main/resources` (sbt-matrix).
      // Cross-built projects don't currently expose per-variant resources, so we ignore crossId here — first match wins.
      def projectMatches(suffix: String): Boolean = {
        if (!s.endsWith(suffix)) return false
        val segments = suffix.count(_ == '/')
        var p: Path = entry
        var i = 0
        while (i < segments && p != null) { p = p.getParent; i += 1 }
        p != null && p.getFileName != null && p.getFileName.toString == crossName.name.value
      }
      projectMatches("/src/resources") || projectMatches("/src/main/resources")
    }

    allNames.toList.flatMap { name =>
      artifacts.get(name).toList.flatMap { crossName =>
        val classes = cpEntries.find(e => matchesClassesDir(e, crossName)).map(_.toAbsolutePath)
        val resources = cpEntries.filter(e => matchesResourcesDir(e, crossName)).map(_.toAbsolutePath).toList
        classes.toList ++ resources
      }
    }
  }
}
