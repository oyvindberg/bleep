package bleep

import coursier.paths.shaded.dirs.{ProjectDirectories, UserDirectories}

import java.nio.file.Path

/** Every machine/user-level fact bleep needs, resolved once per process — `homeDir`/`tempDir` live here, not threaded as their own loose parameters, for the
  * same reason `cacheDir`/`configDir` already do: one named value instead of a `System.getProperty` read repeated (and potentially drifting) at each call site
  * that needs it.
  */
case class UserPaths(cacheDir: Path, configDir: Path, homeDir: Path, tempDir: Path) {
  val bspSocketDir = cacheDir / "socket"
  val resolveCacheDir = cacheDir / "coursier-v2"
  val resolveJvmCacheDir = cacheDir / "coursier-jvms"
  val configYaml = configDir / "config.yaml"
}

object UserPaths {

  /** Resolved once per process. On Windows this is a real Win32 `SHGetKnownFolderPath` call through the FFM API — cheap, but the call sites are not: `Main`
    * does it before anything else, and the BSP server does it from five places. Prior to the JDK-22 floor this went through `powershell.exe -EncodedCommand`
    * compiling C# with `Add-Type` on *every* call, which is what made Windows startup slow and gave us a hard dependency on PowerShell being present,
    * unconstrained, and able to write to TEMP.
    */
  lazy val fromAppDirs: UserPaths = {
    val dirs = ProjectDirectories.from("build", null, "bleep")
    val cacheDir = Path.of(dirs.cacheDir)
    val configDir = Path.of(dirs.configDir)
    // Not `System.getProperty("user.home")`: `UserDirectories` resolves it the same robust way `ProjectDirectories` resolves cacheDir/configDir above
    // (the real Win32 call on Windows), rather than a JVM system property that can disagree with it.
    val homeDir = Path.of(UserDirectories.get().homeDir)
    // No equivalent in `BaseDirectories`/`UserDirectories` for this one — `java.io.tmpdir` is a JVM-level concept (where the JVM itself puts scratch
    // files), not an OS "user directories" concept, so the system property is the right source here, not a hand-rolled substitute for it.
    val tempDir = Path.of(System.getProperty("java.io.tmpdir"))

    UserPaths(cacheDir, configDir, homeDir, tempDir)
  }
}
