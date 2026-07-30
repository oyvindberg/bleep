package bleep

import coursier.paths.shaded.dirs.ProjectDirectories

import java.nio.file.Path

case class UserPaths(cacheDir: Path, configDir: Path) {
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

    UserPaths(cacheDir, configDir)
  }
}
