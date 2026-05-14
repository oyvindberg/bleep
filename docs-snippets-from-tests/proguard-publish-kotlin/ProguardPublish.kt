package scripts

import bleepscript.BleepScript
import bleepscript.BleepscriptServices
import bleepscript.Commands
import bleepscript.CrossProjectName
import bleepscript.PackagedLibrary
import bleepscript.Packaging
import bleepscript.PublishLayout
import bleepscript.Started
import java.util.Optional

/**
 * Compose: compile, package, [Proguard.shrink] the main JAR, publish.
 *
 * The "plugin" lives next door in [Proguard]. This script is just glue: arg
 * parsing + four bleepscript API calls.
 *
 * Usage: `bleep proguard-publish <projectName> <version> <groupId>`.
 */
class ProguardPublish : BleepScript("proguard-publish") {
  override fun run(started: Started, commands: Commands, args: List<String>) {
    require(args.size >= 3) {
      "Usage: bleep proguard-publish <projectName> <version> <groupId>"
    }
    val projectName = args[0]
    val version = args[1]
    val groupId = args[2]

    val project = CrossProjectName(projectName, Optional.empty())
    val log = started.logger().withContext("project", projectName)

    commands.compile(listOf(project))

    log.info("packaging")
    val library = Packaging.packageProject(
      started,
      project,
      groupId,
      version,
      PublishLayout.Maven.INSTANCE,
      BleepscriptServices.Holder.INSTANCE.defaultManifestCreator()
    )

    val originalJar = library.jarFile()
    log.info("main JAR: ${library.jarFilePath().asString()} (${originalJar.size} bytes)")

    val shrunk = Proguard.create(started).shrink(originalJar)
    log.info("ProGuard output: ${shrunk.size} bytes (saved ${originalJar.size - shrunk.size})")

    Packaging.publishToLocalMaven(library.withJarFile(shrunk))
    log.info("Published $groupId:$projectName:$version")
  }
}
