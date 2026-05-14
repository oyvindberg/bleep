package scripts

import bleepscript.{
  BleepScript,
  BleepscriptServices,
  Commands,
  CrossProjectName,
  Packaging,
  PublishLayout,
  Started
}

import java.util.Optional
import scala.jdk.CollectionConverters.*

/** Compose: compile, package, [[Proguard.shrink]] the main JAR, publish.
  *
  * The "plugin" lives next door in [[Proguard]]. This script is just glue: arg
  * parsing + four bleepscript API calls.
  *
  * Usage: `bleep proguard-publish <projectName> <version> <groupId>`.
  */
class ProguardPublish extends BleepScript("proguard-publish"):

  override def run(started: Started, commands: Commands, args: java.util.List[String]): Unit =
    val argList = args.asScala.toList
    require(argList.length >= 3, "Usage: bleep proguard-publish <projectName> <version> <groupId>")
    val List(projectName, version, groupId, _*) = argList

    val project = new CrossProjectName(projectName, Optional.empty())
    val log = started.logger.withContext("project", projectName)

    commands.compile(java.util.List.of(project))

    log.info("packaging")
    val library = Packaging.packageProject(
      started,
      project,
      groupId,
      version,
      PublishLayout.Ivy.INSTANCE,
      BleepscriptServices.Holder.INSTANCE.defaultManifestCreator()
    )

    val originalJar = library.jarFile
    log.info(s"main JAR: ${library.jarFilePath.asString} (${originalJar.length} bytes)")

    val shrunk = Proguard(started).shrink(originalJar)
    log.info(s"ProGuard output: ${shrunk.length} bytes (saved ${originalJar.length - shrunk.length})")

    Packaging.publishToLocalIvy(library.withJarFile(shrunk))
    log.info(s"Published $groupId:$projectName:$version")
