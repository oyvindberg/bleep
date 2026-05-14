package scripts;

import bleepscript.BleepScript;
import bleepscript.BleepscriptServices;
import bleepscript.Commands;
import bleepscript.CrossProjectName;
import bleepscript.Logger;
import bleepscript.PackagedLibrary;
import bleepscript.Packaging;
import bleepscript.PublishLayout;
import bleepscript.Started;
import java.util.List;
import java.util.Optional;

/**
 * Compose: compile, package, {@link Proguard#shrink} the main JAR, publish.
 *
 * <p>The "plugin" lives next door in {@link Proguard}. This script is just glue:
 * arg parsing + four bleepscript API calls.
 *
 * <p>Usage: {@code bleep proguard-publish <projectName> <version> <groupId>}.
 */
public final class ProguardPublish extends BleepScript {
  public ProguardPublish() {
    super("proguard-publish");
  }

  @Override
  public void run(Started started, Commands commands, List<String> args) {
    if (args.size() < 3) {
      throw new IllegalArgumentException(
          "Usage: bleep proguard-publish <projectName> <version> <groupId>");
    }
    String projectName = args.get(0);
    String version = args.get(1);
    String groupId = args.get(2);

    CrossProjectName project = new CrossProjectName(projectName, Optional.empty());
    Logger log = started.logger().withContext("project", projectName);

    commands.compile(List.of(project));

    log.info("packaging");
    PackagedLibrary library =
        Packaging.packageProject(
            started,
            project,
            groupId,
            version,
            PublishLayout.Maven.INSTANCE,
            BleepscriptServices.Holder.INSTANCE.defaultManifestCreator());

    byte[] originalJar = library.jarFile();
    log.info("main JAR: " + library.jarFilePath().asString() + " (" + originalJar.length + " bytes)");

    byte[] shrunk = Proguard.create(started).shrink(originalJar);
    log.info(
        "ProGuard output: " + shrunk.length + " bytes (saved " + (originalJar.length - shrunk.length) + ")");

    Packaging.publishToLocalMaven(library.withJarFile(shrunk));
    log.info("Published " + groupId + ":" + projectName + ":" + version);
  }
}
