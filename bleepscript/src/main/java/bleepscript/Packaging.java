package bleepscript;

import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

/**
 * Static helpers for packaging projects and constructing JARs. Mirrors {@code
 * bleep.packaging.packageLibraries}, {@code bleep.packaging.createJar}, and the publish-bytes
 * primitives used by {@code bleep.commands.Publish}.
 *
 * <p>Typical use: {@link #packageProject} to materialize a project's publish artifacts as bytes,
 * mutate the map via {@link PackagedLibrary#withFiles(Map)}, then publish via {@link
 * #publishToLocalIvy} / {@link #publishToFolder} / {@link #publishToResolver}.
 */
public final class Packaging {
  private Packaging() {}

  /**
   * Package a single project into a {@link PackagedLibrary}. groupId comes from the project's
   * publish config in {@code bleep.yaml}, falling back to {@code fallbackGroupId} when absent.
   */
  public static PackagedLibrary packageProject(
      Started started,
      CrossProjectName project,
      String fallbackGroupId,
      String version,
      PublishLayout layout,
      ManifestCreator manifestCreator) {
    Objects.requireNonNull(started, "started");
    Objects.requireNonNull(project, "project");
    Objects.requireNonNull(fallbackGroupId, "fallbackGroupId");
    Objects.requireNonNull(version, "version");
    Objects.requireNonNull(layout, "layout");
    Objects.requireNonNull(manifestCreator, "manifestCreator");
    return BleepscriptServices.Holder.INSTANCE.packageProject(
        started, project, fallbackGroupId, version, layout, manifestCreator);
  }

  /**
   * Package multiple projects in one DAG walk. The returned map's keys are exactly the requested
   * projects (transitive dependencies are walked internally but not returned).
   */
  public static Map<CrossProjectName, PackagedLibrary> packageProjects(
      Started started,
      List<CrossProjectName> projects,
      String fallbackGroupId,
      String version,
      PublishLayout layout,
      ManifestCreator manifestCreator) {
    Objects.requireNonNull(started, "started");
    Objects.requireNonNull(projects, "projects");
    Objects.requireNonNull(fallbackGroupId, "fallbackGroupId");
    Objects.requireNonNull(version, "version");
    Objects.requireNonNull(layout, "layout");
    Objects.requireNonNull(manifestCreator, "manifestCreator");
    if (projects.isEmpty()) {
      throw new IllegalArgumentException("projects must not be empty");
    }
    return BleepscriptServices.Holder.INSTANCE.packageProjects(
        started, projects, fallbackGroupId, version, layout, manifestCreator);
  }

  /** Build a JAR from one or more class/resource folders. Returns the JAR bytes. */
  public static byte[] createJar(
      JarType jarType,
      ManifestCreator manifestCreator,
      List<Path> fromFolders,
      Optional<CrossProjectName> projectName,
      Optional<String> mainClass) {
    Objects.requireNonNull(jarType, "jarType");
    Objects.requireNonNull(manifestCreator, "manifestCreator");
    Objects.requireNonNull(fromFolders, "fromFolders");
    Objects.requireNonNull(projectName, "projectName");
    Objects.requireNonNull(mainClass, "mainClass");
    return BleepscriptServices.Holder.INSTANCE.createJar(
        jarType, manifestCreator, fromFolders, projectName, mainClass);
  }

  /**
   * Publish a previously-packaged library to the user's local Ivy cache ({@code ~/.ivy2/local}).
   */
  public static void publishToLocalIvy(PackagedLibrary library) {
    Objects.requireNonNull(library, "library");
    BleepscriptServices.Holder.INSTANCE.publishToLocalIvy(library);
  }

  /**
   * Publish a previously-packaged library to the user's local Maven repo ({@code
   * ~/.m2/repository}).
   */
  public static void publishToLocalMaven(PackagedLibrary library) {
    Objects.requireNonNull(library, "library");
    BleepscriptServices.Holder.INSTANCE.publishToLocalMaven(library);
  }

  /** Publish a previously-packaged library to a local Maven-layout folder. */
  public static void publishToFolder(PackagedLibrary library, Path folder) {
    Objects.requireNonNull(library, "library");
    Objects.requireNonNull(folder, "folder");
    BleepscriptServices.Holder.INSTANCE.publishToFolder(library, folder);
  }

  /**
   * Publish a previously-packaged library to a named resolver declared in {@code bleep.yaml}.
   * Credentials are looked up from the user's {@code ~/.config/bleep/config} {@code
   * authentications:} section by host.
   */
  public static void publishToResolver(
      Started started, PackagedLibrary library, String resolverName) {
    Objects.requireNonNull(started, "started");
    Objects.requireNonNull(library, "library");
    Objects.requireNonNull(resolverName, "resolverName");
    BleepscriptServices.Holder.INSTANCE.publishToResolver(started, library, resolverName);
  }

  /**
   * Sign every file in the library with the caller's PGP key (via the local {@code gpg} agent) and
   * return a new library that contains both the original files and the corresponding {@code .asc}
   * detached signatures. Required for publishing to Maven Central; optional for Artifactory and
   * private repos.
   *
   * <p>Uses default gpg behavior: no inline credentials, prompts the agent if the key is locked.
   * For non-interactive CI, pre-unlock the key in your environment before invoking this script.
   */
  public static PackagedLibrary signArtifacts(Started started, PackagedLibrary library) {
    Objects.requireNonNull(started, "started");
    Objects.requireNonNull(library, "library");
    return BleepscriptServices.Holder.INSTANCE.signArtifacts(started, library);
  }
}
