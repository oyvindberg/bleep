package bleepscript;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

public record PublishOptions(
    String groupId,
    PublishVersion version,
    boolean assertRelease,
    boolean dryRun,
    PublishTarget target,
    List<CrossProjectName> projects,
    Optional<ManifestCreator> manifestCreator) {

  public PublishOptions {
    Objects.requireNonNull(groupId, "groupId");
    Objects.requireNonNull(version, "version");
    Objects.requireNonNull(target, "target");
    Objects.requireNonNull(projects, "projects");
    Objects.requireNonNull(manifestCreator, "manifestCreator");
    if (projects.isEmpty()) {
      throw new IllegalArgumentException("projects must not be empty");
    }
    projects = List.copyOf(projects);
  }

  public static Builder builder() {
    return new Builder();
  }

  public static final class Builder {
    private String groupId;
    private PublishVersion version = PublishVersion.fromGit();
    private boolean assertRelease = false;
    private boolean dryRun = false;
    private PublishTarget target;
    private final List<CrossProjectName> projects = new ArrayList<>();
    private ManifestCreator manifestCreator;

    private Builder() {}

    public Builder groupId(String groupId) {
      this.groupId = Objects.requireNonNull(groupId, "groupId");
      return this;
    }

    /** Publish exactly this version. */
    public Builder version(String version) {
      this.version = PublishVersion.of(version);
      return this;
    }

    public Builder version(PublishVersion version) {
      this.version = Objects.requireNonNull(version, "version");
      return this;
    }

    /**
     * Derive the version from git tags, which is also what happens when no version is set at all.
     * Git is read when the publish runs.
     */
    public Builder versionFromGit() {
      this.version = PublishVersion.fromGit();
      return this;
    }

    /**
     * When true, refuse to publish if git state would produce a snapshot version. Applies only to
     * {@link PublishVersion.Dynver}: a version you spelled out yourself is published as given,
     * since bleep has no better source to contradict it with.
     */
    public Builder assertRelease(boolean assertRelease) {
      this.assertRelease = assertRelease;
      return this;
    }

    /**
     * When true, render what would be published without uploading anything. The artifact contents
     * are printed for inspection.
     */
    public Builder dryRun(boolean dryRun) {
      this.dryRun = dryRun;
      return this;
    }

    public Builder project(CrossProjectName project) {
      this.projects.add(Objects.requireNonNull(project, "project"));
      return this;
    }

    public Builder projects(List<CrossProjectName> projects) {
      this.projects.addAll(Objects.requireNonNull(projects, "projects"));
      return this;
    }

    public Builder projects(CrossProjectName... projects) {
      for (CrossProjectName p : projects) {
        this.projects.add(Objects.requireNonNull(p, "project"));
      }
      return this;
    }

    public Builder toLocalIvy() {
      this.target = PublishTarget.LocalIvy.INSTANCE;
      return this;
    }

    public Builder toMavenFolder(java.nio.file.Path path) {
      this.target = new PublishTarget.MavenFolder(Objects.requireNonNull(path, "path"));
      return this;
    }

    /**
     * Publish to a named resolver declared in {@code bleep.yaml}. Credentials come from the user's
     * {@code ~/.config/bleep/config} {@code authentications:} section, looked up by host.
     */
    public Builder toResolver(String resolverName) {
      this.target =
          new PublishTarget.Resolver(Objects.requireNonNull(resolverName, "resolverName"));
      return this;
    }

    /**
     * Publish to Sonatype (Maven Central). Signs all artifacts via the caller's gpg agent, bundles
     * them, uploads through the Sonatype Central Portal API. Reads SONATYPE_USERNAME /
     * SONATYPE_PASSWORD from the environment.
     */
    public Builder toSonatypeCentral(String profileName, String credentialHost) {
      this.target =
          new PublishTarget.SonatypeCentral(
              Objects.requireNonNull(profileName, "profileName"),
              Objects.requireNonNull(credentialHost, "credentialHost"));
      return this;
    }

    public Builder manifestCreator(ManifestCreator creator) {
      this.manifestCreator = Objects.requireNonNull(creator, "creator");
      return this;
    }

    public PublishOptions build() {
      if (groupId == null) throw new IllegalStateException("groupId is required");
      if (target == null)
        throw new IllegalStateException(
            "target is required (toLocalIvy, toMavenFolder, or toResolver)");
      if (projects.isEmpty()) throw new IllegalStateException("at least one project is required");
      return new PublishOptions(
          groupId,
          version,
          assertRelease,
          dryRun,
          target,
          projects,
          Optional.ofNullable(manifestCreator));
    }
  }
}
