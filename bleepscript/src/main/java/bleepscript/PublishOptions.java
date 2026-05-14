package bleepscript;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Supplier;

public record PublishOptions(
    String groupId,
    Optional<String> versionOverride,
    Optional<Supplier<String>> versionFallback,
    boolean assertRelease,
    boolean dryRun,
    PublishTarget target,
    List<CrossProjectName> projects,
    Optional<ManifestCreator> manifestCreator) {

  public PublishOptions {
    Objects.requireNonNull(groupId, "groupId");
    Objects.requireNonNull(versionOverride, "versionOverride");
    Objects.requireNonNull(versionFallback, "versionFallback");
    Objects.requireNonNull(target, "target");
    Objects.requireNonNull(projects, "projects");
    Objects.requireNonNull(manifestCreator, "manifestCreator");
    if (projects.isEmpty()) {
      throw new IllegalArgumentException("projects must not be empty");
    }
    if (versionOverride.isEmpty() && versionFallback.isEmpty()) {
      throw new IllegalArgumentException(
          "at least one of versionOverride or versionFallback is required");
    }
    projects = List.copyOf(projects);
  }

  public static Builder builder() {
    return new Builder();
  }

  public static final class Builder {
    private String groupId;
    private String versionOverride;
    private Supplier<String> versionFallback;
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

    /** Set the version explicitly. Takes precedence over {@link #versionFallback}. */
    public Builder version(String version) {
      this.versionOverride = Objects.requireNonNull(version, "version");
      return this;
    }

    /**
     * Set a fallback version supplier, called only when no explicit version is set. Typical use:
     * derive the version from a git tag.
     */
    public Builder versionFallback(Supplier<String> versionFallback) {
      this.versionFallback = Objects.requireNonNull(versionFallback, "versionFallback");
      return this;
    }

    /**
     * When true, refuse to publish if the resolved version is not a release version (contains
     * {@code +} or ends with {@code -SNAPSHOT}). Only applies to versions resolved via {@link
     * #versionFallback}; explicit {@link #version} values are not checked.
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
      if (versionOverride == null && versionFallback == null)
        throw new IllegalStateException("at least one of version or versionFallback is required");
      return new PublishOptions(
          groupId,
          Optional.ofNullable(versionOverride),
          Optional.ofNullable(versionFallback),
          assertRelease,
          dryRun,
          target,
          projects,
          Optional.ofNullable(manifestCreator));
    }
  }
}
