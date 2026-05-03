package bleepscript;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

public record PublishOptions(
    String groupId,
    String version,
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
    private String version;
    private PublishTarget target;
    private final List<CrossProjectName> projects = new ArrayList<>();
    private ManifestCreator manifestCreator;

    private Builder() {}

    public Builder groupId(String groupId) {
      this.groupId = Objects.requireNonNull(groupId, "groupId");
      return this;
    }

    public Builder version(String version) {
      this.version = Objects.requireNonNull(version, "version");
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

    public Builder toMavenFolder(Path path) {
      this.target = new PublishTarget.MavenFolder(Objects.requireNonNull(path, "path"));
      return this;
    }

    public Builder manifestCreator(ManifestCreator creator) {
      this.manifestCreator = Objects.requireNonNull(creator, "creator");
      return this;
    }

    public PublishOptions build() {
      if (groupId == null) throw new IllegalStateException("groupId is required");
      if (version == null) throw new IllegalStateException("version is required");
      if (target == null)
        throw new IllegalStateException("target is required (toLocalIvy or toMavenFolder)");
      if (projects.isEmpty()) throw new IllegalStateException("at least one project is required");
      return new PublishOptions(
          groupId, version, target, projects, Optional.ofNullable(manifestCreator));
    }
  }
}
