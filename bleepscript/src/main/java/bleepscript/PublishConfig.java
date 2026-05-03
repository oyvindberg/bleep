package bleepscript;

import java.util.Objects;
import java.util.Optional;
import java.util.Set;

public record PublishConfig(
    Optional<Boolean> enabled,
    Optional<String> groupId,
    Optional<String> description,
    Optional<String> url,
    Optional<String> organization,
    Set<Developer> developers,
    Set<License> licenses,
    Optional<String> sonatypeProfileName,
    Optional<String> sonatypeCredentialHost) {

  public PublishConfig {
    Objects.requireNonNull(enabled, "enabled");
    Objects.requireNonNull(groupId, "groupId");
    Objects.requireNonNull(description, "description");
    Objects.requireNonNull(url, "url");
    Objects.requireNonNull(organization, "organization");
    Objects.requireNonNull(developers, "developers");
    Objects.requireNonNull(licenses, "licenses");
    Objects.requireNonNull(sonatypeProfileName, "sonatypeProfileName");
    Objects.requireNonNull(sonatypeCredentialHost, "sonatypeCredentialHost");
    developers = Set.copyOf(developers);
    licenses = Set.copyOf(licenses);
  }

  public boolean isEnabled() {
    return enabled.orElse(true);
  }

  public record Developer(String id, String name, String url) {
    public Developer {
      Objects.requireNonNull(id, "id");
      Objects.requireNonNull(name, "name");
      Objects.requireNonNull(url, "url");
    }
  }

  public record License(String name, Optional<String> url, Optional<String> distribution) {
    public License {
      Objects.requireNonNull(name, "name");
      Objects.requireNonNull(url, "url");
      Objects.requireNonNull(distribution, "distribution");
    }
  }
}
