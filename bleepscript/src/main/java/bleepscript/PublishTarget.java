package bleepscript;

import java.nio.file.Path;
import java.util.Objects;

public sealed interface PublishTarget
    permits PublishTarget.LocalIvy,
        PublishTarget.MavenFolder,
        PublishTarget.Resolver,
        PublishTarget.SonatypeCentral {

  /** Publish to the user's local Ivy cache (~/.ivy2). */
  final class LocalIvy implements PublishTarget {
    public static final LocalIvy INSTANCE = new LocalIvy();

    private LocalIvy() {}
  }

  /** Publish to a local Maven-layout folder. */
  record MavenFolder(Path path) implements PublishTarget {
    public MavenFolder {
      Objects.requireNonNull(path, "path");
    }
  }

  /**
   * Publish to a named resolver defined in {@code bleep.yaml}. The resolver must be present in the
   * build's {@code resolvers:} section; its URI plus any credentials looked up from {@code
   * ~/.config/bleep/config} {@code authentications:} are used for the upload.
   */
  record Resolver(String name) implements PublishTarget {
    public Resolver {
      Objects.requireNonNull(name, "name");
      if (name.isEmpty()) throw new IllegalArgumentException("resolver name must not be empty");
    }
  }

  /**
   * Publish to Sonatype (Maven Central). PGP-signs all artifacts, bundles them, uploads, and
   * releases through the Sonatype Central API. Reads credentials from the {@code SONATYPE_USERNAME}
   * / {@code SONATYPE_PASSWORD} environment variables and the PGP key from the caller's gpg agent.
   *
   * @param profileName the Sonatype profile (typically the groupId, e.g. {@code com.example})
   * @param credentialHost host to authenticate against, e.g. {@code central.sonatype.com} for the
   *     new Central Portal or {@code oss.sonatype.org} for legacy OSSRH
   */
  record SonatypeCentral(String profileName, String credentialHost) implements PublishTarget {
    public SonatypeCentral {
      Objects.requireNonNull(profileName, "profileName");
      Objects.requireNonNull(credentialHost, "credentialHost");
    }
  }
}
