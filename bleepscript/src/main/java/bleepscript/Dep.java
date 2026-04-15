package bleepscript;

import java.util.Objects;

public sealed interface Dep permits Dep.Java, Dep.Scala {
  String organization();

  String moduleName();

  String version();

  boolean transitive();

  String repr();

  record Java(String organization, String moduleName, String version, boolean transitive)
      implements Dep {
    public Java {
      Objects.requireNonNull(organization, "organization");
      Objects.requireNonNull(moduleName, "moduleName");
      Objects.requireNonNull(version, "version");
    }

    @Override
    public String repr() {
      return organization + ":" + moduleName + ":" + version;
    }
  }

  record Scala(
      String organization,
      String moduleName,
      String version,
      boolean transitive,
      boolean fullCrossVersion,
      boolean forceJvm,
      boolean for3Use213,
      boolean for213Use3)
      implements Dep {
    public Scala {
      Objects.requireNonNull(organization, "organization");
      Objects.requireNonNull(moduleName, "moduleName");
      Objects.requireNonNull(version, "version");
    }

    @Override
    public String repr() {
      String sep = fullCrossVersion ? ":::" : "::";
      return organization + sep + moduleName + ":" + version;
    }
  }
}
