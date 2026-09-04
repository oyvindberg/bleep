package bleepscript;

import java.nio.file.Path;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

public record ResolvedProject(
    String name,
    Path directory,
    Path workspaceDir,
    List<Path> sources,
    List<Path> classpath,
    Path classesDir,
    Optional<List<Path>> resources,
    Language language,
    boolean isTestProject,
    List<String> dependencies,
    List<String> testFrameworks,
    Optional<Resolution> resolution,
    List<Dep> boms) {

  public ResolvedProject {
    Objects.requireNonNull(name, "name");
    Objects.requireNonNull(directory, "directory");
    Objects.requireNonNull(workspaceDir, "workspaceDir");
    Objects.requireNonNull(sources, "sources");
    Objects.requireNonNull(classpath, "classpath");
    Objects.requireNonNull(classesDir, "classesDir");
    Objects.requireNonNull(resources, "resources");
    Objects.requireNonNull(language, "language");
    Objects.requireNonNull(dependencies, "dependencies");
    Objects.requireNonNull(testFrameworks, "testFrameworks");
    Objects.requireNonNull(resolution, "resolution");
    Objects.requireNonNull(boms, "boms");
    boms = List.copyOf(boms);
    sources = List.copyOf(sources);
    classpath = List.copyOf(classpath);
    dependencies = List.copyOf(dependencies);
    testFrameworks = List.copyOf(testFrameworks);
  }

  /** What the resolver produced for this project: every module with its artifacts on disk. */
  public record Resolution(List<ResolvedModule> modules) {
    public Resolution {
      Objects.requireNonNull(modules, "modules");
      modules = List.copyOf(modules);
    }
  }

  public record ResolvedModule(
      String organization, String name, String version, List<ResolvedArtifact> artifacts) {
    public ResolvedModule {
      Objects.requireNonNull(organization, "organization");
      Objects.requireNonNull(name, "name");
      Objects.requireNonNull(version, "version");
      Objects.requireNonNull(artifacts, "artifacts");
      artifacts = List.copyOf(artifacts);
    }
  }

  public record ResolvedArtifact(String name, Optional<String> classifier, Path path) {
    public ResolvedArtifact {
      Objects.requireNonNull(name, "name");
      Objects.requireNonNull(classifier, "classifier");
      Objects.requireNonNull(path, "path");
    }
  }

  public sealed interface Language permits Language.Java, Language.Scala, Language.Kotlin {
    List<String> options();

    List<String> javaOptions();

    record Java(List<String> options) implements Language {
      public Java {
        Objects.requireNonNull(options, "options");
        options = List.copyOf(options);
      }

      @Override
      public List<String> javaOptions() {
        return options;
      }
    }

    record Scala(
        String organization,
        String name,
        String version,
        List<String> options,
        List<Path> compilerJars,
        Optional<Path> analysisFile,
        List<String> javaOptions)
        implements Language {
      public Scala {
        Objects.requireNonNull(organization, "organization");
        Objects.requireNonNull(name, "name");
        Objects.requireNonNull(version, "version");
        Objects.requireNonNull(options, "options");
        Objects.requireNonNull(compilerJars, "compilerJars");
        Objects.requireNonNull(analysisFile, "analysisFile");
        Objects.requireNonNull(javaOptions, "javaOptions");
        options = List.copyOf(options);
        compilerJars = List.copyOf(compilerJars);
        javaOptions = List.copyOf(javaOptions);
      }
    }

    record Kotlin(
        String version, List<String> options, List<Path> compilerJars, List<String> javaOptions)
        implements Language {
      public Kotlin {
        Objects.requireNonNull(version, "version");
        Objects.requireNonNull(options, "options");
        Objects.requireNonNull(compilerJars, "compilerJars");
        Objects.requireNonNull(javaOptions, "javaOptions");
        options = List.copyOf(options);
        compilerJars = List.copyOf(compilerJars);
        javaOptions = List.copyOf(javaOptions);
      }
    }
  }
}
