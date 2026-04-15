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
    List<String> testFrameworks) {

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
    sources = List.copyOf(sources);
    classpath = List.copyOf(classpath);
    dependencies = List.copyOf(dependencies);
    testFrameworks = List.copyOf(testFrameworks);
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
