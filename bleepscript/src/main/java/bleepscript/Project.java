package bleepscript;

import java.util.Objects;
import java.util.Optional;
import java.util.Set;

public record Project(
    CrossProjectName crossName,
    Set<Dep> dependencies,
    Set<String> dependsOn,
    Set<RelPath> sources,
    Set<RelPath> resources,
    Optional<ScalaConfig> scala,
    Optional<JavaConfig> java,
    Optional<KotlinConfig> kotlin,
    Optional<PlatformConfig> platform,
    boolean isTestProject,
    Set<String> testFrameworks,
    Set<ScriptDef> sourcegen,
    Optional<PublishConfig> publish) {

  public Project {
    Objects.requireNonNull(crossName, "crossName");
    Objects.requireNonNull(dependencies, "dependencies");
    Objects.requireNonNull(dependsOn, "dependsOn");
    Objects.requireNonNull(sources, "sources");
    Objects.requireNonNull(resources, "resources");
    Objects.requireNonNull(scala, "scala");
    Objects.requireNonNull(java, "java");
    Objects.requireNonNull(kotlin, "kotlin");
    Objects.requireNonNull(platform, "platform");
    Objects.requireNonNull(testFrameworks, "testFrameworks");
    Objects.requireNonNull(sourcegen, "sourcegen");
    Objects.requireNonNull(publish, "publish");
    dependencies = Set.copyOf(dependencies);
    dependsOn = Set.copyOf(dependsOn);
    sources = Set.copyOf(sources);
    resources = Set.copyOf(resources);
    testFrameworks = Set.copyOf(testFrameworks);
    sourcegen = Set.copyOf(sourcegen);
  }
}
