package bleep.quarkus;

import coursierapi.Fetch;
import coursierapi.FetchResult;
import coursierapi.MavenRepository;
import coursierapi.Module;
import coursierapi.Repository;
import coursierapi.ResolutionParams;
import io.quarkus.bootstrap.BootstrapConstants;
import io.quarkus.bootstrap.model.ApplicationModel;
import io.quarkus.bootstrap.model.ApplicationModelBuilder;
import io.quarkus.bootstrap.model.CapabilityContract;
import io.quarkus.bootstrap.workspace.ArtifactSources;
import io.quarkus.bootstrap.workspace.DefaultArtifactSources;
import io.quarkus.bootstrap.workspace.SourceDir;
import io.quarkus.bootstrap.workspace.WorkspaceModule;
import io.quarkus.bootstrap.workspace.WorkspaceModuleId;
import io.quarkus.maven.dependency.ArtifactCoords;
import io.quarkus.maven.dependency.ArtifactKey;
import io.quarkus.maven.dependency.DependencyFlags;
import io.quarkus.maven.dependency.ResolvedDependencyBuilder;
import io.quarkus.paths.PathList;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

/**
 * Builds a Quarkus {@link ApplicationModel} from a bleep {@link ModelSpec}. This is the same job
 * {@code GradleApplicationModelBuilder} does for Gradle: bleep supplies the resolved runtime
 * classpath and the workspace layout, and this class supplies what Quarkus derives on top of it —
 * extension detection from {@code META-INF/quarkus-extension.properties}, the conditional
 * dependency fixpoint, and the deployment classpath resolved at the runtime graph's versions.
 *
 * <p>Two deliberate simplifications against the Maven/Gradle builders, both verified against
 * current Quarkus before being made:
 *
 * <ul>
 *   <li>Every extension is flagged {@code TOP_LEVEL_RUNTIME_EXTENSION_ARTIFACT}. The precise "first
 *       extension on each root-to-leaf branch" computation needs the dependency tree, and the
 *       flag's only consumers are analytics, {@code ProjectStates} and the Maven dependency list
 *       mojo — nothing in augmentation or the test framework.
 *   <li>{@code PlatformImports} carries only the {@code quarkus-bom-quarkus-platform-properties}
 *       content (required: build-time config expands {@code platform.*} properties from it), not
 *       BOM alignment data — bleep builds import no BOMs, so there is nothing to validate.
 * </ul>
 */
public final class AppModelAssembler {

  /** One entry on the runtime classpath, after coordinates are settled. */
  private record RuntimeEntry(
      ModelSpec.Gav gav, List<Path> paths, boolean direct, Properties extensionProps) {}

  private final ModelSpec spec;
  private final List<Repository> repositories;

  /**
   * g:a -> version, for forcing deployment and conditional resolution onto the runtime graph's
   * versions.
   */
  private final Map<Module, String> forcedVersions = new HashMap<>();

  /** g:a keys present in the runtime graph; the domain the dependency-condition checks run over. */
  private final Set<String> presentKeys = new HashSet<>();

  private final List<RuntimeEntry> runtimeEntries = new ArrayList<>();

  public AppModelAssembler(ModelSpec spec) {
    this.spec = spec;
    List<Repository> repos = new ArrayList<>();
    repos.add(Repository.central());
    for (String url : spec.repos) repos.add(MavenRepository.of(url));
    this.repositories = repos;
  }

  private static long T0 = System.nanoTime();

  private static void phase(String name) {
    if (System.getenv("BLEEP_QUARKUS_TIMING") == null) return;
    long now = System.nanoTime();
    System.err.println(String.format("[timing] %-28s %6.2fs", name, (now - T0) / 1e9));
    T0 = now;
  }

  public ApplicationModel assemble() {
    T0 = System.nanoTime();
    Set<String> directKeys = new HashSet<>(spec.directDeps);

    for (ModelSpec.Dep dep : spec.deps) {
      Properties extProps = readExtensionDescriptor(dep.path());
      boolean direct = directKeys.contains(dep.gav().groupId() + ":" + dep.gav().artifactId());
      runtimeEntries.add(new RuntimeEntry(dep.gav(), List.of(dep.path()), direct, extProps));
      presentKeys.add(dep.gav().groupId() + ":" + dep.gav().artifactId());
      forcedVersions.put(
          Module.of(dep.gav().groupId(), dep.gav().artifactId()), dep.gav().version());
    }
    for (ModelSpec.ProjectDep pd : spec.projectDeps) {
      ModelSpec.Gav gav = new ModelSpec.Gav("bleep.workspace", pd.name(), "", "0.0.0-bleep");
      Properties extProps = readExtensionDescriptor(pd.paths().get(0));
      runtimeEntries.add(new RuntimeEntry(gav, pd.paths(), true, extProps));
      presentKeys.add(gav.groupId() + ":" + gav.artifactId());
    }
    phase("read-extension-descriptors");

    resolveConditionalDependencies();
    phase("conditional-dependencies");

    ApplicationModelBuilder builder = new ApplicationModelBuilder();
    ResolvedDependencyBuilder appArtifact = buildAppArtifact();
    builder.setAppArtifact(appArtifact);
    builder.addReloadableWorkspaceModule(appArtifact.getKey());
    builder.setPlatformImports(buildPlatformImports());
    phase("platform-imports");

    for (RuntimeEntry entry : runtimeEntries) {
      ResolvedDependencyBuilder dep =
          ResolvedDependencyBuilder.newInstance()
              .setGroupId(entry.gav().groupId())
              .setArtifactId(entry.gav().artifactId())
              .setClassifier(entry.gav().classifier())
              .setType(ArtifactCoords.TYPE_JAR)
              .setVersion(entry.gav().version())
              .setResolvedPaths(PathList.from(entry.paths()));
      dep.setRuntimeCp().setDeploymentCp();
      dep.setDirect(entry.direct());
      if (entry.extensionProps() != null) {
        dep.setRuntimeExtensionArtifact();
        dep.setFlags(DependencyFlags.TOP_LEVEL_RUNTIME_EXTENSION_ARTIFACT);
        QuarkusCompat.handleExtensionProperties(builder, entry.extensionProps(), dep.getKey());
        String provides =
            entry.extensionProps().getProperty(BootstrapConstants.PROP_PROVIDES_CAPABILITIES);
        if (provides != null) {
          String requires =
              entry.extensionProps().getProperty(BootstrapConstants.PROP_REQUIRES_CAPABILITIES);
          builder.addExtensionCapabilities(
              CapabilityContract.of(dep.toGACTVString(), provides, requires));
        }
      }
      builder.addDependency(dep);
    }

    phase("runtime-entries");
    addDeploymentDependencies(builder);
    phase("deployment-classpath");

    ApplicationModel m = builder.build();
    phase("build");
    return m;
  }

  /**
   * Quarkus's own build-time config references platform properties — e.g. {@code
   * quarkus.native.builder-image=${platform.quarkus.native.builder-image}} — which Maven and Gradle
   * feed in from the {@code quarkus-bom-quarkus-platform-properties} artifact of the imported
   * platform BOM. Bleep builds import no BOMs, so fetch that artifact directly at the resolved
   * quarkus-core version. Without it augmentation fails config validation, so its absence is an
   * error worth stopping on, not working around.
   */
  private io.quarkus.bootstrap.model.PlatformImportsImpl buildPlatformImports() {
    io.quarkus.bootstrap.model.PlatformImportsImpl platformImports =
        new io.quarkus.bootstrap.model.PlatformImportsImpl();
    String quarkusCoreVersion =
        runtimeEntries.stream()
            .filter(
                e ->
                    e.gav().groupId().equals("io.quarkus")
                        && e.gav().artifactId().equals("quarkus-core"))
            .map(e -> e.gav().version())
            .findFirst()
            .orElseThrow(
                () ->
                    new IllegalStateException(
                        "io.quarkus:quarkus-core is not on the runtime classpath"));

    final String artifactId = "quarkus-bom-quarkus-platform-properties";
    List<java.io.File> files;
    try {
      files =
          Fetch.create()
              .withRepositories(repositories.toArray(new Repository[0]))
              // "properties" is not among coursier's default artifact types, so it must be allowed
              // explicitly or the artifact resolves and is then silently filtered from the result.
              .addArtifactTypes("properties")
              .addDependencies(
                  coursierapi.Dependency.of("io.quarkus", artifactId, quarkusCoreVersion)
                      .withType("properties")
                      .withTransitive(false))
              .fetch();
    } catch (coursierapi.error.CoursierError e) {
      throw new IllegalStateException(
          "Failed to resolve io.quarkus:"
              + artifactId
              + ":"
              + quarkusCoreVersion
              + "; Quarkus augmentation needs the platform properties it carries",
          e);
    }
    if (files.size() != 1) {
      throw new IllegalStateException(
          "Expected exactly one file resolving io.quarkus:"
              + artifactId
              + ":"
              + quarkusCoreVersion
              + ", got "
              + files);
    }
    try {
      platformImports.addPlatformProperties(
          "io.quarkus", artifactId, null, "properties", quarkusCoreVersion, files.get(0).toPath());
    } catch (Exception e) {
      throw new IllegalStateException("Failed to load platform properties from " + files.get(0), e);
    }
    // Platform properties become lowest-priority build-time config defaults
    // (BuildTimeConfigurationReader.initConfiguration), so this rides inside the model instead of
    // being a jvmOption every project must declare: bleep projects appear in the model under the
    // synthesized bleep.workspace group, and this group-wide index-dependency makes Quarkus index
    // them all as application archives — the role jandex-maven-plugin output plays under Maven.
    // Without it, CDI beans and @RegisterRestClient interfaces in sibling projects silently don't
    // exist. Being a default, any explicit user config still wins.
    platformImports.setPlatformProperties(
        java.util.Map.of("quarkus.index-dependency.bleep-workspace.group-id", "bleep.workspace"));
    return platformImports;
  }

  private ResolvedDependencyBuilder buildAppArtifact() {
    WorkspaceModule.Mutable module =
        WorkspaceModule.builder()
            .setModuleId(WorkspaceModuleId.of(spec.appGroupId, spec.appArtifactId, spec.appVersion))
            .setModuleDir(spec.moduleDir)
            .setBuildDir(spec.buildDir)
            // bleep.yaml plays the role of pom.xml/build.gradle; the deserializer requires the
            // build-files collection to be present.
            .setBuildFile(spec.buildFile);

    // The MAIN classifier's content tree IS the application root as far as quarkus is concerned:
    // ResolvedDependency.getContentTree() prefers the workspace module's tree over resolvedPaths,
    // and AppMakerHelper adds those roots to the runtime classloader. Registering test dirs only
    // under the TEST classifier hides them from that tree — src/test/resources then never reaches
    // the runtime classloader, and quarkus misses the app's application.properties (maven never
    // hits this because it copies resources into target/test-classes). So in test mode the test
    // dirs fold into MAIN, test first, matching the shadowing order of the resolved paths.
    List<SourceDir> mainSources = new ArrayList<>();
    List<SourceDir> mainResources = new ArrayList<>();
    if (spec.testClasses != null) {
      mainSources.addAll(sourceDirs(spec.testSourceDirs, spec.testClasses));
      mainResources.addAll(selfServedResourceDirs(spec.testResourceDirs));
    }
    mainSources.addAll(sourceDirs(spec.mainSourceDirs, spec.mainClasses));
    mainResources.addAll(selfServedResourceDirs(spec.mainResourceDirs));
    module.addArtifactSources(
        new DefaultArtifactSources(ArtifactSources.MAIN, mainSources, mainResources));
    if (spec.testClasses != null) {
      module.addArtifactSources(
          new DefaultArtifactSources(
              ArtifactSources.TEST,
              sourceDirs(spec.testSourceDirs, spec.testClasses),
              selfServedResourceDirs(spec.testResourceDirs)));
    }

    // In test mode the test output joins the application artifact's paths, the way Maven's test
    // model does it: the whole application root (QuarkusClassLoader's view of "the app") is then
    // carried by the model alone, with test classes shadowing main ones. Resource dirs are bleep's
    // source-tree resource dirs served as-is.
    // The spec is the authority on which dirs belong: it already filtered configured-but-absent
    // source dirs, and workspace output dirs it lists may legitimately not exist while this writer
    // runs (sourcegen order) — they will by the time the model is read.
    List<Path> appPaths = new ArrayList<>();
    if (spec.testClasses != null) {
      appPaths.add(spec.testClasses);
      appPaths.addAll(spec.testResourceDirs);
    }
    appPaths.add(spec.mainClasses);
    appPaths.addAll(spec.mainResourceDirs);

    return ResolvedDependencyBuilder.newInstance()
        .setGroupId(spec.appGroupId)
        .setArtifactId(spec.appArtifactId)
        .setVersion(spec.appVersion)
        .setWorkspaceModule(module)
        .setReloadable()
        .setResolvedPaths(PathList.from(appPaths));
  }

  private static List<SourceDir> sourceDirs(List<Path> srcDirs, Path outputDir) {
    List<SourceDir> result = new ArrayList<>(srcDirs.size());
    for (Path src : srcDirs) result.add(SourceDir.of(src, outputDir));
    return result;
  }

  /**
   * bleep serves resources straight from the source tree; source dir and output dir are the same.
   */
  private static List<SourceDir> selfServedResourceDirs(List<Path> dirs) {
    List<SourceDir> result = new ArrayList<>(dirs.size());
    for (Path dir : dirs) result.add(SourceDir.of(dir, dir));
    return result;
  }

  /**
   * The conditional dependency fixpoint, same shape as Gradle's {@code
   * ConditionalDependenciesEnabler}: extensions may declare {@code conditional-dependencies} that
   * only join the graph when their own {@code dependency-condition} (a list of g:a keys) is already
   * satisfied by it. Adding one can satisfy another, so iterate until nothing changes.
   */
  private void resolveConditionalDependencies() {
    List<ArtifactCoords> pending = new ArrayList<>();
    for (RuntimeEntry entry : runtimeEntries) queueConditionalDeps(entry.extensionProps(), pending);

    boolean progressed = true;
    while (progressed && !pending.isEmpty()) {
      progressed = false;
      List<ArtifactCoords> stillPending = new ArrayList<>();
      for (ArtifactCoords coords : pending) {
        if (presentKeys.contains(coords.getGroupId() + ":" + coords.getArtifactId())) continue;
        List<Map.Entry<ModelSpec.Gav, Path>> resolved = fetch(List.of(coords), true);
        Map.Entry<ModelSpec.Gav, Path> self =
            resolved.stream()
                .filter(
                    e ->
                        e.getKey().groupId().equals(coords.getGroupId())
                            && e.getKey().artifactId().equals(coords.getArtifactId()))
                .findFirst()
                .orElseThrow(
                    () ->
                        new IllegalStateException(
                            "Conditional dependency " + coords + " resolved to no artifact"));
        Properties selfProps = readExtensionDescriptor(self.getValue());
        if (selfProps != null && !conditionSatisfied(selfProps)) {
          stillPending.add(coords);
          continue;
        }
        // Condition holds (or the artifact is not an extension): the conditional dependency and its
        // whole transitive closure join the runtime graph.
        progressed = true;
        for (Map.Entry<ModelSpec.Gav, Path> e : resolved) {
          String key = e.getKey().groupId() + ":" + e.getKey().artifactId();
          if (!presentKeys.add(key)) continue;
          Properties extProps = readExtensionDescriptor(e.getValue());
          runtimeEntries.add(new RuntimeEntry(e.getKey(), List.of(e.getValue()), false, extProps));
          forcedVersions.put(
              Module.of(e.getKey().groupId(), e.getKey().artifactId()), e.getKey().version());
          queueConditionalDeps(extProps, stillPending);
        }
      }
      pending = stillPending;
    }
  }

  private void queueConditionalDeps(Properties extensionProps, List<ArtifactCoords> pending) {
    if (extensionProps == null) return;
    addCoords(extensionProps.getProperty(BootstrapConstants.CONDITIONAL_DEPENDENCIES), pending);
    if (spec.mode == ModelSpec.Mode.DEV) {
      // The constant for this only exists in newer Quarkus; the property name itself is stable.
      addCoords(extensionProps.getProperty("conditional-dev-dependencies"), pending);
    }
  }

  private static void addCoords(String whitespaceSeparated, List<ArtifactCoords> target) {
    if (whitespaceSeparated == null) return;
    for (String s : whitespaceSeparated.split("\\s+")) {
      if (!s.isEmpty()) target.add(ArtifactCoords.fromString(s));
    }
  }

  private boolean conditionSatisfied(Properties extensionProps) {
    String condition = extensionProps.getProperty(BootstrapConstants.DEPENDENCY_CONDITION);
    if (condition == null) return true;
    for (String key : condition.split("\\s+")) {
      if (key.isEmpty()) continue;
      ArtifactKey artifactKey = ArtifactKey.fromString(key);
      if (!presentKeys.contains(artifactKey.getGroupId() + ":" + artifactKey.getArtifactId()))
        return false;
    }
    return true;
  }

  /**
   * Resolves every extension's {@code deployment-artifact} in one go, versions forced to the
   * runtime graph's, and adds whatever is not already a runtime dependency as {@code DEPLOYMENT_CP}
   * only. This is the classpath the augmentor runs on.
   */
  private void addDeploymentDependencies(ApplicationModelBuilder builder) {
    List<ArtifactCoords> deploymentCoords = new ArrayList<>();
    for (RuntimeEntry entry : runtimeEntries) {
      if (entry.extensionProps() == null) continue;
      String deploymentArtifact =
          entry.extensionProps().getProperty(BootstrapConstants.PROP_DEPLOYMENT_ARTIFACT);
      if (deploymentArtifact == null)
        throw new IllegalStateException(
            "Extension "
                + entry.gav()
                + " has a quarkus-extension.properties without "
                + BootstrapConstants.PROP_DEPLOYMENT_ARTIFACT);
      deploymentCoords.add(ArtifactCoords.fromString(deploymentArtifact));
    }
    if (deploymentCoords.isEmpty()) return;

    Set<String> runtimeKeys = new HashSet<>(presentKeys);
    // LinkedHashMap: first occurrence wins, order kept stable for the deployment classloader.
    Map<String, Map.Entry<ModelSpec.Gav, Path>> deploymentOnly = new LinkedHashMap<>();
    for (Map.Entry<ModelSpec.Gav, Path> e : fetch(deploymentCoords, true)) {
      String key = e.getKey().groupId() + ":" + e.getKey().artifactId();
      if (runtimeKeys.contains(key)) continue;
      deploymentOnly.putIfAbsent(key, e);
    }

    for (Map.Entry<ModelSpec.Gav, Path> e : deploymentOnly.values()) {
      builder.addDependency(
          ResolvedDependencyBuilder.newInstance()
              .setGroupId(e.getKey().groupId())
              .setArtifactId(e.getKey().artifactId())
              .setClassifier(e.getKey().classifier())
              .setType(ArtifactCoords.TYPE_JAR)
              .setVersion(e.getKey().version())
              .setResolvedPath(e.getValue())
              .setDeploymentCp());
    }
  }

  /** Resolve coords (transitively) against the spec's repositories, runtime versions forced. */
  private List<Map.Entry<ModelSpec.Gav, Path>> fetch(
      List<ArtifactCoords> coords, boolean forceRuntimeVersions) {
    Fetch fetch = Fetch.create().withRepositories(repositories.toArray(new Repository[0]));
    for (ArtifactCoords c : coords) {
      coursierapi.Dependency dep =
          coursierapi.Dependency.of(c.getGroupId(), c.getArtifactId(), c.getVersion());
      if (!c.getClassifier().isEmpty()) dep = dep.withClassifier(c.getClassifier());
      fetch.addDependencies(dep);
    }
    if (forceRuntimeVersions) {
      fetch.withResolutionParams(ResolutionParams.create().forceVersions(forcedVersions));
    }
    FetchResult result;
    try {
      result = fetch.fetchResult();
    } catch (coursierapi.error.CoursierError e) {
      throw new IllegalStateException(
          "Failed to resolve " + coords + " for the Quarkus application model", e);
    }
    List<String> repoBases = new ArrayList<>(repositories.size());
    for (Repository r : repositories) {
      if (r instanceof MavenRepository mr) repoBases.add(mr.getBase());
    }
    List<Map.Entry<ModelSpec.Gav, Path>> out = new ArrayList<>(result.getArtifacts().size());
    for (Map.Entry<coursierapi.Artifact, java.io.File> e : result.getArtifacts()) {
      out.add(Map.entry(gavFromMavenUrl(e.getKey().getUrl(), repoBases), e.getValue().toPath()));
    }
    return out;
  }

  /**
   * Recover coordinates from a Maven-layout artifact URL: {@code <repoBase>/<group/as/path>/
   * <artifact>/<version>/<artifact>-<version>[-<classifier>].<ext>}. The repo bases are the ones
   * this class configured on the fetch, so stripping the matching base leaves exactly the maven
   * layout path — no heuristics.
   */
  static ModelSpec.Gav gavFromMavenUrl(String url, List<String> repoBases) {
    String layoutPath = null;
    for (String base : repoBases) {
      String normalized = base.endsWith("/") ? base : base + "/";
      if (url.startsWith(normalized)) {
        layoutPath = url.substring(normalized.length());
        break;
      }
    }
    if (layoutPath == null)
      throw new IllegalArgumentException(
          "Artifact url " + url + " is not under any configured repository: " + repoBases);
    String[] segments = layoutPath.split("/");
    if (segments.length < 4)
      throw new IllegalArgumentException(
          "Cannot derive maven coordinates from artifact url " + url);
    String file = segments[segments.length - 1];
    String version = segments[segments.length - 2];
    String artifactId = segments[segments.length - 3];
    String expectedPrefix = artifactId + "-" + version;
    if (!file.startsWith(expectedPrefix))
      throw new IllegalArgumentException(
          "Artifact file " + file + " does not match " + expectedPrefix + " from url " + url);
    String remainder = file.substring(expectedPrefix.length());
    String classifier = "";
    if (remainder.startsWith("-")) {
      int dot = remainder.lastIndexOf('.');
      classifier = remainder.substring(1, dot);
    }
    String groupId =
        String.join(".", java.util.Arrays.asList(segments).subList(0, segments.length - 3));
    return new ModelSpec.Gav(groupId, artifactId, classifier, version);
  }

  /**
   * Reads {@code META-INF/quarkus-extension.properties} from a jar or classes directory; null when
   * absent.
   */
  static Properties readExtensionDescriptor(Path artifact) {
    if (Files.isDirectory(artifact)) {
      Path descriptor = artifact.resolve(BootstrapConstants.DESCRIPTOR_PATH);
      if (!Files.exists(descriptor)) return null;
      Properties props = new Properties();
      try (InputStream in = Files.newInputStream(descriptor)) {
        props.load(in);
      } catch (IOException e) {
        throw new IllegalStateException("Failed to read " + descriptor, e);
      }
      return props;
    }
    if (!artifact.getFileName().toString().endsWith(".jar")) return null;
    try (ZipFile zip = new ZipFile(artifact.toFile())) {
      ZipEntry entry = zip.getEntry(BootstrapConstants.DESCRIPTOR_PATH);
      if (entry == null) return null;
      Properties props = new Properties();
      try (InputStream in = zip.getInputStream(entry)) {
        props.load(in);
      }
      return props;
    } catch (IOException e) {
      throw new IllegalStateException("Failed to read " + artifact, e);
    }
  }
}
