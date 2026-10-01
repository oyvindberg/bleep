package bleep.quarkus;

import io.quarkus.bootstrap.app.CuratedApplication;
import io.quarkus.bootstrap.app.QuarkusBootstrap;
import io.quarkus.bootstrap.model.ApplicationModel;
import io.quarkus.bootstrap.workspace.SourceDir;
import io.quarkus.maven.dependency.ResolvedDependency;
import io.quarkus.paths.PathList;
import java.io.Closeable;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CountDownLatch;

/**
 * Dev-mode launcher for a bleep-built Quarkus application. Does what {@code IDELauncherImpl.launch}
 * does — bootstrap in DEV mode, then hand off to {@code io.quarkus.deployment.dev.IDEDevModeMain}
 * inside the augmentation classloader — minus the {@code BuildToolHelper.getProjectDir} probe that
 * makes the stock path throw outside a Maven/Gradle checkout.
 *
 * <p>Because the model's workspace module carries bleep's real source directories, Quarkus dev
 * mode's own recompilation (its {@code CompilationProvider}s invoke javac/kotlinc directly, no
 * build tool involved) gives working live reload.
 *
 * <p>Usage: {@code QuarkusDevRun <serialized-model> <app-args...>}. The application model file is
 * produced by {@link QuarkusModelWriter} for the same Quarkus version this JVM runs.
 */
public final class QuarkusDevRun {
  private QuarkusDevRun() {}

  public static void main(String[] args) throws Exception {
    if (args.length < 1) {
      throw new IllegalArgumentException("usage: QuarkusDevRun <serialized-model> <app-args...>");
    }
    Path modelFile = Path.of(args[0]);
    String[] appArgs = new String[args.length - 1];
    System.arraycopy(args, 1, appArgs, 0, appArgs.length);

    ApplicationModel model = QuarkusCompat.deserialize(modelFile);

    PathList.Builder applicationRoot = PathList.builder();
    for (SourceDir dir : model.getApplicationModule().getMainSources().getSourceDirs()) {
      if (Files.exists(dir.getOutputDir()) && !applicationRoot.contains(dir.getOutputDir())) {
        applicationRoot.add(dir.getOutputDir());
      }
    }
    for (SourceDir dir : model.getApplicationModule().getMainSources().getResourceDirs()) {
      if (Files.exists(dir.getOutputDir()) && !applicationRoot.contains(dir.getOutputDir())) {
        applicationRoot.add(dir.getOutputDir());
      }
    }

    // The base classloader is the one Quarkus uses as the parent of both the augment and
    // base-runtime QuarkusClassLoaders. Parent-first artifacts — notably
    // quarkus-development-mode-spi
    // (the io.quarkus.dev.spi SPI types: HotReplacementSetup et al.) — are loaded from it directly:
    // if such a type is absent here each QuarkusClassLoader defines its own copy and the two are
    // mutually "not a subtype", so the ServiceLoader<HotReplacementSetup> lookup in
    // IsolatedDevModeMain dies with a ServiceConfigurationError. But a parent-first class also
    // resolves ITS references against this classloader (e.g. wildfly-elytron-base is parent-first
    // and reaches for org.wildfly.common.Assert, which is not), so exposing only the parent-first
    // jars is not enough — the whole external library classpath has to be here, exactly as a real
    // IDE/Maven dev launch provides it on its launching classpath. bleep's runner classpath is
    // deliberately minimal, so build the base classloader from every external (non-workspace)
    // dependency jar in the model. The app's own workspace classes stay off it so they remain
    // reloadable in the isolated runtime classloader. Delegation between base and isolated copies
    // is
    // still governed by each dependency's parent-first flag; this only controls what is reachable.
    List<URL> baseJars = new ArrayList<>();
    for (ResolvedDependency dep : model.getDependencies()) {
      if (dep.isWorkspaceModule()) {
        continue;
      }
      for (Path p : dep.getResolvedPaths()) {
        baseJars.add(p.toUri().toURL());
      }
    }
    ClassLoader baseClassLoader =
        baseJars.isEmpty()
            ? QuarkusDevRun.class.getClassLoader()
            : new URLClassLoader(
                baseJars.toArray(new URL[0]), QuarkusDevRun.class.getClassLoader());

    Path buildDir = model.getApplicationModule().getBuildDir().toPath();
    CuratedApplication curated =
        QuarkusBootstrap.builder()
            .setExistingModel(model)
            .setBaseClassLoader(baseClassLoader)
            .setIsolateDeployment(true)
            .setMode(QuarkusBootstrap.Mode.DEV)
            .setApplicationRoot(applicationRoot.build())
            .setProjectRoot(model.getApplicationModule().getModuleDir().toPath())
            .setTargetDirectory(buildDir)
            .build()
            .bootstrap();

    Map<String, Object> context = new HashMap<>();
    context.put(
        "app-classes",
        model
            .getApplicationModule()
            .getMainSources()
            .getSourceDirs()
            .iterator()
            .next()
            .getOutputDir());
    context.put("args", appArgs);

    Object app =
        curated.runInAugmentClassLoader("io.quarkus.deployment.dev.IDEDevModeMain", context);

    CountDownLatch shutdown = new CountDownLatch(1);
    Runtime.getRuntime()
        .addShutdownHook(
            new Thread(
                () -> {
                  try {
                    if (app instanceof Closeable c) c.close();
                    curated.close();
                  } catch (Exception e) {
                    e.printStackTrace();
                  } finally {
                    shutdown.countDown();
                  }
                },
                "bleep-quarkus-dev-shutdown"));
    // IDEDevModeMain runs the application on background threads; keep the main thread parked so the
    // JVM's lifetime is the application's lifetime.
    shutdown.await();
  }
}
