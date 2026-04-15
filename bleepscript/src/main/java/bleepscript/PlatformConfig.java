package bleepscript;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

public sealed interface PlatformConfig
    permits PlatformConfig.Jvm, PlatformConfig.Js, PlatformConfig.Native {

  Optional<String> mainClass();

  record Jvm(
      Optional<String> mainClass,
      List<String> jvmOptions,
      List<String> jvmRuntimeOptions,
      Map<String, String> jvmEnvironment)
      implements PlatformConfig {
    public Jvm {
      Objects.requireNonNull(mainClass, "mainClass");
      Objects.requireNonNull(jvmOptions, "jvmOptions");
      Objects.requireNonNull(jvmRuntimeOptions, "jvmRuntimeOptions");
      Objects.requireNonNull(jvmEnvironment, "jvmEnvironment");
      jvmOptions = List.copyOf(jvmOptions);
      jvmRuntimeOptions = List.copyOf(jvmRuntimeOptions);
      jvmEnvironment = Map.copyOf(jvmEnvironment);
    }
  }

  record Js(
      Optional<String> mainClass,
      Optional<String> jsVersion,
      Optional<String> jsNodeVersion,
      Optional<Boolean> jsEmitSourceMaps)
      implements PlatformConfig {
    public Js {
      Objects.requireNonNull(mainClass, "mainClass");
      Objects.requireNonNull(jsVersion, "jsVersion");
      Objects.requireNonNull(jsNodeVersion, "jsNodeVersion");
      Objects.requireNonNull(jsEmitSourceMaps, "jsEmitSourceMaps");
    }
  }

  record Native(
      Optional<String> mainClass, Optional<String> nativeVersion, Optional<String> nativeGc)
      implements PlatformConfig {
    public Native {
      Objects.requireNonNull(mainClass, "mainClass");
      Objects.requireNonNull(nativeVersion, "nativeVersion");
      Objects.requireNonNull(nativeGc, "nativeGc");
    }
  }
}
