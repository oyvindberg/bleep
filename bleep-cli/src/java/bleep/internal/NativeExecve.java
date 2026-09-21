package bleep.internal;

import org.graalvm.nativeimage.Platform;
import org.graalvm.nativeimage.Platforms;
import org.graalvm.nativeimage.c.function.CFunction;
import org.graalvm.nativeimage.c.type.CCharPointer;
import org.graalvm.nativeimage.c.type.CCharPointerPointer;
import org.graalvm.nativeimage.c.type.CIntPointer;
import org.graalvm.nativeimage.c.type.CTypeConversion;

/**
 * Replace the running native image with another program, for the launcher that hands off to the
 * bleep release a build file asks for.
 *
 * <p>This used to be coursier's {@code coursier.jvm.Execve}. From coursier-exec 2.1.25-M26 that
 * runs {@code com.oracle.svm.core.jdk.RuntimeSupport} before the exec, from a plain classpath
 * class. GraalVM 25 exports that package only to its own modules, so native-image leaves the
 * reference unresolved and the image throws {@code NoClassDefFoundError} at the call. coursier's
 * fallback catches {@code Exception}, which that is not. Every release from M11 on crashed instead
 * of launching the version its build pinned.
 *
 * <p>So this uses only the public C interop API. It does not run shutdown hooks: the launcher
 * decides to exec before bleep has registered any, which is also how coursier-exec behaved until
 * M26. Only call it from a native image; on a JVM the {@code native} methods have no body.
 */
public final class NativeExecve {
  private NativeExecve() {}

  @Platforms({Platform.DARWIN.class, Platform.LINUX.class})
  @CFunction("execve")
  private static native int execve0(
      CCharPointer path, CCharPointerPointer argv, CCharPointerPointer envp);

  @Platforms({Platform.DARWIN.class, Platform.LINUX.class})
  @CFunction("strerror")
  private static native CCharPointer strerror(int errnum);

  @Platforms(Platform.DARWIN.class)
  @CFunction("__error")
  private static native CIntPointer darwinErrno();

  @Platforms(Platform.LINUX.class)
  @CFunction("__errno_location")
  private static native CIntPointer linuxErrno();

  /**
   * Does not return: either the process image is replaced by {@code path}, or this throws with the
   * reason the kernel gave.
   *
   * @param argv the full argument vector, including {@code argv[0]}
   * @param env {@code KEY=VALUE} entries
   */
  public static void execve(String path, String[] argv, String[] env) {
    // `Platform.includedIn` is folded when the image is built, so on Windows everything below is
    // dropped and the POSIX-only functions are never linked. A check that is only decided at run
    // time, like the launcher's OS test, still leaves them reachable and fails the Windows build.
    if (!Platform.includedIn(Platform.DARWIN.class) && !Platform.includedIn(Platform.LINUX.class)) {
      throw new UnsupportedOperationException("execve is only available on macOS and Linux");
    }
    // Whatever is buffered belongs to this process and would be lost with its image.
    System.out.flush();
    System.err.flush();
    try (CTypeConversion.CCharPointerHolder path0 = CTypeConversion.toCString(path);
        CTypeConversion.CCharPointerPointerHolder argv0 = CTypeConversion.toCStrings(argv);
        CTypeConversion.CCharPointerPointerHolder env0 = CTypeConversion.toCStrings(env)) {
      execve0(path0.get(), argv0.get(), env0.get());
    }
    // execve only returns on failure.
    int errno;
    if (Platform.includedIn(Platform.DARWIN.class)) {
      errno = darwinErrno().read();
    } else {
      errno = linuxErrno().read();
    }
    throw new IllegalStateException(
        "execve("
            + path
            + ") failed: "
            + CTypeConversion.toJavaString(strerror(errno))
            + " (errno "
            + errno
            + ")");
  }
}
