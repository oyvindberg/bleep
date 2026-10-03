package bleep.machine

import java.io.IOException
import java.nio.file.{Files, Path, StandardCopyOption}
import java.security.MessageDigest

/** The JNI entry points of `bleep-core/src/c/bleep_machine.c`, the probes' native half on macOS and Windows.
  *
  * JNI rather than the FFM API because the probes run on the build's own JVM, which may be JDK 17: FFM is final only from 22. JNI rather than forking `vm_stat`
  * or `footprint` because the scheduler asks up to every 10 ms.
  *
  * An instance exists only once the library is loaded and its ABI version checked, so holding one is proof the methods can be called. Only the methods for the
  * platform the library was built for exist; calling another platform's is an `UnsatisfiedLinkError`.
  */
final class MachineNative private () {
  @native def abiVersion(): Int

  // macOS — see the C source for the layout of `out` and the statuses.
  @native def macHostPort(): Long
  @native def macSample(hostPort: Long, out: Array[Long]): Int
  @native def macFootprint(pid: Int): Long
}

object MachineNative {

  /** Must equal `BLEEP_MACHINE_ABI_VERSION` in the C source. */
  val AbiVersion: Int = 1

  /** Where the library for `platform` lives on the classpath (inside the bleep-core jar), as `(directory, file name)`. */
  def resourceFor(platform: ProbePlatform): (String, String) = platform match {
    case ProbePlatform.MacOsArm64 => ("bleep/machine/native/darwin-arm64", "libbleep-machine.dylib")
    case ProbePlatform.WindowsX64 => ("bleep/machine/native/windows-x86_64", "bleep-machine.dll")
    case ProbePlatform.Linux      => throw new IllegalArgumentException("Linux needs no native library: its probes read /proc")
  }

  /** Unpacks the library for `platform` from the classpath into `dir` and loads it.
    *
    * The file is named by its content hash, so it is written once per bleep version and never overwritten: several servers of different bleep versions share
    * the directory, and Windows cannot replace a DLL another process has loaded. Loading the same file twice in one JVM is a no-op, so there is nothing to
    * remember between calls.
    */
  def load(dir: Path, platform: ProbePlatform): MachineNative = {
    val (resourceDir, fileName) = resourceFor(platform)
    val resource = s"$resourceDir/$fileName"
    val in = classOf[MachineNative].getClassLoader.getResourceAsStream(resource)
    if (in == null)
      throw new IllegalStateException(
        s"This bleep has no native memory probe for $platform: `$resource` is not on the classpath. A bleep built from source builds it in bleep-core's " +
          "sourcegen step (bleep.scripts.BuildMachineNative), which needs a C compiler; a released bleep-core jar carries one for every supported platform."
      )
    val bytes =
      try in.readAllBytes()
      finally in.close()
    val hash = MessageDigest.getInstance("SHA-256").digest(bytes).take(12).map(b => f"${b & 0xff}%02x").mkString
    val target = dir.resolve(hash).resolve(fileName)

    if (!Files.exists(target)) {
      Files.createDirectories(target.getParent)
      val tmp = Files.createTempFile(target.getParent, fileName, ".tmp")
      Files.write(tmp, bytes): Unit
      try Files.move(tmp, target, StandardCopyOption.ATOMIC_MOVE): Unit
      catch {
        // Another process unpacked the same content first. On Windows the move cannot replace it if that process has already loaded it.
        case e: IOException =>
          Files.delete(tmp)
          if (!Files.exists(target)) throw e
      }
    }
    if (!java.util.Arrays.equals(Files.readAllBytes(target), bytes))
      throw new IllegalStateException(s"$target does not hold the native library it is named for; delete it and bleep will unpack it again")

    System.load(target.toAbsolutePath.toString)
    val native = new MachineNative
    val loaded = native.abiVersion()
    if (loaded != AbiVersion) throw new IllegalStateException(s"$target has native ABI version $loaded, this bleep expects $AbiVersion")
    native
  }
}
