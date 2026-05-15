package bleep

import org.scalatest.funsuite.AnyFunSuite

import java.io.File
import java.nio.file.{Path, Paths}
import scala.collection.immutable.SortedMap

/** Unit tests for the pure parts of [[SymbolProcessorResolver]] — the `KSPJvmMain` argv layout, the incremental decision encoded in args, and the service-file
  * scanner — without going to Coursier or spawning the runner JVM.
  */
class SymbolProcessorResolverTest extends AnyFunSuite {

  private val processorA: Path = Paths.get("/repo/com/example/proc-a.jar")
  private val processorB: Path = Paths.get("/repo/com/example/proc-b.jar")
  private val runnerJarA: Path = Paths.get("/repo/com/google/devtools/ksp/symbol-processing-aa-embeddable-2.1.20-1.0.32.jar")
  private val runnerJarB: Path = Paths.get("/repo/com/google/devtools/ksp/symbol-processing-api-2.1.20-1.0.32.jar")
  private val libA: Path = Paths.get("/repo/com/squareup/moshi/moshi-1.15.0.jar")
  private val libB: Path = Paths.get("/build/.bleep/bloop/upstream-project/classes")

  private def result(
      processors: List[Path] = List(processorA, processorB),
      options: SortedMap[String, String] = SortedMap.empty
  ): SymbolProcessorResult =
    SymbolProcessorResult(
      runnerClasspath = List(runnerJarA, runnerJarB),
      processorJars = processors,
      librariesClasspath = List(libA, libB),
      sourceRoots = List(Paths.get("/ws/myapp/src/main/kotlin")),
      javaSourceRoots = List(Paths.get("/ws/myapp/src/main/java")),
      moduleName = "myapp",
      jvmTarget = "21",
      languageVersion = "2.0",
      apiVersion = "2.0",
      jdkHome = Paths.get("/jdk"),
      projectBaseDir = Paths.get("/ws/myapp"),
      kspOutputBaseDir = Paths.get("/ws/.bleep/projects/myapp/generated-sources/ksp"),
      kotlinOutputDir = Paths.get("/ws/.bleep/projects/myapp/generated-sources/ksp/kotlin"),
      javaOutputDir = Paths.get("/ws/.bleep/projects/myapp/generated-sources/ksp/java"),
      classOutputDir = Paths.get("/ws/.bleep/projects/myapp/builds/normal/ksp/classes"),
      resourceOutputDir = Paths.get("/ws/.bleep/projects/myapp/generated-sources/ksp/resources"),
      cachesDir = Paths.get("/ws/.bleep/projects/myapp/builds/normal/ksp/caches"),
      processorOptions = options
    )

  test("runnerArgs (FullRebuild): incremental=false, no modified/removed lists") {
    val r = result(processors = List(processorA, processorB), options = SortedMap("k1" -> "v1"))
    val args = r.runnerArgs(KspIncrementalState.Decision.FullRebuild)
    val sep = File.pathSeparator

    assert(args.contains("-module-name=myapp"))
    assert(args.contains(s"-libraries=${libA.toString}${sep}${libB.toString}"))
    assert(args.contains("-incremental=false"))
    assert(!args.exists(_.startsWith("-modified-sources=")))
    assert(!args.exists(_.startsWith("-removed-sources=")))
    assert(args.contains("-processor-options=k1=v1"))
    assert(args.last === s"${processorA.toString}${sep}${processorB.toString}")
  }

  test("runnerArgs (CacheBust): incremental=false (cache will be wiped by caller separately)") {
    val args = result().runnerArgs(KspIncrementalState.Decision.CacheBust)
    assert(args.contains("-incremental=false"))
    assert(!args.exists(_.startsWith("-modified-sources=")))
    assert(!args.exists(_.startsWith("-removed-sources=")))
  }

  test("runnerArgs (Incremental, empty deltas): incremental=true, no mod/rem args (no-op KSP run)") {
    val args = result().runnerArgs(KspIncrementalState.Decision.Incremental(Nil, Nil))
    assert(args.contains("-incremental=true"))
    assert(!args.exists(_.startsWith("-modified-sources=")))
    assert(!args.exists(_.startsWith("-removed-sources=")))
  }

  test("runnerArgs (Incremental, with deltas): -modified-sources and -removed-sources are pathsep-joined") {
    val modA = Paths.get("/ws/myapp/src/main/kotlin/Foo.kt")
    val modB = Paths.get("/ws/myapp/src/main/kotlin/Bar.kt")
    val rem = Paths.get("/ws/myapp/src/main/kotlin/Old.kt")
    val args = result().runnerArgs(KspIncrementalState.Decision.Incremental(List(modA, modB), List(rem)))
    val sep = File.pathSeparator
    assert(args.contains("-incremental=true"))
    assert(args.contains(s"-modified-sources=${modA.toString}${sep}${modB.toString}"))
    assert(args.contains(s"-removed-sources=${rem.toString}"))
  }

  test("runnerArgs: empty options → no -processor-options arg") {
    val args = result(options = SortedMap.empty).runnerArgs(KspIncrementalState.Decision.FullRebuild)
    assert(!args.exists(_.startsWith("-processor-options=")))
  }

  test("symbolProcessorProviderClasses: missing service file → None") {
    val tmp = java.nio.file.Files.createTempFile("bleep-ksp-test-", ".jar")
    try {
      val zip = new java.util.zip.ZipOutputStream(java.nio.file.Files.newOutputStream(tmp))
      try {
        zip.putNextEntry(new java.util.zip.ZipEntry("META-INF/MANIFEST.MF"))
        zip.write("Manifest-Version: 1.0\n".getBytes("UTF-8"))
        zip.closeEntry()
      } finally zip.close()
      assert(SymbolProcessorResolver.symbolProcessorProviderClasses(tmp) === None)
    } finally java.nio.file.Files.deleteIfExists(tmp)
  }

  test("symbolProcessorProviderClasses: present service file → parsed provider class names") {
    val tmp = java.nio.file.Files.createTempFile("bleep-ksp-test-", ".jar")
    try {
      val zip = new java.util.zip.ZipOutputStream(java.nio.file.Files.newOutputStream(tmp))
      try {
        zip.putNextEntry(new java.util.zip.ZipEntry("META-INF/services/com.google.devtools.ksp.processing.SymbolProcessorProvider"))
        zip.write("# leading comment\ncom.example.AlphaProcessorProvider\ncom.example.BetaProcessorProvider # trailing\n\n".getBytes("UTF-8"))
        zip.closeEntry()
      } finally zip.close()
      val classes = SymbolProcessorResolver.symbolProcessorProviderClasses(tmp)
      assert(classes === Some(List("com.example.AlphaProcessorProvider", "com.example.BetaProcessorProvider")))
    } finally java.nio.file.Files.deleteIfExists(tmp)
  }

  test("symbolProcessorProviderClasses: empty service file → Some(Nil)") {
    val tmp = java.nio.file.Files.createTempFile("bleep-ksp-test-", ".jar")
    try {
      val zip = new java.util.zip.ZipOutputStream(java.nio.file.Files.newOutputStream(tmp))
      try {
        zip.putNextEntry(new java.util.zip.ZipEntry("META-INF/services/com.google.devtools.ksp.processing.SymbolProcessorProvider"))
        zip.write("# only a comment\n\n".getBytes("UTF-8"))
        zip.closeEntry()
      } finally zip.close()
      assert(SymbolProcessorResolver.symbolProcessorProviderClasses(tmp) === Some(Nil))
    } finally java.nio.file.Files.deleteIfExists(tmp)
  }
}
