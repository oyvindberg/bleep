package bleep

import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Path}
import scala.collection.immutable.SortedMap

/** Unit tests for [[KspIncrementalState]] — the decision logic that picks between FullRebuild / CacheBust / Incremental given the current inputs vs. the
  * persisted manifest. Each test runs against a fresh tmp directory so manifest reads/writes touch the real filesystem.
  */
class KspIncrementalStateTest extends AnyFunSuite {

  /** Make a fresh tmp dir and a state-file path inside it. */
  private def freshState(): (Path, Path) = {
    val dir = Files.createTempDirectory("bleep-ksp-inc-")
    (dir, dir.resolve("inputs-manifest.json"))
  }

  /** Write a kt source under the given dir, return its path. */
  private def writeSrc(dir: Path, name: String, content: String): Path = {
    Files.createDirectories(dir)
    val p = dir.resolve(name)
    Files.writeString(p, content)
    p
  }

  /** Write a small file representing a "jar" — for fingerprinting we only care about mtime+size, not contents. */
  private def writeJar(dir: Path, name: String, bytes: String = "x"): Path = {
    Files.createDirectories(dir)
    val p = dir.resolve(name)
    Files.writeString(p, bytes)
    p
  }

  test("no prior state → FullRebuild") {
    val (root, stateFile) = freshState()
    val src = writeSrc(root, "Foo.kt", "package myapp")
    val inputs = KspIncrementalState.CurrentInputs(
      kspVersion = "1.0.32",
      kotlinVersion = "2.1.20",
      jdkHome = "/test/jdk",
      jvmTarget = "11",
      languageVersion = "1.9",
      apiVersion = "1.9",
      processorOptions = SortedMap.empty,
      processorJars = List(writeJar(root, "proc.jar")),
      libraries = List(writeJar(root, "moshi.jar")),
      sources = List(src)
    )
    assert(KspIncrementalState.decide(stateFile, inputs) === KspIncrementalState.Decision.FullRebuild)
  }

  test("save then decide on identical inputs → Incremental(empty, empty)") {
    val (root, stateFile) = freshState()
    val src = writeSrc(root, "Foo.kt", "package myapp")
    val inputs = KspIncrementalState.CurrentInputs(
      kspVersion = "1.0.32",
      kotlinVersion = "2.1.20",
      jdkHome = "/test/jdk",
      jvmTarget = "11",
      languageVersion = "1.9",
      apiVersion = "1.9",
      processorOptions = SortedMap.empty,
      processorJars = List(writeJar(root, "proc.jar")),
      libraries = List(writeJar(root, "moshi.jar")),
      sources = List(src)
    )
    KspIncrementalState.save(stateFile, inputs)
    val d = KspIncrementalState.decide(stateFile, inputs)
    assert(d === KspIncrementalState.Decision.Incremental(Nil, Nil))
  }

  test("save, then edit a source → Incremental(modified=[that source])") {
    val (root, stateFile) = freshState()
    val src = writeSrc(root, "Foo.kt", "package myapp; val x = 1")
    val inputs = KspIncrementalState.CurrentInputs(
      kspVersion = "1.0.32",
      kotlinVersion = "2.1.20",
      jdkHome = "/test/jdk",
      jvmTarget = "11",
      languageVersion = "1.9",
      apiVersion = "1.9",
      processorOptions = SortedMap.empty,
      processorJars = List(writeJar(root, "proc.jar")),
      libraries = List(writeJar(root, "moshi.jar")),
      sources = List(src)
    )
    KspIncrementalState.save(stateFile, inputs)

    // Mutate the source file.
    Files.writeString(src, "package myapp; val x = 2")
    val d = KspIncrementalState.decide(stateFile, inputs)
    assert(d === KspIncrementalState.Decision.Incremental(modifiedSources = List(src), removedSources = Nil))
  }

  test("save, then add a new source → Incremental(modified=[new source])") {
    val (root, stateFile) = freshState()
    val src1 = writeSrc(root, "Foo.kt", "package myapp")
    val priorInputs = KspIncrementalState.CurrentInputs(
      kspVersion = "1.0.32",
      kotlinVersion = "2.1.20",
      jdkHome = "/test/jdk",
      jvmTarget = "11",
      languageVersion = "1.9",
      apiVersion = "1.9",
      processorOptions = SortedMap.empty,
      processorJars = List(writeJar(root, "proc.jar")),
      libraries = List(writeJar(root, "moshi.jar")),
      sources = List(src1)
    )
    KspIncrementalState.save(stateFile, priorInputs)

    val src2 = writeSrc(root, "Bar.kt", "package myapp")
    val nextInputs = priorInputs.copy(sources = List(src1, src2))
    val d = KspIncrementalState.decide(stateFile, nextInputs)
    assert(d === KspIncrementalState.Decision.Incremental(modifiedSources = List(src2), removedSources = Nil))
  }

  test("save, then remove a source → Incremental(removed=[that source])") {
    val (root, stateFile) = freshState()
    val src1 = writeSrc(root, "Foo.kt", "package myapp")
    val src2 = writeSrc(root, "Bar.kt", "package myapp")
    val priorInputs = KspIncrementalState.CurrentInputs(
      kspVersion = "1.0.32",
      kotlinVersion = "2.1.20",
      jdkHome = "/test/jdk",
      jvmTarget = "11",
      languageVersion = "1.9",
      apiVersion = "1.9",
      processorOptions = SortedMap.empty,
      processorJars = List(writeJar(root, "proc.jar")),
      libraries = List(writeJar(root, "moshi.jar")),
      sources = List(src1, src2)
    )
    KspIncrementalState.save(stateFile, priorInputs)

    val nextInputs = priorInputs.copy(sources = List(src1))
    val d = KspIncrementalState.decide(stateFile, nextInputs)
    assert(d === KspIncrementalState.Decision.Incremental(modifiedSources = Nil, removedSources = List(src2)))
  }

  test("save, then change processor option → CacheBust") {
    val (root, stateFile) = freshState()
    val src = writeSrc(root, "Foo.kt", "package myapp")
    val priorInputs = KspIncrementalState.CurrentInputs(
      kspVersion = "1.0.32",
      kotlinVersion = "2.1.20",
      jdkHome = "/test/jdk",
      jvmTarget = "11",
      languageVersion = "1.9",
      apiVersion = "1.9",
      processorOptions = SortedMap("k" -> "v1"),
      processorJars = List(writeJar(root, "proc.jar")),
      libraries = List(writeJar(root, "moshi.jar")),
      sources = List(src)
    )
    KspIncrementalState.save(stateFile, priorInputs)

    val nextInputs = priorInputs.copy(processorOptions = SortedMap("k" -> "v2"))
    val d = KspIncrementalState.decide(stateFile, nextInputs)
    assert(d === KspIncrementalState.Decision.CacheBust)
  }

  test("save, then change KSP version → CacheBust") {
    val (root, stateFile) = freshState()
    val src = writeSrc(root, "Foo.kt", "package myapp")
    val priorInputs = KspIncrementalState.CurrentInputs(
      kspVersion = "1.0.32",
      kotlinVersion = "2.1.20",
      jdkHome = "/test/jdk",
      jvmTarget = "11",
      languageVersion = "1.9",
      apiVersion = "1.9",
      processorOptions = SortedMap.empty,
      processorJars = List(writeJar(root, "proc.jar")),
      libraries = List(writeJar(root, "moshi.jar")),
      sources = List(src)
    )
    KspIncrementalState.save(stateFile, priorInputs)

    val nextInputs = priorInputs.copy(kspVersion = "1.0.33")
    val d = KspIncrementalState.decide(stateFile, nextInputs)
    assert(d === KspIncrementalState.Decision.CacheBust)
  }

  test("save, then add a processor jar → CacheBust (fingerprint changed)") {
    val (root, stateFile) = freshState()
    val src = writeSrc(root, "Foo.kt", "package myapp")
    val procA = writeJar(root, "proc.jar")
    val priorInputs = KspIncrementalState.CurrentInputs(
      kspVersion = "1.0.32",
      kotlinVersion = "2.1.20",
      jdkHome = "/test/jdk",
      jvmTarget = "11",
      languageVersion = "1.9",
      apiVersion = "1.9",
      processorOptions = SortedMap.empty,
      processorJars = List(procA),
      libraries = List(writeJar(root, "moshi.jar")),
      sources = List(src)
    )
    KspIncrementalState.save(stateFile, priorInputs)

    val procB = writeJar(root, "proc2.jar")
    val nextInputs = priorInputs.copy(processorJars = List(procA, procB))
    val d = KspIncrementalState.decide(stateFile, nextInputs)
    assert(d === KspIncrementalState.Decision.CacheBust)
  }

  test("save, then change a library jar's content (mtime/size) → CacheBust") {
    val (root, stateFile) = freshState()
    val src = writeSrc(root, "Foo.kt", "package myapp")
    val libJar = writeJar(root, "moshi.jar", "v1")
    val priorInputs = KspIncrementalState.CurrentInputs(
      kspVersion = "1.0.32",
      kotlinVersion = "2.1.20",
      jdkHome = "/test/jdk",
      jvmTarget = "11",
      languageVersion = "1.9",
      apiVersion = "1.9",
      processorOptions = SortedMap.empty,
      processorJars = List(writeJar(root, "proc.jar")),
      libraries = List(libJar),
      sources = List(src)
    )
    KspIncrementalState.save(stateFile, priorInputs)

    // Mutate the lib's content so size differs. mtime resolution can be 1s, but size differs reliably.
    Files.writeString(libJar, "v2 with different size")
    val d = KspIncrementalState.decide(stateFile, priorInputs)
    assert(d === KspIncrementalState.Decision.CacheBust)
  }

  test("schema version mismatch → FullRebuild") {
    val (root, stateFile) = freshState()
    // Write a manifest with an old schema version.
    Files.writeString(
      stateFile,
      """{"schemaVersion":0,"kspVersion":"1.0.32","kotlinVersion":"2.1.20","processorOptions":{},"processorJarFingerprint":"x","librariesFingerprint":"y","sources":{}}"""
    )
    val src = writeSrc(root, "Foo.kt", "package myapp")
    val inputs = KspIncrementalState.CurrentInputs(
      kspVersion = "1.0.32",
      kotlinVersion = "2.1.20",
      jdkHome = "/test/jdk",
      jvmTarget = "11",
      languageVersion = "1.9",
      apiVersion = "1.9",
      processorOptions = SortedMap.empty,
      processorJars = List(writeJar(root, "proc.jar")),
      libraries = List(writeJar(root, "moshi.jar")),
      sources = List(src)
    )
    assert(KspIncrementalState.decide(stateFile, inputs) === KspIncrementalState.Decision.FullRebuild)
  }

  test("corrupt manifest → FullRebuild (best-effort recovery)") {
    val (root, stateFile) = freshState()
    Files.writeString(stateFile, "{not valid json")
    val src = writeSrc(root, "Foo.kt", "package myapp")
    val inputs = KspIncrementalState.CurrentInputs(
      kspVersion = "1.0.32",
      kotlinVersion = "2.1.20",
      jdkHome = "/test/jdk",
      jvmTarget = "11",
      languageVersion = "1.9",
      apiVersion = "1.9",
      processorOptions = SortedMap.empty,
      processorJars = List(writeJar(root, "proc.jar")),
      libraries = List(writeJar(root, "moshi.jar")),
      sources = List(src)
    )
    assert(KspIncrementalState.decide(stateFile, inputs) === KspIncrementalState.Decision.FullRebuild)
  }

  test("listSources: recursively walks .kt and .java; skips other extensions") {
    val (root, _) = freshState()
    val srcDir = root.resolve("src/kotlin")
    val javaDir = root.resolve("src/java")
    val nestedKt = writeSrc(srcDir.resolve("foo"), "Foo.kt", "package myapp")
    val nestedJava = writeSrc(javaDir.resolve("bar"), "Bar.java", "package myapp;")
    val ignored = writeSrc(srcDir, "notes.txt", "ignore me")
    val classFile = writeSrc(srcDir, "Foo.class", "fake class file")

    val found = KspIncrementalState.listSources(List(srcDir, javaDir))
    assert(found.contains(nestedKt))
    assert(found.contains(nestedJava))
    assert(!found.contains(ignored))
    assert(!found.contains(classFile))
  }

  test("manifest field set is pinned (forces deliberate SchemaVersion bump on changes)") {
    // If you're seeing this test fail because you added or removed a field on `KspIncrementalState`, that's the schema mismatch this test is supposed to catch.
    // The action you want is to (1) bump `KspIncrementalState.SchemaVersion`, then (2) update the expected set below. The bump makes existing on-disk manifests
    // self-invalidate to FullRebuild, which is the safe behaviour for an incompatible field-set change.
    val expectedFields = Set(
      "schemaVersion",
      "kspVersion",
      "kotlinVersion",
      "jdkHome",
      "jvmTarget",
      "languageVersion",
      "apiVersion",
      "processorOptions",
      "processorJarFingerprint",
      "librariesFingerprint",
      "sources"
    )
    val actualFields = classOf[KspIncrementalState].getDeclaredFields.iterator.map(_.getName).toSet
    assert(
      actualFields === expectedFields,
      s"KspIncrementalState fields changed. If this is deliberate, bump SchemaVersion and update this test. " +
        s"missing=${expectedFields -- actualFields}, extra=${actualFields -- expectedFields}"
    )
  }

  test("save manifest is JSON-deterministic for stable inputs (round-trip)") {
    val (root, stateFile) = freshState()
    val src = writeSrc(root, "Foo.kt", "package myapp")
    val inputs = KspIncrementalState.CurrentInputs(
      kspVersion = "1.0.32",
      kotlinVersion = "2.1.20",
      jdkHome = "/test/jdk",
      jvmTarget = "11",
      languageVersion = "1.9",
      apiVersion = "1.9",
      processorOptions = SortedMap("b" -> "2", "a" -> "1"),
      processorJars = List(writeJar(root, "proc.jar")),
      libraries = List(writeJar(root, "moshi.jar")),
      sources = List(src)
    )
    KspIncrementalState.save(stateFile, inputs)
    val first = new String(Files.readAllBytes(stateFile), java.nio.charset.StandardCharsets.UTF_8)
    KspIncrementalState.save(stateFile, inputs)
    val second = new String(Files.readAllBytes(stateFile), java.nio.charset.StandardCharsets.UTF_8)
    assert(first === second, "expected manifest output to be byte-identical on repeated saves with same inputs")
  }
}
