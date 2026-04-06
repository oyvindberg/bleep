package bleep.analysis

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}

/** Unit tests for the portable zinc analysis mappers (sbt 2 style).
  *
  * Tests that:
  *   - PlainVirtualFile produces machine-independent IDs with marker prefixes
  *   - Path mappers correctly relativize and absolutize
  *   - Round-tripping through write+read mappers recovers original paths
  *   - Backward compatibility: absolute paths without markers pass through unchanged
  */
class PortableAnalysisMappersTest extends AnyFunSuite with Matchers {

  private def withTempBuildDir(testName: String)(f: Path => Unit): Unit = {
    val dir = Files.createTempDirectory(s"bleep-test-$testName-")
    try {
      Files.createDirectories(dir.resolve(".bleep"))
      f(dir)
    } finally deleteRecursively(dir)
  }

  private def deleteRecursively(path: Path): Unit =
    if (Files.exists(path)) {
      if (Files.isDirectory(path)) {
        import scala.jdk.StreamConverters._
        Files.list(path).toScala(List).foreach(deleteRecursively)
      }
      Files.delete(path)
    }

  // ── PlainVirtualFile ID tests ──────────────────────────────────────────

  test("PlainVirtualFile uses ${BASE} prefix for paths under build dir") {
    withTempBuildDir("base-prefix") {
      buildDir =>
        val srcFile = buildDir.resolve("src/main/scala/Foo.scala")
      Files.createDirectories(srcFile.getParent)
      Files.writeString(srcFile, "object Foo")

      val vf = PlainVirtualFile(srcFile, buildDir)
      vf.id() shouldBe "${BASE}/src/main/scala/Foo.scala"
      vf.path shouldBe srcFile.toAbsolutePath.normalize()
    }
  }

  test("PlainVirtualFile uses ${LIB} prefix for paths under coursier cache") {
    withTempBuildDir("lib-prefix") {
      buildDir =>
        val coursierCache = PortableAnalysisMappers.coursierCacheDir

      // Don't actually create files in the real coursier cache — just test the ID logic
      // We know the coursier cache dir, so construct a path under it
      val fakeJarPath = coursierCache.resolve("v1/https/repo1.maven.org/maven2/org/example/foo/1.0/foo-1.0.jar")
      val vf = PlainVirtualFile(fakeJarPath, buildDir)
      vf.id() should startWith("${LIB}/")
      vf.id() should endWith("foo-1.0.jar")
    }
  }

  test("PlainVirtualFile uses ${JDK} prefix for paths under java.home") {
    withTempBuildDir("jdk-prefix") {
      buildDir =>
        val jdkDir = PortableAnalysisMappers.jdkDir

      val rtPath = jdkDir.resolve("lib/modules")
      val vf = PlainVirtualFile(rtPath, buildDir)
      vf.id() shouldBe "${JDK}/lib/modules"
    }
  }

  test("PlainVirtualFile uses absolute path for unknown roots") {
    withTempBuildDir("unknown-root") {
      buildDir =>
        val randomPath = Path.of("/some/random/path/foo.jar")
      val vf = PlainVirtualFile(randomPath, buildDir)
      // Should be absolute, no marker prefix
      vf.id() should startWith("/")
      vf.id() should not contain "${BASE}"
      vf.id() should not contain "${LIB}"
    }
  }

  test("PlainVirtualFile equals/hashCode matches BasicVirtualFileRef with same ID") {
    withTempBuildDir("equals-test") {
      buildDir =>
        val srcFile = buildDir.resolve("src/Hello.scala")
      Files.createDirectories(srcFile.getParent)
      Files.writeString(srcFile, "object Hello")

      val vf = PlainVirtualFile(srcFile, buildDir)
      val basic = xsbti.VirtualFileRef.of(vf.id())

      // This is CRITICAL: zinc deserializes to BasicVirtualFileRef, and HashMap lookups
      // must find PlainVirtualFile entries. Symmetric equals is required.
      vf.equals(basic) shouldBe true
      basic.equals(vf) shouldBe true
      vf.hashCode() shouldBe basic.hashCode()
    }
  }

  // ── Path mapper tests ──────────────────────────────────────────────────

  test("writeMapper relativizes output dir under build dir") {
    withTempBuildDir("write-output") { buildDir =>
      val mapper = PortableAnalysisMappers.writeMapper(buildDir)
      val outputDir = buildDir.resolve(".bleep/builds/normal/.bloop/myproject/jvm3/classes")

      val mapped = mapper.mapOutputDir(outputDir)
      mapped.toString.replace('\\', '/') shouldBe "${BASE}/.bleep/builds/normal/.bloop/myproject/jvm3/classes"
    }
  }

  test("writeMapper relativizes classpath entry under coursier cache") {
    withTempBuildDir("write-classpath") { buildDir =>
      val mapper = PortableAnalysisMappers.writeMapper(buildDir)
      val jarPath = PortableAnalysisMappers.coursierCacheDir.resolve("v1/https/repo1/org/foo/1.0/foo-1.0.jar")

      val mapped = mapper.mapClasspathEntry(jarPath)
      mapped.toString.replace('\\', '/') shouldBe "${LIB}/v1/https/repo1/org/foo/1.0/foo-1.0.jar"
    }
  }

  test("writeMapper leaves unknown root paths absolute") {
    withTempBuildDir("write-unknown") { buildDir =>
      val mapper = PortableAnalysisMappers.writeMapper(buildDir)
      val randomPath = Path.of("/opt/custom/lib/thing.jar")

      val mapped = mapper.mapClasspathEntry(randomPath)
      mapped shouldBe randomPath.toAbsolutePath.normalize()
    }
  }

  test("readMapper absolutizes marker-prefixed output dir") {
    withTempBuildDir("read-output") { buildDir =>
      val mapper = PortableAnalysisMappers.readMapper(buildDir)
      val markerPath = Path.of("${BASE}/.bleep/builds/normal/.bloop/myproject/jvm3/classes")

      val mapped = mapper.mapOutputDir(markerPath)
      mapped shouldBe buildDir.resolve(".bleep/builds/normal/.bloop/myproject/jvm3/classes")
    }
  }

  test("readMapper absolutizes ${LIB} classpath entry") {
    withTempBuildDir("read-classpath") { buildDir =>
      val mapper = PortableAnalysisMappers.readMapper(buildDir)
      val markerPath = Path.of("${LIB}/v1/https/repo1/org/foo/1.0/foo-1.0.jar")

      val mapped = mapper.mapClasspathEntry(markerPath)
      mapped shouldBe PortableAnalysisMappers.coursierCacheDir.resolve("v1/https/repo1/org/foo/1.0/foo-1.0.jar")
    }
  }

  test("readMapper passes through absolute paths (backward compatibility)") {
    withTempBuildDir("read-compat") { buildDir =>
      val mapper = PortableAnalysisMappers.readMapper(buildDir)
      val absolutePath = Path.of("/old/machine/path/to/classes")

      val mapped = mapper.mapOutputDir(absolutePath)
      mapped shouldBe absolutePath
    }
  }

  // ── Round-trip tests ───────────────────────────────────────────────────

  test("write+read round-trip recovers original output dir") {
    withTempBuildDir("roundtrip-output") { buildDir =>
      val writeMapper = PortableAnalysisMappers.writeMapper(buildDir)
      val readMapper = PortableAnalysisMappers.readMapper(buildDir)

      val original = buildDir.resolve(".bleep/builds/normal/.bloop/foo/classes")
      val written = writeMapper.mapOutputDir(original)
      val restored = readMapper.mapOutputDir(written)

      restored shouldBe original.toAbsolutePath.normalize()
    }
  }

  test("write+read round-trip works across different build dirs (cross-machine simulation)") {
    withTempBuildDir("roundtrip-cross-a") { buildDirA =>
      withTempBuildDir("roundtrip-cross-b") { buildDirB =>
        val writeMapper = PortableAnalysisMappers.writeMapper(buildDirA)
        val readMapper = PortableAnalysisMappers.readMapper(buildDirB)

        // Machine A output dir
        val outputA = buildDirA.resolve(".bleep/builds/normal/.bloop/myproject/jvm3/classes")
        val written = writeMapper.mapOutputDir(outputA)
        written.toString.replace('\\', '/') shouldBe "${BASE}/.bleep/builds/normal/.bloop/myproject/jvm3/classes"

        // Machine B reads and absolutizes with its own build dir
        val restoredB = readMapper.mapOutputDir(written)
        restoredB shouldBe buildDirB.resolve(".bleep/builds/normal/.bloop/myproject/jvm3/classes")
      }
    }
  }

  test("VirtualFileRef mappers are identity (sbt 2 style)") {
    withTempBuildDir("vfr-identity") { buildDir =>
      val writeMapper = PortableAnalysisMappers.writeMapper(buildDir)
      val readMapper = PortableAnalysisMappers.readMapper(buildDir)

      val vfr = xsbti.VirtualFileRef.of("${BASE}/src/Foo.scala")
      writeMapper.mapSourceFile(vfr) should be theSameInstanceAs vfr
      readMapper.mapSourceFile(vfr) should be theSameInstanceAs vfr

      val absVfr = xsbti.VirtualFileRef.of("/absolute/path/Foo.scala")
      writeMapper.mapBinaryFile(absVfr) should be theSameInstanceAs absVfr
      readMapper.mapBinaryFile(absVfr) should be theSameInstanceAs absVfr
    }
  }

  // ── analysisMappers integration ────────────────────────────────────────

  test("analysisMappers infers build dir from analysis file path") {
    withTempBuildDir("infer-builddir") { buildDir =>
      val analysisFile = buildDir.resolve(".bleep/builds/normal/.bloop/myproject/jvm3/.zinc/analysis.zip")
      Files.createDirectories(analysisFile.getParent)

      // Test via the private inferBuildDir method (indirectly through analysisMappers)
      // We can't directly test the private method, but we can test that the mapper works
      // by checking that it relativizes paths correctly
      val mappers = ZincBridge.analysisMappers(analysisFile)
      val writeMapper = mappers.getWriteMapper

      val outputDir = buildDir.resolve(".bleep/builds/normal/.bloop/myproject/jvm3/classes")
      val mapped = writeMapper.mapOutputDir(outputDir)
      mapped.toString.replace('\\', '/') shouldBe "${BASE}/.bleep/builds/normal/.bloop/myproject/jvm3/classes"
    }
  }
}
