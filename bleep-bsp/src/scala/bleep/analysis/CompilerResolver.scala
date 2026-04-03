package bleep.analysis

import java.lang.invoke.{MethodHandles, MethodType}
import java.net.URLClassLoader
import java.nio.file.{Files, Path}
import java.util.concurrent.ConcurrentHashMap

import bleep.model.{Dep, VersionCombo, VersionKotlin, VersionScala, VersionScalaJs, VersionScalaNative}
import coursier.Fetch

/** Resolves compiler artifacts and creates isolated classloaders.
  *
  * This allows using different compiler versions than what bleep-bsp bundles, by fetching the appropriate JARs via Coursier and loading them in isolation.
  *
  * Uses bleep-model's VersionScala and VersionKotlin for dependency definitions, ensuring consistency with the rest of the bleep build system.
  *
  * Based on bloop's ScalaInstance and ScalaInstanceTopLoader patterns.
  */
object CompilerResolver {

  // ============================================================================
  // Compiler Instance Classes
  // ============================================================================

  /** A resolved compiler instance with its JARs and classloader.
    *
    * @param language
    *   the compiler language (scala, kotlin)
    * @param version
    *   the compiler version
    * @param allJars
    *   all resolved JAR files
    * @param loader
    *   isolated classloader for this compiler
    */
  case class CompilerInstance(
      language: String,
      version: String,
      allJars: Seq[Path],
      loader: ClassLoader
  ) {

    /** Get the compiler JARs (excluding library JARs) */
    def compilerJars: Seq[Path] = allJars.filter { p =>
      val name = p.getFileName.toString
      language match {
        case "scala" =>
          name.contains("scala3-compiler") || name.contains("scala-compiler") ||
          name.contains("scala-reflect") || name.contains("scala-asm") ||
          name.contains("tasty-core") || name.contains("scala3-interfaces")
        case "kotlin" =>
          name.contains("kotlin-compiler") || name.contains("kotlin-scripting")
        case _ => false
      }
    }

    /** Get the library JARs */
    def libraryJars: Seq[Path] = allJars.filter { p =>
      val name = p.getFileName.toString
      language match {
        case "scala"  => name.contains("scala3-library") || name.contains("scala-library")
        case "kotlin" => name.contains("kotlin-stdlib")
        case _        => false
      }
    }
  }

  // ============================================================================
  // Classloader Infrastructure
  // ============================================================================

  /** The boot/platform classloader for JDK classes.
    *
    * On Java 9+, this is the platform classloader. On Java 8, this is null (which means bootstrap classloader).
    */
  private val bootClassLoader: ClassLoader =
    if !scala.util.Properties.isJavaAtLeast("9") then null
    else
      try
        MethodHandles
          .lookup()
          .findStatic(
            classOf[ClassLoader],
            "getPlatformClassLoader",
            MethodType.methodType(classOf[ClassLoader])
          )
          .invoke()
          .asInstanceOf[ClassLoader]
      catch case _: Throwable => null

  /** Custom classloader that delegates specific packages to the parent.
    *
    * This is based on bloop's ScalaInstanceTopLoader. It ensures that:
    *   - xsbti.* classes (Zinc interfaces) come from the parent (bleep's classloader)
    *   - JDK classes come from the boot classloader
    *   - Everything else comes from the compiler JARs
    *
    * This prevents classloader conflicts when the loaded compiler uses different versions of shared dependencies.
    */
  private class CompilerTopLoader(bleepClassLoader: ClassLoader, parent: ClassLoader) extends ClassLoader(parent) {
    override protected def loadClass(name: String, resolve: Boolean): Class[?] =
      // xsbti.* interfaces need to come from bleep's classloader for Zinc compatibility.
      // sbt.testing.* interfaces need to come from bleep's classloader so that
      // TestAdapter (from scala-native:test-runner) returns Framework/Runner/Task objects
      // that are assignment-compatible with bleep's sbt.testing types.
      if name.startsWith("xsbti.") || name.startsWith("sbt.testing.") then {
        val c = bleepClassLoader.loadClass(name)
        if resolve then resolveClass(c)
        c
      } else super.loadClass(name, resolve)
  }

  /** The top-level classloader for compiler instances.
    *
    * This is shared by all compiler instances and delegates xsbti.* to bleep's classloader.
    */
  private lazy val topClassLoader: ClassLoader = {
    val bleepClassLoader = getClass.getClassLoader
    new CompilerTopLoader(bleepClassLoader, bootClassLoader)
  }

  // ============================================================================
  // Caching
  // ============================================================================

  private case class InstanceKey(language: String, version: String)

  /** Cache of resolved compiler instances */
  private val instanceCache = new ConcurrentHashMap[InstanceKey, CompilerInstance]()

  /** Cache of resolved JAR paths */
  private val jarCache = new ConcurrentHashMap[InstanceKey, Seq[Path]]()

  // ============================================================================
  // Resolution Methods
  // ============================================================================

  /** Resolve and create a Scala compiler instance for the specified version.
    *
    * @param version
    *   Scala version (e.g., "3.3.3", "3.7.4")
    * @return
    *   compiler instance with isolated classloader
    */
  def getScalaCompiler(version: String): CompilerInstance =
    getScalaCompiler(VersionScala(version))

  /** Resolve and create a Scala compiler instance for the specified version.
    *
    * @param version
    *   VersionScala instance
    * @return
    *   compiler instance with isolated classloader
    */
  def getScalaCompiler(version: VersionScala): CompilerInstance = {
    val key = InstanceKey("scala", version.scalaVersion)
    instanceCache.computeIfAbsent(
      key,
      _ => {
        val jars = resolveScalaCompiler(version)
        val loader = createCompilerClassLoader(jars)
        CompilerInstance("scala", version.scalaVersion, jars, loader)
      }
    )
  }

  /** Resolve and create a Kotlin compiler instance for the specified version.
    *
    * @param version
    *   Kotlin version (e.g., "2.0.0", "2.3.0")
    * @return
    *   compiler instance with isolated classloader
    */
  def getKotlinCompiler(version: String): CompilerInstance =
    getKotlinCompiler(VersionKotlin(version))

  /** Resolve and create a Kotlin compiler instance for the specified version.
    *
    * @param version
    *   VersionKotlin instance
    * @return
    *   compiler instance with isolated classloader
    */
  def getKotlinCompiler(version: VersionKotlin): CompilerInstance = {
    val key = InstanceKey("kotlin", version.kotlinVersion)
    instanceCache.computeIfAbsent(
      key,
      _ => {
        val jars = resolveKotlinCompiler(version)
        val loader = createCompilerClassLoader(jars)
        CompilerInstance("kotlin", version.kotlinVersion, jars, loader)
      }
    )
  }

  /** Resolve Scala 3 compiler JARs for a specific version.
    *
    * Uses VersionScala.compiler to get the correct dependency definition.
    *
    * @param version
    *   Scala version string
    * @return
    *   paths to all necessary compiler JARs
    */
  def resolveScalaCompiler(version: String): Seq[Path] =
    resolveScalaCompiler(VersionScala(version))

  /** Resolve Scala 3 compiler JARs for a specific version.
    *
    * @param version
    *   VersionScala instance
    * @return
    *   paths to all necessary compiler JARs
    */
  def resolveScalaCompiler(version: VersionScala): Seq[Path] = {
    val key = InstanceKey("scala-compiler", version.scalaVersion)
    Option(jarCache.get(key)).getOrElse {
      val combo = VersionCombo.Jvm(version)
      val paths = resolveDep(version.compiler, combo)
      jarCache.put(key, paths)
      paths
    }
  }

  /** Resolve Scala 3 library JARs for a specific version.
    *
    * Uses VersionScala.libraries to get the correct dependency definitions, which handles the scala-library vs scala3-library distinction.
    *
    * @param version
    *   Scala version string
    * @return
    *   paths to all necessary library JARs
    */
  def resolveScalaLibrary(version: String): Seq[Path] =
    resolveScalaLibrary(VersionScala(version))

  /** Resolve Scala 3 library JARs for a specific version.
    *
    * @param version
    *   VersionScala instance
    * @return
    *   paths to all necessary library JARs
    */
  def resolveScalaLibrary(version: VersionScala): Seq[Path] = {
    val key = InstanceKey("scala-library", version.scalaVersion)
    Option(jarCache.get(key)).getOrElse {
      val combo = VersionCombo.Jvm(version)
      val paths = version.libraries.flatMap(dep => resolveDep(dep, combo))
      jarCache.put(key, paths)
      paths
    }
  }

  /** Resolve Kotlin compiler JARs for a specific version.
    *
    * Uses VersionKotlin.compiler to get the correct dependency definition.
    *
    * @param version
    *   Kotlin version string
    * @return
    *   paths to all necessary compiler JARs
    */
  def resolveKotlinCompiler(version: String): Seq[Path] =
    resolveKotlinCompiler(VersionKotlin(version))

  /** Resolve Kotlin compiler JARs for a specific version.
    *
    * The kotlin-compiler artifact includes incremental compilation support.
    *
    * @param version
    *   VersionKotlin instance
    * @return
    *   paths to all necessary compiler JARs
    */
  def resolveKotlinCompiler(version: VersionKotlin): Seq[Path] = {
    val key = InstanceKey("kotlin-compiler", version.kotlinVersion)
    Option(jarCache.get(key)).getOrElse {
      val paths = resolveDep(version.compiler)
      jarCache.put(key, paths)
      paths
    }
  }

  /** Resolve Kotlin standard library JARs for a specific version.
    *
    * Uses VersionKotlin.library to get the correct dependency definition.
    *
    * @param version
    *   Kotlin version string
    * @return
    *   paths to all necessary library JARs
    */
  def resolveKotlinLibrary(version: String): Seq[Path] =
    resolveKotlinLibrary(VersionKotlin(version))

  /** Resolve Kotlin standard library JARs for a specific version.
    *
    * @param version
    *   VersionKotlin instance
    * @return
    *   paths to all necessary library JARs
    */
  def resolveKotlinLibrary(version: VersionKotlin): Seq[Path] = {
    val key = InstanceKey("kotlin-stdlib", version.kotlinVersion)
    Option(jarCache.get(key)).getOrElse {
      val paths = resolveDep(version.library)
      jarCache.put(key, paths)
      paths
    }
  }

  /** Resolve a Kotlin compiler plugin JAR for a specific version.
    *
    * Maps well-known plugin IDs (spring, jpa, allopen, noarg, serialization, etc.) to their Maven artifact names and resolves via coursier. Returns only the
    * plugin JAR itself (filtered from transitive dependencies).
    *
    * @param pluginId
    *   plugin ID as used in kotlin-maven-plugin (e.g., "spring", "jpa", "allopen")
    * @param version
    *   Kotlin version
    * @return
    *   path to the plugin JAR
    */
  def resolveKotlinPlugin(pluginId: String, version: VersionKotlin): Path = {
    val artifactName = pluginIdToArtifact(pluginId)
    val key = InstanceKey(artifactName, version.kotlinVersion)
    Option(jarCache.get(key))
      .getOrElse {
        val dep = Dep.Java(version.kotlinOrganization, artifactName, version.kotlinVersion)
        val paths = resolveDep(dep)
        jarCache.put(key, paths)
        paths
      }
      .find(_.getFileName.toString.startsWith(artifactName))
      .getOrElse(throw new IllegalArgumentException(s"Could not find plugin JAR for $pluginId ($artifactName) in resolved artifacts"))
  }

  private def pluginIdToArtifact(pluginId: String): String = pluginId match {
    case "spring" | "allopen" | "all-open"         => "kotlin-allopen-compiler-plugin"
    case "jpa" | "noarg"                           => "kotlin-noarg-compiler-plugin"
    case "serialization" | "kotlinx-serialization" => "kotlin-serialization-compiler-plugin"
    case "sam-with-receiver"                       => "kotlin-sam-with-receiver-compiler-plugin"
    case "lombok"                                  => "kotlin-lombok-compiler-plugin"
    case "assignment"                              => "kotlin-assignment-compiler-plugin"
    case other                                     => s"kotlin-$other-compiler-plugin"
  }

  // ============================================================================
  // Internal Resolution
  // ============================================================================

  /** Resolve a bleep Dep to JAR paths using Coursier.
    *
    * This converts the bleep Dep to a Coursier Dependency and fetches it. For Scala dependencies, uses VersionCombo.Jvm to compute the correct module name
    * suffix.
    */
  private def resolveDep(dep: Dep, combo: VersionCombo = VersionCombo.Java): Seq[Path] =
    dep.asJava(combo) match {
      case Right(javaDep) =>
        val coursierDep = javaDep.dependency
        val files = Fetch()
          .addDependencies(coursierDep)
          .run()
        files.map(_.toPath).toSeq
      case Left(err) =>
        throw new IllegalArgumentException(s"Failed to resolve dependency: $err")
    }

  /** Create an isolated classloader for compiler JARs.
    *
    * The classloader hierarchy is:
    *   - Boot classloader (JDK classes)
    *     - CompilerTopLoader (delegates xsbti.* to bleep)
    *       - URLClassLoader (compiler JARs)
    */
  private def createCompilerClassLoader(jars: Seq[Path]): ClassLoader = {
    val urls = jars.map(_.toUri.toURL).toArray
    new URLClassLoader(urls, topClassLoader)
  }

  /** Resolve Scala.js compiler plugin JARs.
    *
    * For Scala 2, this is needed as a -Xplugin argument. For Scala 3, use the -scalajs flag instead.
    *
    * @param scalaJsVersion
    *   Scala.js version (e.g., "1.16.0")
    * @param scalaVersion
    *   Full Scala version (e.g., "2.13.15")
    * @return
    *   paths to the compiler plugin JAR(s)
    */
  def resolveScalaJsCompilerPlugin(scalaJsVersion: String, scalaVersion: String): Seq[Path] = {
    val vsjs = VersionScalaJs(scalaJsVersion)
    val vs = VersionScala(scalaVersion)
    val combo = VersionCombo.Js(vs, vsjs)
    val key = InstanceKey(s"scalajs-plugin-$scalaVersion", scalaJsVersion)
    Option(jarCache.get(key)).getOrElse {
      // compilerPlugin uses fullCrossVersion=true; forceJvm strips platform suffix → scalajs-compiler_<fullScalaVersion>
      val pluginDep = vsjs.compilerPlugin.mapScala(_.copy(forceJvm = true))
      val paths = resolveDep(pluginDep, combo)
      jarCache.put(key, paths)
      paths
    }
  }

  /** Resolve Scala Native compiler plugin (nscplugin) JARs.
    *
    * @param scalaNativeVersion
    *   Scala Native version (e.g., "0.5.6")
    * @param scalaVersion
    *   Full Scala version (e.g., "3.3.3", "2.13.15")
    * @return
    *   paths to the nscplugin JAR(s)
    */
  def resolveScalaNativeCompilerPlugin(scalaNativeVersion: String, scalaVersion: String): Seq[Path] = {
    val vsn = VersionScalaNative(scalaNativeVersion)
    val vs = VersionScala(scalaVersion)
    val combo = VersionCombo.Native(vs, vsn)
    val key = InstanceKey(s"scala-native-plugin-$scalaVersion", scalaNativeVersion)
    Option(jarCache.get(key)).getOrElse {
      // compilerPlugin is fullCrossVersion=true; forceJvm strips platform suffix → nscplugin_<fullScalaVersion>
      val pluginDep = vsn.compilerPlugin.mapScala(_.copy(forceJvm = true))
      val paths = resolveDep(pluginDep, combo)
      jarCache.put(key, paths)
      paths
    }
  }

  /** Resolve a Java dependency (org:name:version) to JAR paths.
    *
    * Useful in tests for resolving arbitrary dependencies.
    *
    * @param org
    *   Maven organization
    * @param name
    *   Maven artifact name (already includes platform/Scala suffix)
    * @param version
    *   Maven version
    * @return
    *   paths to all resolved JARs
    */
  def resolveJavaDep(org: String, name: String, version: String): Seq[Path] = {
    val dep = Dep.JavaDependency(
      coursier.core.Organization(org),
      coursier.core.ModuleName(name),
      version
    )
    resolveDep(dep)
  }

  /** Resolve a Dep using a VersionCombo for proper cross-version suffix computation.
    *
    * This is the preferred way to resolve platform-specific dependencies in tests.
    */
  def resolveDeps(dep: Dep, combo: VersionCombo): Seq[Path] =
    resolveDep(dep, combo)

  /** Clear all caches (useful for testing) */
  def clearCaches(): Unit = {
    instanceCache.clear()
    jarCache.clear()
  }

  // ============================================================================
  // Scala.js Toolchain Resolution
  // ============================================================================

  /** Resolve Scala.js linker JARs for a specific version.
    *
    * The linker includes the scalajs-linker and related tools needed for linking Scala.js IR to JavaScript.
    *
    * @param scalaJsVersion
    *   Scala.js version (e.g., "1.16.0", "1.19.0")
    * @param scalaVersion
    *   Scala version for the linker (e.g., "2.13.15")
    * @return
    *   compiler instance with isolated classloader
    */
  def getScalaJsLinker(scalaJsVersion: String, scalaVersion: String): CompilerInstance = {
    val key = InstanceKey(s"scalajs-linker-$scalaVersion", scalaJsVersion)
    instanceCache.computeIfAbsent(
      key,
      _ => {
        val jars = resolveScalaJsLinker(scalaJsVersion, scalaVersion)
        val loader = createCompilerClassLoader(jars)
        CompilerInstance("scalajs", scalaJsVersion, jars, loader)
      }
    )
  }

  /** Resolve Scala.js linker JARs for a specific version.
    *
    * @param scalaJsVersion
    *   Scala.js version
    * @param scalaVersion
    *   Scala version for the linker
    * @return
    *   paths to all necessary linker JARs
    */
  def resolveScalaJsLinker(scalaJsVersion: String, scalaVersion: String): Seq[Path] = {
    val key = InstanceKey(s"scalajs-linker-$scalaVersion", scalaJsVersion)
    Option(jarCache.get(key)).getOrElse {
      // scalajs-linker is published for Scala 2.12 and 2.13
      val linkerScalaVersion =
        if (scalaVersion.startsWith("3.")) "2.13"
        else {
          val parts = scalaVersion.split('.')
          if (parts.length >= 2) s"${parts(0)}.${parts(1)}" else "2.13"
        }
      val linkerDep = Dep.JavaDependency(
        coursier.core.Organization("org.scala-js"),
        coursier.core.ModuleName(s"scalajs-linker_$linkerScalaVersion"),
        scalaJsVersion
      )
      val paths = resolveDep(linkerDep)
      jarCache.put(key, paths)
      paths
    }
  }

  /** Resolve Scala.js standard library for a specific version.
    *
    * @param scalaJsVersion
    *   Scala.js version
    * @param scalaVersion
    *   Scala version for the library
    * @return
    *   paths to all necessary library JARs (including scalajs-library)
    */
  def resolveScalaJsLibrary(scalaJsVersion: String, scalaVersion: String): Seq[Path] = {
    val vsjs = VersionScalaJs(scalaJsVersion)
    val vs = VersionScala(scalaVersion)
    val combo = VersionCombo.Js(vs, vsjs)
    val key = InstanceKey(s"scalajs-library-$scalaVersion", scalaJsVersion)
    Option(jarCache.get(key)).getOrElse {
      // For Scala 3: library3 is a JavaDependency for scalajs-library_2.13
      // For Scala 2: library is a ScalaDependency with forceJvm=true
      val libraryDep: Dep = if (vs.is3) vsjs.library3 else vsjs.library
      val paths = resolveDep(libraryDep, combo)
      jarCache.put(key, paths)
      paths
    }
  }

  // ============================================================================
  // Scala Native Toolchain Resolution
  // ============================================================================

  /** Resolve and create a Scala Native tools instance for the specified version.
    *
    * @param scalaNativeVersion
    *   Scala Native version (e.g., "0.4.17", "0.5.6")
    * @param scalaVersion
    *   Scala version for the tools (e.g., "2.13.15", "3.3.3")
    * @return
    *   compiler instance with isolated classloader
    */
  def getScalaNativeTools(scalaNativeVersion: String, scalaVersion: String): CompilerInstance = {
    val key = InstanceKey(s"scala-native-tools-$scalaVersion", scalaNativeVersion)
    instanceCache.computeIfAbsent(
      key,
      _ => {
        val jars = resolveScalaNativeTools(scalaNativeVersion, scalaVersion)
        val loader = createCompilerClassLoader(jars)
        CompilerInstance("scala-native", scalaNativeVersion, jars, loader)
      }
    )
  }

  /** Resolve Scala Native tools JARs for a specific version.
    *
    * @param scalaNativeVersion
    *   Scala Native version
    * @param scalaVersion
    *   Scala version for the tools
    * @return
    *   paths to all necessary tools JARs
    */
  def resolveScalaNativeTools(scalaNativeVersion: String, scalaVersion: String): Seq[Path] = {
    val key = InstanceKey(s"scala-native-tools-$scalaVersion", scalaNativeVersion)
    Option(jarCache.get(key)).getOrElse {
      // scala-native tools are published for Scala 2.12/2.13/3.x
      val toolsScalaVersion =
        if (scalaVersion.startsWith("3.")) "3"
        else {
          val parts = scalaVersion.split('.')
          if (parts.length >= 2) s"${parts(0)}.${parts(1)}" else "2.13"
        }
      val toolsDep = Dep.JavaDependency(
        coursier.core.Organization("org.scala-native"),
        coursier.core.ModuleName(s"tools_$toolsScalaVersion"),
        scalaNativeVersion
      )
      val paths = resolveDep(toolsDep)
      jarCache.put(key, paths)
      paths
    }
  }

  /** Resolve Scala Native library for a specific version.
    *
    * @param scalaNativeVersion
    *   Scala Native version
    * @param scalaVersion
    *   Scala version for the library
    * @return
    *   paths to all necessary library JARs
    */
  def resolveScalaNativeLibrary(scalaNativeVersion: String, scalaVersion: String): Seq[Path] = {
    val vsn = VersionScalaNative(scalaNativeVersion)
    val vs = VersionScala(scalaVersion)
    val combo = VersionCombo.Native(vs, vsn)
    val key = InstanceKey(s"scala-native-library-$scalaVersion", scalaNativeVersion)
    Option(jarCache.get(key)).getOrElse {
      // Use VersionCombo.libraries() which handles the complex nativelib naming
      // (scalalib/scala3lib with version like "3.3.3+0.5.6" for SN 0.5+)
      val platformLibs = combo.libraries(isTest = false)
      // Deduplicate: javalib and scala3lib share transitive deps (nativelib, clib, etc.)
      // but each resolveDep call resolves transitives independently
      val paths = platformLibs.flatMap(dep => resolveDep(dep, combo)).distinct
      jarCache.put(key, paths)
      paths
    }
  }

  /** Resolve Scala Native test-runner JARs for a specific version.
    *
    * The test-runner module provides the JVM-side TestAdapter that communicates with the native test binary via a TCP socket-based RPC protocol.
    *
    * @param scalaNativeVersion
    *   Scala Native version (e.g., "0.5.6")
    * @return
    *   paths to all necessary test-runner JARs
    */
  def resolveScalaNativeTestRunner(scalaNativeVersion: String): Seq[Path] = {
    val key = InstanceKey("scala-native-test-runner", scalaNativeVersion)
    Option(jarCache.get(key)).getOrElse {
      // test-runner is published for Scala 2.12, 2.13, and 3
      val dep = Dep.JavaDependency(
        coursier.core.Organization("org.scala-native"),
        coursier.core.ModuleName("test-runner_3"),
        scalaNativeVersion
      )
      val paths = resolveDep(dep)
      jarCache.put(key, paths)
      paths
    }
  }

  /** Get a Scala Native test-runner instance with isolated classloader.
    *
    * @param scalaNativeVersion
    *   Scala Native version (e.g., "0.5.6")
    * @return
    *   compiler instance with the test-runner classes
    */
  def getScalaNativeTestRunner(scalaNativeVersion: String): CompilerInstance = {
    val key = InstanceKey("scala-native-test-runner", scalaNativeVersion)
    instanceCache.computeIfAbsent(
      key,
      _ => {
        val jars = resolveScalaNativeTestRunner(scalaNativeVersion)
        val loader = createCompilerClassLoader(jars)
        CompilerInstance("scala-native-test-runner", scalaNativeVersion, jars, loader)
      }
    )
  }

  // ============================================================================
  // Kotlin/JS Toolchain Resolution
  // ============================================================================

  /** Resolve and create a Kotlin/JS compiler instance for the specified version.
    *
    * The K2JSCompiler is included in the kotlin-compiler artifact.
    *
    * @param version
    *   Kotlin version (e.g., "2.0.0", "2.3.0")
    * @return
    *   compiler instance with isolated classloader
    */
  def getKotlinJsCompiler(version: String): CompilerInstance =
    getKotlinJsCompiler(VersionKotlin(version))

  /** Resolve and create a Kotlin/JS compiler instance for the specified version.
    *
    * @param version
    *   VersionKotlin instance
    * @return
    *   compiler instance with isolated classloader
    */
  def getKotlinJsCompiler(version: VersionKotlin): CompilerInstance = {
    // K2JSCompiler is in the regular kotlin-compiler, so we reuse the same instance
    val key = InstanceKey("kotlin-js", version.kotlinVersion)
    instanceCache.computeIfAbsent(
      key,
      _ => {
        val jars = resolveKotlinCompiler(version)
        val loader = createCompilerClassLoader(jars)
        CompilerInstance("kotlin-js", version.kotlinVersion, jars, loader)
      }
    )
  }

  /** Resolve Kotlin/JS standard library for a specific version.
    *
    * @param version
    *   Kotlin version string
    * @return
    *   paths to all necessary library JARs
    */
  def resolveKotlinJsLibrary(version: String): Seq[Path] =
    resolveKotlinJsLibrary(VersionKotlin(version))

  /** Resolve Kotlin/JS standard library for a specific version.
    *
    * @param version
    *   VersionKotlin instance
    * @return
    *   paths to JS-compatible library files (KLIB or JS-specific JARs)
    */
  def resolveKotlinJsLibrary(version: VersionKotlin): Seq[Path] = {
    val key = InstanceKey("kotlin-stdlib-js", version.kotlinVersion)
    Option(jarCache.get(key)).getOrElse {
      val dep = Dep.JavaDependency(
        coursier.core.Organization("org.jetbrains.kotlin"),
        coursier.core.ModuleName("kotlin-stdlib-js"),
        version.kotlinVersion
      )
      // Filter to only include JS-compatible files (KLIB or -js- artifacts)
      val paths = resolveDep(dep).filter(isJsCompatibleLibrary)
      jarCache.put(key, paths)
      paths
    }
  }

  /** Resolve Kotlin/JS test library for a specific version.
    *
    * This includes kotlin-test-js which is needed for test projects using kotlin.test annotations.
    *
    * @param version
    *   Kotlin version string
    * @return
    *   paths to JS-compatible test library files
    */
  def resolveKotlinTestJs(version: String): Seq[Path] =
    resolveKotlinTestJs(VersionKotlin(version))

  /** Resolve Kotlin/JS test library for a specific version.
    *
    * @param version
    *   VersionKotlin instance
    * @return
    *   paths to JS-compatible test library files
    */
  def resolveKotlinTestJs(version: VersionKotlin): Seq[Path] = {
    val key = InstanceKey("kotlin-test-js", version.kotlinVersion)
    Option(jarCache.get(key)).getOrElse {
      val dep = Dep.JavaDependency(
        coursier.core.Organization("org.jetbrains.kotlin"),
        coursier.core.ModuleName("kotlin-test-js"),
        version.kotlinVersion
      )
      // Filter to only include JS-compatible files (KLIB or -js- artifacts)
      val paths = resolveDep(dep).filter(isJsCompatibleLibrary)
      jarCache.put(key, paths)
      paths
    }
  }

  /** Check if a library path is compatible with Kotlin/JS compilation.
    *
    * The Kotlin/JS compiler only accepts KLIB files or JS-specific JARs. JVM-only dependencies (like kotlin-stdlib without -js) will cause errors.
    */
  private def isJsCompatibleLibrary(path: Path): Boolean = {
    val name = path.getFileName.toString.toLowerCase
    name.endsWith(".klib") || name.contains("-js")
  }

  // ============================================================================
  // Kotlin/Native Toolchain Resolution
  // ============================================================================

  /** Resolve and create a Kotlin/Native compiler instance for the specified version.
    *
    * Uses kotlin-native-compiler-embeddable for embedding in the build tool.
    *
    * @param version
    *   Kotlin version (e.g., "2.0.0", "2.3.0")
    * @return
    *   compiler instance with isolated classloader
    */
  def getKotlinNativeCompiler(version: String): CompilerInstance =
    getKotlinNativeCompiler(VersionKotlin(version))

  /** Resolve and create a Kotlin/Native compiler instance for the specified version.
    *
    * @param version
    *   VersionKotlin instance
    * @return
    *   compiler instance with isolated classloader
    */
  def getKotlinNativeCompiler(version: VersionKotlin): CompilerInstance = {
    val key = InstanceKey("kotlin-native", version.kotlinVersion)
    instanceCache.computeIfAbsent(
      key,
      _ => {
        val jars = resolveKotlinNativeCompilerEmbeddable(version)
        val loader = createCompilerClassLoader(jars)
        CompilerInstance("kotlin-native", version.kotlinVersion, jars, loader)
      }
    )
  }

  /** Resolve Kotlin/Native compiler JARs from the prebuilt distribution.
    *
    * Uses the prebuilt distribution (not Maven) because it includes all required dependencies like trove4j.jar that aren't declared as transitive deps of
    * kotlin-native-compiler-embeddable.
    *
    * @param version
    *   VersionKotlin instance
    * @return
    *   paths to all necessary compiler JARs
    */
  def resolveKotlinNativeCompilerEmbeddable(version: VersionKotlin): Seq[Path] = {
    val key = InstanceKey("kotlin-native-compiler-embeddable", version.kotlinVersion)
    Option(jarCache.get(key)).getOrElse {
      // Use the prebuilt distribution which includes all required JARs (including trove4j)
      val konanHome = resolveKonanHome(version.kotlinVersion)
      val libDir = konanHome.resolve("konan").resolve("lib")
      val jars = if (Files.isDirectory(libDir)) {
        import scala.jdk.CollectionConverters._
        Files
          .list(libDir)
          .iterator()
          .asScala
          .filter(_.toString.endsWith(".jar"))
          .toSeq
      } else {
        // Fallback to Maven if prebuilt distribution doesn't have expected structure
        val dep = Dep.JavaDependency(
          coursier.core.Organization("org.jetbrains.kotlin"),
          coursier.core.ModuleName("kotlin-native-compiler-embeddable"),
          version.kotlinVersion
        )
        resolveDep(dep)
      }
      jarCache.put(key, jars)
      jars
    }
  }

  /** Resolve the Kotlin/Native prebuilt distribution home directory.
    *
    * The K/N compiler needs a distribution directory containing platform libraries, LLVM, etc. This is stored at
    * ~/.konan/kotlin-native-prebuilt-<os>-<arch>-<version>/. If not present, downloads it from Maven Central.
    */
  private def resolveKonanHome(kotlinVersion: String): Path = {
    val konanDir = Path.of(System.getProperty("user.home"), ".konan")
    val os = System.getProperty("os.name").toLowerCase
    val arch = System.getProperty("os.arch").toLowerCase

    val platform = (os, arch) match {
      case (o, "aarch64") if o.contains("mac")   => "macos-aarch64"
      case (o, _) if o.contains("mac")           => "macos-x86_64"
      case (o, "aarch64") if o.contains("linux") => "linux-x86_64" // K/N doesn't have linux-aarch64 prebuilt
      case (o, _) if o.contains("linux")         => "linux-x86_64"
      case _                                     => "linux-x86_64"
    }

    val distDir = konanDir.resolve(s"kotlin-native-prebuilt-$platform-$kotlinVersion")
    if (Files.isDirectory(distDir)) {
      distDir
    } else {
      // Download and extract the prebuilt distribution
      Files.createDirectories(konanDir)
      val tarGz = konanDir.resolve(s"kotlin-native-prebuilt-$kotlinVersion-$platform.tar.gz")
      if (!Files.exists(tarGz)) {
        val url =
          s"https://repo1.maven.org/maven2/org/jetbrains/kotlin/kotlin-native-prebuilt/$kotlinVersion/kotlin-native-prebuilt-$kotlinVersion-$platform.tar.gz"
        val conn = java.net.URI.create(url).toURL.openConnection()
        val in = conn.getInputStream
        try Files.copy(in, tarGz)
        finally in.close()
      }
      // Extract
      val pb = new ProcessBuilder("tar", "xzf", tarGz.toString)
        .directory(konanDir.toFile)
        .redirectErrorStream(true)
      val proc = pb.start()
      proc.getInputStream.transferTo(java.io.OutputStream.nullOutputStream())
      val exitCode = proc.waitFor()
      if (exitCode != 0) throw new RuntimeException(s"Failed to extract Kotlin/Native distribution: exit code $exitCode")
      if (!Files.isDirectory(distDir)) throw new RuntimeException(s"Kotlin/Native distribution not found after extraction at $distDir")
      distDir
    }
  }

  /** Resolve Kotlin/Native test library for a specific version.
    *
    * This provides kotlin.test annotations and assertions for native test projects. The library is packaged as a KLIB within the Kotlin/Native prebuilt
    * distribution.
    *
    * @param version
    *   Kotlin version string
    * @return
    *   paths to the kotlin-test KLIB
    */
  def resolveKotlinTestNative(version: String): Seq[Path] =
    resolveKotlinTestNative(VersionKotlin(version))

  /** Resolve Kotlin/Native test library for a specific version.
    *
    * The kotlin-test KLIB is located in the K/N prebuilt distribution at: ~/.konan/kotlin-native-prebuilt-<platform>-<version>/klib/common/kotlin-test
    *
    * @param version
    *   VersionKotlin instance
    * @return
    *   paths to the kotlin-test KLIB directory
    */
  def resolveKotlinTestNative(version: VersionKotlin): Seq[Path] = {
    val key = InstanceKey("kotlin-test-native", version.kotlinVersion)
    Option(jarCache.get(key)).getOrElse {
      // The kotlin-test KLIB is in the K/N distribution
      val konanDir = Path.of(System.getProperty("user.home"), ".konan")
      val os = System.getProperty("os.name").toLowerCase
      val arch = System.getProperty("os.arch").toLowerCase

      val platform = (os, arch) match {
        case (o, "aarch64") if o.contains("mac")   => "macos-aarch64"
        case (o, _) if o.contains("mac")           => "macos-x86_64"
        case (o, "aarch64") if o.contains("linux") => "linux-x86_64" // K/N doesn't have linux-aarch64 prebuilt
        case (o, _) if o.contains("linux")         => "linux-x86_64"
        case _                                     => "linux-x86_64"
      }

      val distDir = konanDir.resolve(s"kotlin-native-prebuilt-$platform-${version.kotlinVersion}")
      val testKlib = distDir.resolve("klib").resolve("common").resolve("kotlin-test")

      val paths =
        if (java.nio.file.Files.isDirectory(testKlib)) Seq(testKlib)
        else Seq.empty // Distribution not yet downloaded - will be downloaded during compilation

      jarCache.put(key, paths)
      paths
    }
  }
}
