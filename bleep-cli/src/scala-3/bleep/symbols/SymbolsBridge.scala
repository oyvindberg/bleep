package bleep.symbols

import bleep.{ResolvedJvm, ResolvedProject}
import tastyquery.Classpaths.Classpath
import tastyquery.Contexts.Context
import tastyquery.jdk.ClasspathLoaders

import java.net.{URI, URLClassLoader}
import java.nio.file.{FileSystems, Files, Path}
import scala.jdk.CollectionConverters.*

/** Bridge from bleep's ResolvedProject / ResolvedJvm to a tasty-query Context.
  *
  * Bleep already owns the build, so we skip cellar's classpath extraction and feed jars + classes dirs straight into tasty-query.
  */
object SymbolsBridge {

  /** Classpath for a resolved project: its compiled classes plus all dependency jars. Test classes are included for test projects via the resolved classpath.
    */
  def projectClasspath(project: ResolvedProject): Classpath = {
    val paths = project.classpath ++ List(project.classesDir)
    readClasspathRobust(paths)
  }

  /** JRE classpath loaded from the JDK at the given home. Reads from `jrt:/` via the JDK's internal JrtFileSystemProvider, falling back to a URLClassLoader on
    * `lib/jrt-fs.jar` if the internal class is locked down.
    */
  def jreClasspath(jvm: ResolvedJvm): Classpath = {
    val javaHome = jvm.javaBin.getParent.getParent // .../bin/java -> .../
    val jrtFsJar = javaHome.resolve("lib/jrt-fs.jar")
    if (!Files.exists(jrtFsJar))
      throw new IllegalArgumentException(s"Not a valid JDK home (missing lib/jrt-fs.jar): $javaHome")
    val env = java.util.Map.of("java.home", javaHome.toString)
    val fs =
      try {
        val cls = Class.forName("jdk.internal.jrtfs.JrtFileSystemProvider")
        val ctor = cls.getDeclaredConstructor()
        ctor.setAccessible(true)
        val provider = ctor.newInstance().asInstanceOf[java.nio.file.spi.FileSystemProvider]
        provider.newFileSystem(URI.create("jrt:/"), env)
      } catch {
        case _: java.lang.reflect.InaccessibleObjectException =>
          val cl = new URLClassLoader(Array(jrtFsJar.toUri.toURL))
          FileSystems.newFileSystem(URI.create("jrt:/"), env, cl)
      }
    ClasspathLoaders.read(Files.list(fs.getPath("modules")).iterator().asScala.toList)
  }

  /** Full context for a project: JRE + project classpath, initialized as one tasty-query Context. */
  def contextOf(project: ResolvedProject, jvm: ResolvedJvm): (Context, Classpath) = {
    val jre = jreClasspath(jvm)
    val projectCp = projectClasspath(project)
    val classpath = jre ++ projectCp
    (Context.initialize(classpath), classpath)
  }

  /** Reads the classpath, excluding paths that cause `MatchError` in tasty-query (e.g. vendor-injected JRT modules such as the Azul CRS client). Vendored from
    * cellar.ContextResource.
    */
  private def readClasspathRobust(paths: List[Path]): Classpath =
    try ClasspathLoaders.read(paths)
    catch {
      case e: MatchError =>
        val bad = paths.find { p =>
          try { val _ = ClasspathLoaders.read(List(p)); false }
          catch { case _: MatchError => true }
        }
        bad match {
          case Some(offender) => readClasspathRobust(paths.filterNot(_ == offender))
          case None           => throw e
        }
    }
}
