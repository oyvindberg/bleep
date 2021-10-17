package bleep.infra

import bloop.config.Config.Project

import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.StandardCopyOption
import java.util.jar.Attributes
import java.util.jar.JarOutputStream
import java.util.jar.Manifest
import scala.collection.mutable
import scala.sys.process.Process
import scala.util.Properties

/**
 * Ported from https://github.com/scalameta/sbt-native-image
 */
class NativeImagePlugin(
    project: Project,
    // The version of GraalVM to use by default.
    nativeImageVersion: String = "20.2.0",
    // The GraalVM JVM version, one of: graalvm-java11 (default) | graalvm (Java 8)
    nativeImageJvm: String = "graalvm-java11",
    // The JVM version index to use, one of: cs (default) | jabba
    nativeImageJvmIndex: String = "cs",
    // Extra command-line arguments that should be forwarded to the native-image optimizer.
    nativeImageOptions: Seq[String] = Nil) {

  val target = project.directory.resolve("target")
  val targetNativeImageInternal = target.resolve("native-image-internal")
  val targetNativeImage = target.resolve("native-image")

  // The binary that is produced by native-image
  def nativeImageOutput: Path = target.resolve(project.name)

  final class MessageOnlyException(override val toString: String) extends RuntimeException(toString)

  // Path to a coursier binary that is used to launch GraalVM native-image.
  def nativeImageCoursier(): Path = {
    val dir = targetNativeImageInternal
    val out = copyResource("coursier", dir)
    if (Properties.isWin) {
      copyResource("coursier.bat", dir)
    } else {
      out
    }
  }

  // Whether GraalVM is manually installed or should be downloaded with coursier.
  lazy val nativeImageInstalled: Boolean = {
    "true".equalsIgnoreCase(System.getProperty("native-image-installed")) ||
    "true".equalsIgnoreCase(System.getenv("NATIVE_IMAGE_INSTALLED")) ||
    "true".equalsIgnoreCase(System.getProperty("graalvm-installed")) ||
    "true".equalsIgnoreCase(System.getenv("GRAALVM_INSTALLED"))
  }

  // Path to GraalVM home directory.
  lazy val nativeImageGraalHome: Path = {
    if (nativeImageInstalled) {
      Paths.get {
        List("GRAAL_HOME", "GRAALVM_HOME", "JAVA_HOME").iterator
          .map(key => Option(System.getenv(key)))
          .collectFirst { case Some(value) => value }
          .getOrElse("")
      }
    } else {
      val coursier = nativeImageCoursier().toAbsolutePath.toString
      val svm = nativeImageVersion
      val jvm = nativeImageJvm
      val index = nativeImageJvmIndex
      Paths.get(Process(List(coursier, "java-home", "--jvm-index", index, "--jvm", s"$jvm:$svm")).!!.trim)
    }
  }

  // The command arguments to launch the GraalVM native-image binary
  lazy val nativeImageCommand: Seq[String] = {
    val graalHome = nativeImageGraalHome
    if (nativeImageInstalled) {
      val binary = if (Properties.isWin) "native-image.cmd" else "native-image"
      val path = graalHome.resolve("bin").resolve(binary)
      List[String](path.toString)
    } else {
      val cmd =
        if (Properties.isWin)
          ".cmd"
        else
          ""
      val ni = graalHome.resolve("bin").resolve(s"native-image$cmd")
      if (!Files.isExecutable(ni)) {
        val gu = ni.resolveSibling(s"gu$cmd")
        Process(List(gu.toString, "install", "native-image")).!
      }
      if (!Files.isExecutable(ni)) {
        throw new MessageOnlyException(
          "Failed to automatically install native-image. " +
            "To fix this problem, install native-image manually and start sbt with " +
            "the environment variable 'NATIVE_IMAGE_INSTALLED=true'"
        )
      }
      List(ni.toString)
    }
  }
//    // Run application, tracking all usages of dynamic features of an execution with `native-image-agent`.
//    def nativeImageRunAgent: Unit = {
//      val _ = nativeImageCommand
//      val graalHome = nativeImageGraalHome.toFile
//      val agentConfig = if (nativeImageAgentMerge) "config-merge-dir" else "config-output-dir"
//      val agentOption = s"-agentlib:native-image-agent=$agentConfig=${nativeImageAgentOutputDir}"
//      val tpr = thisProjectRef.value
//      val settings = Seq(
//        fork in (tpr, Compile, run) := true,
//        javaHome in (tpr, Compile, run) := Some(graalHome),
//        javaOptions in (tpr, Compile, run) += agentOption
//      )
//      val state0 = state.value
//      val extracted = Project.extract(state0)
//      val newState = extracted.append(settings, state0)
//      val arguments = spaceDelimited("<arg>").parsed
//      val input = if (arguments.isEmpty) "" else arguments.mkString(" ")
//      Project
//        .extract(newState)
//        .runInputTask(run in (tpr, Compile), input, newState)
//    }

  // Directory where `native-image-agent` should put generated configurations.
  def nativeImageAgentOutputDir: Path = target.resolve("native-image-configs")

  // Whether `native-image-agent` should merge generated configurations.
  // See https://www.graalvm.org/reference-manual/native-image/BuildConfiguration/#assisted-configuration-of-native-image-builds for details
  def nativeImageAgentMerge: Boolean = false

  // Generate a native image for this project.
  def nativeImage(): Path = {
    val main = project.platform.flatMap(_.mainClass)
    val binaryName = nativeImageOutput

    val projectClassPath =
      // bloopProject.project.classesDir todo: bloop doesn't put the files where we want it to
      project.directory.resolve(s".bloop/${project.name}/bloop-bsp-clients-classes/classes-bloop-cli")

    val cp = List(
      List(projectClassPath),
      project.resources.getOrElse(Nil),
      project.classpath
    ).flatten

    // NOTE(olafur): we pass in a manifest jar instead of the full classpath
    // for two reasons:
    // * large classpaths quickly hit on the "argument list too large"
    //   error, especially on Windows.
    // * we print the full command to the console and the manifest jar makes
    //   it more readable and easier to copy-paste.
    val manifest = targetNativeImageInternal.resolve("manifest.jar")
    Files.createDirectories(manifest.getParent)
    createManifestJar(manifest, cp)
    val nativeClasspath = manifest.toAbsolutePath.toString

    // Assemble native-image argument list.
    val command = mutable.ListBuffer.empty[String]
    command ++= nativeImageCommand
    command += "-cp"
    command += nativeClasspath
    command ++= nativeImageOptions
    command +=
      main.getOrElse(
        throw new MessageOnlyException(
          "no mainClass is specified. " +
            "To fix this problem, update build.sbt to include the settings " +
            "`mainClass.in(Compile) := Some(\"com.MainClass\")`"
        )
      )
    command += binaryName.toAbsolutePath.toString

    // Start native-image linker.
    println(command.mkString(" "))
    val cwd = targetNativeImage
    Files.createDirectories(cwd)
    val exit = Process(command, cwd = Some(cwd.toFile)).!
    if (exit != 0) { throw new MessageOnlyException(s"native-image command failed with exit code '$exit'") }
    println(s"Native image ready at ${binaryName.toAbsolutePath}!")
    binaryName
  }

  // Run the generated native-image binary without linking
  def nativeImageRun(arguments: List[String]): Unit = {
    val binary = nativeImageOutput
    if (!Files.isRegularFile(binary)) {
      throw new MessageOnlyException(
        s"no such file: $binary.\nTo fix this problem, run 'nativeImage' first."
      )
    }
    val exit = Process(binary.toAbsolutePath.toString :: arguments).!
    if (exit != 0) {
      throw new MessageOnlyException(s"non-zero exit: $exit")
    }
  }

  private def copyResource(filename: String, outDir: Path): Path = {
    Files.createDirectories(outDir)
    val in = this.getClass.getResourceAsStream(s"/sbt-native-image/${filename}")
    if (in == null) {
      throw new MessageOnlyException(
        "unable to find coursier binary via resources. " +
          "To fix this problem, define the `nativeImageCoursier` task to return the path to a Coursier binary."
      )
    }
    val out = outDir.resolve(filename)
    Files.copy(in, out, StandardCopyOption.REPLACE_EXISTING)
    out.toFile.setExecutable(true)
    out
  }

  private def createManifestJar(manifestJar: Path, cp: Seq[Path]): Unit = {
    // Add trailing slash to directories so that manifest dir entries work
    val classpathStr = cp.map(addTrailingSlashToDirectories(manifestJar)).mkString(" ")
    val manifest = new Manifest()
    manifest.getMainAttributes.put(Attributes.Name.MANIFEST_VERSION, "1.0")
    manifest.getMainAttributes.put(Attributes.Name.CLASS_PATH, classpathStr)
    val out = Files.newOutputStream(manifestJar)
    // This needs to be declared since jos itself should be set to close as well.
    var jos: JarOutputStream = null
    try jos = new JarOutputStream(out, manifest)
    finally if (jos == null) {
      out.close()
    } else {
      jos.close()
    }
  }

  private def addTrailingSlashToDirectories(manifestJar: Path)(path: Path): String = {
    val syntax: String =
      if (Properties.isWin) {
        // NOTE(danirey): absolute paths are not supported by all JDKs on Windows, therefore using relative paths
        // relative paths may not be URL-encoded, otherwise an absolute path is constructed
        val manifestPath = manifestJar.getParent
        val dependencyPath = path
        try manifestPath.relativize(dependencyPath).toString
        catch {
          //java.lang.IllegalArgumentException: 'other' has different root
          //this happens if the dependency jar resides on a different drive then the manifest, i.e. C:\Coursier\Cache and D:\myapp\target
          //copy dependency next to manifest as fallback
          case _: IllegalArgumentException =>
            import java.nio.file.Files
            import java.nio.file.StandardCopyOption
            Files.copy(
              dependencyPath,
              manifestPath.resolve(path.getFileName),
              StandardCopyOption.REPLACE_EXISTING
            )
            path.getFileName.toString
        }
      } else {
        // NOTE(olafur): manifest jars must use URL-encoded paths.
        // https://docs.oracle.com/javase/7/docs/technotes/guides/jar/jar.html
        path.toUri.getPath
      }

    val separatorAdded = {
      if (syntax.endsWith(".jar") || syntax.endsWith(File.separator)) {
        syntax
      } else {
        syntax + File.separator
      }
    }
    separatorAdded
  }
}
