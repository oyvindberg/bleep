package bleep
package bsp

import ryddig.Logger

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import scala.jdk.CollectionConverters.*

/** Opens every package of `jdk.compiler` to classpath code in the compile server. javac runs inside the server, and javac plugins which reach into its
  * internals need them: error prone and nullaway (`-Xplugin:ErrorProne`) fail with an `IllegalAccessError` otherwise. A maven build lists the same packages in
  * `.mvn/jvm.config`. Only the compiler is opened, the rest of the JDK stays encapsulated.
  *
  * Which packages `jdk.compiler` has is the JDK's own business, so the JDK is asked: `java --describe-module jdk.compiler`. That takes about a second, so the
  * answer is kept per java binary, and a JDK is asked once.
  */
object JdkCompilerOpens {
  def apply(javaBin: Path, cacheDir: Path, logger: Logger): List[String] =
    packages(javaBin, cacheDir, logger).flatMap(pkg => List("--add-opens", s"jdk.compiler/$pkg=ALL-UNNAMED"))

  def packages(javaBin: Path, cacheDir: Path, logger: Logger): List[String] = {
    val real = javaBin.toRealPath()
    // a JDK replaced in place at the same path is asked again
    val key = s"$real ${Files.getLastModifiedTime(real).toMillis}"
    val digest = MessageDigest.getInstance("SHA-256").digest(key.getBytes(StandardCharsets.UTF_8)).take(12).map("%02x".format(_)).mkString
    val cacheFile = cacheDir.resolve("jdk-compiler-packages").resolve(s"$digest.txt")

    if (Files.isRegularFile(cacheFile)) Files.readAllLines(cacheFile).asScala.toList.filter(_.nonEmpty)
    else {
      val packages = describe(real)
      logger.withContext("java", real.toString).debug(s"jdk.compiler has ${packages.size} packages")
      Files.createDirectories(cacheFile.getParent)
      // written whole and moved into place, so a server starting at the same time never reads half of it
      val tmp = Files.createTempFile(cacheFile.getParent, digest, ".tmp")
      Files.writeString(tmp, packages.mkString("\n"))
      Files.move(tmp, cacheFile, java.nio.file.StandardCopyOption.REPLACE_EXISTING, java.nio.file.StandardCopyOption.ATOMIC_MOVE)
      packages
    }
  }

  /** The packages in the output of `java --describe-module jdk.compiler`: `exports p`, `qualified exports p to ..`, `opens p`, `qualified opens p to ..`, and
    * `contains p` for those it keeps to itself
    */
  def parse(output: String): List[String] =
    output.linesIterator
      .map(_.trim.split("\\s+").toList)
      .collect {
        case ("exports" | "opens" | "contains") :: pkg :: _   => pkg
        case "qualified" :: ("exports" | "opens") :: pkg :: _ => pkg
      }
      .toList
      .distinct
      .sorted

  private def describe(javaBin: Path): List[String] = {
    val process = new ProcessBuilder(javaBin.toString, "--describe-module", "jdk.compiler").redirectErrorStream(true).start()
    process.getOutputStream.close()
    val output = new String(process.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
    val exit = process.waitFor()
    val packages = parse(output)
    if (exit != 0 || packages.isEmpty)
      throw new BleepException.Text(s"Could not list the packages of jdk.compiler with `$javaBin --describe-module jdk.compiler` (exit $exit):\n$output")
    packages
  }
}
