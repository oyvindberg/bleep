package bleep
package internal

import cats.data.Validated
import com.monovore.decline.Opts

/** The JVM an imported build is compiled and run with, always temurin. Without one the build runs on whatever JVM happens to be installed, and the newest ones
  * break older compilers: scala 2.12 cannot read their class files, and scala 2.13 rejects `Ordering`s when `Comparator` gets methods of its own. Java 17 is
  * the lowest bleep-bsp supports, and compiles code for older java versions with `--release`
  */
object importJvm {
  val lowest: Int = 17

  val opts: Opts[Option[Int]] =
    Opts
      .option[Int](
        "build-jvm",
        s"java version of the (temurin) JVM the imported build uses. At least $lowest, which is the default unless the build compiles for a newer java version"
      )
      .mapValidated(version =>
        if (version >= lowest) Validated.validNel(version) else Validated.invalidNel(s"--build-jvm must be at least $lowest, got $version")
      )
      .orNone

  /** The java version code is compiled for, where compiler options say: `--release 17` or `-target 1.8` for javac, `-release 11`, `-release:11` or
    * `-java-output-version 21` for scalac. The JVM running the build must be at least that new
    */
  def compiledFor(options: model.Options): List[Int] = {
    def version(v: String): Option[Int] = v.stripPrefix("jvm-").stripPrefix("1.").toIntOption
    options.values.toList.flatMap(_.render match {
      case List("--release" | "-release" | "-target" | "-java-output-version", v)    => version(v)
      case List(flag) if flag.startsWith("-release:") || flag.startsWith("-target:") => version(flag.dropWhile(_ != ':').drop(1))
      case _                                                                         => None
    })
  }

  /** @param chosen
    *   `--build-jvm`
    * @param javaRelease
    *   the java version the build compiles for, if it says
    */
  def apply(chosen: Option[Int], javaRelease: Option[Int]): model.Jvm = {
    val version = chosen.getOrElse(javaRelease.fold(lowest)(release => math.max(release, lowest)))
    if (version < lowest) throw new BleepException.Text(s"Imported builds need java $lowest at least, got $version")
    model.Jvm(s"temurin:$version", None)
  }
}
