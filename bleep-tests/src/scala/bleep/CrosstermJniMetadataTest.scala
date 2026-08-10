package bleep

import io.circe.parser.decode
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path, Paths}
import java.util.zip.ZipFile
import scala.jdk.CollectionConverters._

/** The crossterm native library constructs whichever event class the terminal produced and looks it up with JNI `FindClass`. native-image only keeps classes
  * something references, so any event kind our Scala never mentions by name is stripped from the binary — and the lookup then fails at runtime with
  * `NoClassDefFoundError`, killing the TUI the first time someone produces that event.
  *
  * That is not hypothetical: horizontal scroll on a trackpad produces `MouseEventKind$ScrollLeft`, which nothing matched on, and `bleep server top` died with
  * exactly that error. Note the old tui-scala crossterm had no `ScrollLeft` at all — the binding gained event kinds, so this failure mode arrived with it.
  *
  * The fix registers every class in the package rather than the one that was reported. This test is what keeps that registration honest: it fails when jatatui
  * adds a class, which is the moment to regenerate, rather than the moment a user finds it.
  */
class CrosstermJniMetadataTest extends AnyFunSuite with Matchers {

  private val metadataFile: Path =
    Paths.get("bleep-cli/src/resources/META-INF/native-image/build.bleep/bleep-cli/reachability-metadata.json")

  /** Every class in `tui.crossterm`, read from the jar actually on this test's classpath. */
  private def classesInJar: Set[String] = {
    val location = classOf[tui.crossterm.CrosstermJni].getProtectionDomain.getCodeSource.getLocation
    val jar = Paths.get(location.toURI)
    withClue(s"expected a jar, got $jar: ")(jar.toString should endWith(".jar"))

    val zip = new ZipFile(jar.toFile)
    try
      zip
        .entries()
        .asScala
        .map(_.getName)
        .filter(name => name.startsWith("tui/crossterm/") && name.endsWith(".class"))
        .map(name => name.dropRight(".class".length).replace('/', '.'))
        .toSet
    finally zip.close()
  }

  private def registered: Set[String] = {
    val json = decode[io.circe.Json](Files.readString(metadataFile)).fold(throw _, identity)
    val types = json.hcursor.downField("jni").as[List[io.circe.Json]].fold(throw _, identity)
    types.flatMap(_.hcursor.get[String]("type").toOption).toSet
  }

  test("every class the crossterm binding ships is registered for JNI") {
    val missing = classesInJar -- registered

    withClue(
      s"${missing.size} class(es) in the crossterm jar are not registered, so native-image will strip them and FindClass will fail at runtime: " +
        s"${missing.toList.sorted.take(10).mkString(", ")}. Regenerate $metadataFile from the jar. "
    )(missing shouldBe empty)
  }

  test("the registration does not name classes the jar no longer has") {
    val stale = registered -- classesInJar

    withClue(s"registered but absent from the jar: ${stale.toList.sorted.take(10).mkString(", ")}. ")(stale shouldBe empty)
  }

  test("the event kinds that crashed the dashboard are registered by name") {
    // Named explicitly so the reason this file exists survives a future refactor of the check above.
    registered should contain("tui.crossterm.MouseEventKind$ScrollLeft")
    registered should contain("tui.crossterm.MouseEventKind$ScrollRight")
  }

  test("reflection is registered alongside JNI, since the same classes are reached both ways") {
    val json = decode[io.circe.Json](Files.readString(metadataFile)).fold(throw _, identity)
    val reflection = json.hcursor
      .downField("reflection")
      .as[List[io.circe.Json]]
      .fold(throw _, identity)
      .flatMap(_.hcursor.get[String]("type").toOption)
      .toSet

    reflection shouldBe registered
  }
}
