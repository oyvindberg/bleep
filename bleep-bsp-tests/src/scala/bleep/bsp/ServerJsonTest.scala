package bleep.bsp

import bleep.BleepException
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}

/** `server.json` is the only thing that can name a daemon once it is dead.
  *
  * The socket directory is named after an irreversible SHA-256 of version + JVM + options, and a dead daemon cannot answer `bleep/status`. Without this file
  * `bleep server ls` can show that 400MB of socket directory exists but not what produced it, and `bleep server restart` has no command to replay.
  */
class ServerJsonTest extends AnyFunSuite with Matchers {

  private def tempDir(): Path = {
    val d = Files.createTempDirectory("bleep-server-json")
    d.toFile.deleteOnExit()
    d
  }

  private val sample = ServerJson(
    bleepVersion = "1.0.0-M11",
    jvmName = "graalvm-community",
    jvmVersion = "25.0.1",
    javaBin = "/opt/jvm/bin/java",
    javaOpts = List("-Xmx8g", "-XX:+UseZGC"),
    serverMainClass = "bleep.bsp.BspServerDaemon",
    command = List("/opt/jvm/bin/java", "-Xmx8g", "-cp", "a.jar", "bleep.bsp.BspServerDaemon"),
    workingDir = "/home/dev/project",
    spawnedAtEpochMs = 1754600000000L
  )

  test("round-trips through the socket directory") {
    val dir = tempDir()
    ServerJson.write(dir, sample)
    ServerJson.read(dir) shouldBe Some(sample)
  }

  test("absent file reads as None — daemons spawned before this file existed are a real state, not an error") {
    ServerJson.read(tempDir()) shouldBe None
  }

  test("malformed json throws rather than being silently treated as absent") {
    val dir = tempDir()
    Files.writeString(ServerJson.file(dir), "{ this is not json")
    a[BleepException.InvalidJson] should be thrownBy ServerJson.read(dir)
  }

  test("json of the wrong shape throws — a directory we cannot make sense of is a bug to surface") {
    val dir = tempDir()
    Files.writeString(ServerJson.file(dir), """{"bleepVersion":"1.0.0-M11"}""")
    a[BleepException.InvalidJson] should be thrownBy ServerJson.read(dir)
  }

  test("unknown fields are tolerated so a newer daemon's file stays readable by an older client") {
    val dir = tempDir()
    val withExtra = io.circe.parser
      .parse(io.circe.syntax.EncoderOps(sample).asJson.noSpaces)
      .toOption
      .get
      .asObject
      .get
      .add("somethingFromTheFuture", io.circe.Json.fromString("x"))
    Files.writeString(ServerJson.file(dir), io.circe.Json.fromJsonObject(withExtra).noSpaces)
    ServerJson.read(dir) shouldBe Some(sample)
  }
}
