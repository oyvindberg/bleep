package bleep.bsp

import bleep.model
import io.circe.Json
import io.circe.syntax._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.io.ByteArrayInputStream

/** `bleep bsp` sits between an IDE and the daemon and now edits the traffic rather than copying it. These cover the parts of that which are easy to get subtly
  * wrong and which no integration test reaches — the IDE path has no automated coverage at all.
  */
class BspProxyMessageTest extends AnyFunSuite with Matchers {

  private val payload: BspBuildData.Payload =
    BspBuildData.Payload.of(
      variantName = "bsp",
      build = model.Build.Exploded(
        $version = model.BleepVersion("1.0.0-test"),
        explodedProjects = Map.empty,
        resolvers = model.JsonList.empty,
        jvm = None,
        scripts = Map.empty,
        remoteCache = None
      ),
      resolvedProjects = Map.empty
    )

  private def framed(body: String): Array[Byte] =
    (s"Content-Length: ${body.getBytes("UTF-8").length}\r\n\r\n" + body).getBytes("UTF-8")

  private def parse(bytes: Array[Byte]): Json =
    io.circe.parser.parse(new String(bytes, "UTF-8")).fold(throw _, identity)

  test("payload is added without disturbing the IDE's own data") {
    // Metals puts semanticdbVersion/javaSemanticdbVersion in `data`, and the server reads them from
    // there. Overwriting `data` with our payload would silently cost Metals its semanticDB support.
    val initJson = parse(
      framedBody(
        Json.obj(
          "jsonrpc" -> "2.0".asJson,
          "id" -> 1.asJson,
          "method" -> "build/initialize".asJson,
          "params" -> Json.obj(
            "displayName" -> "Metals".asJson,
            "rootUri" -> "file:///tmp/x".asJson,
            "data" -> Json.obj(
              "semanticdbVersion" -> "4.15.2".asJson,
              "javaSemanticdbVersion" -> "0.10.0".asJson
            )
          )
        )
      )
    )

    val patched = parse(BspProxy.withBuildPayload(initJson, payload))
    val data = patched.hcursor.downField("params").downField("data")

    data.get[String]("semanticdbVersion") shouldBe Right("4.15.2")
    data.get[String]("javaSemanticdbVersion") shouldBe Right("0.10.0")
    data.get[BspBuildData.Payload](BspBuildData.DataField) shouldBe Right(payload)
    patched.hcursor.downField("params").get[String]("dataKind") shouldBe Right(BspBuildData.DataKind)

    // everything else survives untouched
    patched.hcursor.get[String]("method") shouldBe Right("build/initialize")
    patched.hcursor.downField("params").get[String]("rootUri") shouldBe Right("file:///tmp/x")
  }

  test("payload is added when the client sent no data at all") {
    val initJson = parse(
      framedBody(
        Json.obj(
          "jsonrpc" -> "2.0".asJson,
          "id" -> 1.asJson,
          "method" -> "build/initialize".asJson,
          "params" -> Json.obj("displayName" -> "SomeClient".asJson)
        )
      )
    )

    val patched = parse(BspProxy.withBuildPayload(initJson, payload))
    patched.hcursor.downField("params").downField("data").get[BspBuildData.Payload](BspBuildData.DataField) shouldBe Right(payload)
  }

  test("buildChanged notification round-trips the payload and has no id") {
    val json = parse(BspProxy.buildChangedNotification(payload))
    json.hcursor.get[String]("method") shouldBe Right(bleep.bsp.protocol.BleepBspProtocol.BuildChanged)
    json.hcursor.get[String]("jsonrpc") shouldBe Right("2.0")
    json.hcursor.downField("id").succeeded shouldBe false // a notification, not a request
    json.hcursor.get[BspBuildData.Payload]("params") shouldBe Right(payload)
  }

  test("reads consecutive framed messages and stops at end of stream") {
    val stream = new ByteArrayInputStream(framed("""{"method":"a"}""") ++ framed("""{"method":"b"}"""))

    new String(BspProxy.readMessage(stream).get, "UTF-8") shouldBe """{"method":"a"}"""
    new String(BspProxy.readMessage(stream).get, "UTF-8") shouldBe """{"method":"b"}"""
    BspProxy.readMessage(stream) shouldBe None
  }

  test("reads a message whose body is multi-byte UTF-8") {
    // Content-Length counts bytes, not characters. Reading by character would desync the stream
    // for every subsequent message.
    val body = """{"method":"a","note":"pluss – nød"}"""
    val stream = new ByteArrayInputStream(framed(body) ++ framed("""{"method":"b"}"""))

    new String(BspProxy.readMessage(stream).get, "UTF-8") shouldBe body
    new String(BspProxy.readMessage(stream).get, "UTF-8") shouldBe """{"method":"b"}"""
  }

  test("recognises workspace/reload and nothing else") {
    BspProxy.isReloadRequest("""{"method":"workspace/reload","id":3}""".getBytes("UTF-8")) shouldBe true
    BspProxy.isReloadRequest("""{"method":"buildTarget/compile","id":3}""".getBytes("UTF-8")) shouldBe false
    BspProxy.isReloadRequest("""not json""".getBytes("UTF-8")) shouldBe false
  }

  private def framedBody(json: Json): Array[Byte] = json.noSpaces.getBytes("UTF-8")
}
