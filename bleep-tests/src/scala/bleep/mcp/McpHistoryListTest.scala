package bleep.mcp

import bleep.history.Transcript
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** bleep.history.list over MCP: the most recent runs only, each naming just its first targets, filterable by mode and pageable back in time. */
class McpHistoryListTest extends AnyFunSuite with Matchers {

  private def transcript(id: Long, mode: String): Transcript =
    Transcript(
      id = id,
      timestampMs = id,
      workspace = "/ws",
      variant = "normal",
      mode = mode,
      targets = (1 to 40).map(i => s"p$i").toList,
      client = "bleep-mcp",
      events = Nil,
      testRunResult = None
    )

  private val history = (1L to 30L).map(id => transcript(id, if (id % 3 == 0) "test" else "compile")).toList

  private def ids(json: io.circe.Json): List[Long] =
    json.hcursor.downField("entries").values.get.toList.map(_.hcursor.get[Long]("historyId").toOption.get)

  test("lists the most recent entries, oldest first, with targetCount and the first few targets") {
    val json = BleepMcpServer.historyListing(history, mode = None, limit = 20, offset = 0)
    ids(json) shouldBe (11L to 30L).toList
    json.hcursor.get[Int]("total") shouldBe Right(30)
    json.hcursor.get[Int]("olderOffset") shouldBe Right(20)
    val entry = json.hcursor.downField("entries").downArray
    entry.get[Int]("targetCount") shouldBe Right(40)
    entry.get[List[String]]("targets") shouldBe Right(List("p1", "p2", "p3"))
  }

  test("offset pages back to older entries, and the last page has no olderOffset") {
    val json = BleepMcpServer.historyListing(history, mode = None, limit = 20, offset = 20)
    ids(json) shouldBe (1L to 10L).toList
    json.hcursor.get[Int]("olderOffset").toOption shouldBe None
  }

  test("mode filters before paging") {
    val json = BleepMcpServer.historyListing(history, mode = Some("test"), limit = 3, offset = 0)
    ids(json) shouldBe List(24L, 27L, 30L)
    json.hcursor.get[Int]("total") shouldBe Right(10)
  }
}
