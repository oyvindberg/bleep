package bleep.mcp

import cats.effect.{Deferred, IO}
import cats.effect.unsafe.implicits.global
import ch.linkyard.mcp.jsonrpc2.JsonRpcConnection
import ch.linkyard.mcp.jsonrpc2.transport.LineBasedJsonRpcConnection
import fs2.Stream
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** The MCP server exits when its client closes stdin. linkyard never reports that, so [[McpServerRunner.endingWith]] does; without it every ended session left
  * a server running.
  */
class McpServerRunnerTest extends AnyFunSuite with Matchers {

  private def connection(input: Stream[IO, Byte]): JsonRpcConnection[IO] =
    new LineBasedJsonRpcConnection[IO](input, _.drain, JsonRpcConnection.Info.Stdio(Map.empty))

  private val ping = """{"jsonrpc":"2.0","id":1,"method":"ping"}""" + "\n"

  private def endOf(input: Stream[IO, Byte]): (Int, Either[Throwable, Unit]) = {
    for {
      ended <- Deferred[IO, Either[Throwable, Unit]]
      read <- McpServerRunner.endingWith(connection(input), ended).in.compile.count.attempt
      result <- ended.tryGet
    } yield (read.fold(_ => -1, _.toInt), result.getOrElse(fail("input stopped but nobody was told")))
  }.unsafeRunSync()

  test("end of input is reported, after every message on it was read") {
    endOf(Stream.emits(ping.getBytes).covary[IO]) shouldBe ((1, Right(())))
  }

  test("an input that fails is reported as a failure, not as a clean end") {
    val (_, result) = endOf(Stream.emits(ping.getBytes).covary[IO] ++ Stream.raiseError[IO](new java.io.IOException("pipe broke")))
    result.left.map(_.getMessage) shouldBe Left("pipe broke")
  }

  test("an input that never ends is not reported as ended") {
    val notEnded = (for {
      ended <- Deferred[IO, Either[Throwable, Unit]]
      _ <- McpServerRunner
        .endingWith(connection(Stream.never[IO]), ended)
        .in
        .compile
        .drain
        .timeoutTo(scala.concurrent.duration.DurationInt(200).millis, IO.unit)
      result <- ended.tryGet
    } yield result).unsafeRunSync()
    notEnded shouldBe None
  }
}
