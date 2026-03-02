package bleep.bsp

import bleep.BleepException
import cats.effect.IO
import cats.syntax.all._
import java.util.concurrent.{CompletableFuture, ExecutionException}
import java.io.IOException

/** Utilities for handling BSP request cancellation.
  *
  * Provides helpers to properly cancel BSP requests when:
  *   - The IO fiber is cancelled (e.g., user presses Ctrl-C)
  *   - A timeout is exceeded
  *   - An error occurs
  *
  * Usage:
  * {{{
  * // Wrap BSP call to support cancellation
  * val result = BspRequestHelper.callCancellable(server.buildTargetCompile(params)).unsafeRunSync()
  * }}}
  */
object BspRequestHelper {

  /** Execute a BSP request as an IO, supporting proper cancellation.
    *
    * When the IO is cancelled (e.g., fiber cancellation from Ctrl-C), the underlying CompletableFuture is also cancelled, which triggers lsp4j to send
    * $/cancelRequest to the server.
    *
    * @param future
    *   The CompletableFuture returned by an lsp4j BSP method call
    * @return
    *   IO that completes with the result, or is cancelled if the fiber is cancelled
    */
  def callCancellable[T](future: => CompletableFuture[T]): IO[T] =
    IO.defer {
      val f = future
      IO.interruptible(f.get())
        .adaptError {
          case e: ExecutionException if isBrokenPipe(e) =>
            new BleepException.Cause(e, "BSP server connection lost (server may have crashed)")
          case e: IOException if e.getMessage != null && e.getMessage.contains("Broken pipe") =>
            new BleepException.Cause(e, "BSP server connection lost (server may have crashed)")
        }
        .onCancel(IO.blocking {
          // When the IO is cancelled, cancel the underlying future
          // This triggers lsp4j to send $/cancelRequest
          // Suppress errors if server already died (broken pipe)
          try f.cancel(true)
          catch { case _: Exception => () }
          ()
        })
    }

  private def isBrokenPipe(e: Throwable): Boolean = {
    var current: Throwable = e
    while (current != null) {
      current match {
        case io: IOException if io.getMessage != null && io.getMessage.contains("Broken pipe") => return true
        case _                                                                                 => ()
      }
      current = current.getCause
    }
    false
  }

  /** Execute a BSP request, racing against the connection's listening future.
    *
    * When the BSP server dies, lsp4j does NOT fail outstanding request futures — they hang forever. The listening future (from launcher.startListening())
    * completes when the reader thread exits. Racing against it ensures we detect server death immediately.
    *
    * @param future
    *   The CompletableFuture returned by an lsp4j BSP method call
    * @param listening
    *   The listening future from launcher.startListening()
    * @return
    *   IO that completes with the result, or fails if the connection dies
    */
  def callCancellable[T](future: => CompletableFuture[T], listening: java.util.concurrent.Future[Void]): IO[T] = {
    val request = callCancellable(future)
    val connectionDied = IO.interruptible(listening.get()).attempt.flatMap { _ =>
      IO.raiseError[T](new BleepException.Text("BSP server connection lost (server may have crashed)"))
    }
    IO.race(request, connectionDied).flatMap {
      case Left(result) => IO.pure(result)
      case Right(_)     => IO.raiseError(new BleepException.Text("BSP server connection lost (server may have crashed)"))
    }
  }

  /** Execute a BSP request with a timeout, supporting cancellation.
    *
    * @param future
    *   The CompletableFuture returned by an lsp4j BSP method call
    * @param timeout
    *   Maximum time to wait
    * @return
    *   IO that completes with the result, fails with TimeoutException, or is cancelled
    */
  def callWithTimeout[T](future: => CompletableFuture[T], timeout: scala.concurrent.duration.FiniteDuration): IO[T] =
    callCancellable(future).timeout(timeout)
}
