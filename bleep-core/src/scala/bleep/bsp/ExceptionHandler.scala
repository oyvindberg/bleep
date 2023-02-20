package bleep
package bsp

import bleep.logging.Logger
import org.eclipse.lsp4j.jsonrpc.ResponseErrorException
import org.eclipse.lsp4j.jsonrpc.messages.{ResponseError, ResponseErrorCode}

import java.io.{ByteArrayOutputStream, PrintWriter}
import java.util.function.Function

// the default exception handler doesn't look deep enough in an exception chain to dig out the `ResponseErrorException`
// good that after all these years java libraries can still disappoint.
class ExceptionHandler(logger: Logger) extends Function[Throwable, ResponseError] {
  override def apply(th: Throwable): ResponseError =
    ExceptionHandler.look(classOf[ResponseErrorException])(th) match {
      case Some(found) => found.getResponseError
      case None        => default(th)
    }

  def default(th: Throwable): ResponseError = {
    val header = "Internal error"
    logger.warn(header, th)
    val error = new ResponseError
    error.setMessage(header + ".")
    error.setCode(ResponseErrorCode.InternalError)
    val stackTrace = new ByteArrayOutputStream
    val stackTraceWriter = new PrintWriter(stackTrace)
    th.printStackTrace(stackTraceWriter)
    stackTraceWriter.flush()
    error.setData(stackTrace.toString)
    error
  }
}

object ExceptionHandler {
  def look[T <: Throwable](clazz: Class[T])(th: Throwable): Option[T] =
    if (clazz.isInstance(th)) Some(th.asInstanceOf[T])
    else Option(th.getCause).flatMap(look(clazz)).orElse(th.getSuppressed.toList.flatMap(look(clazz)).headOption)
}
