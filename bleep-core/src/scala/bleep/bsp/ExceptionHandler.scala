package bleep
package bsp

import bleep.internal.Throwables
import bleep.logging.Logger
import org.eclipse.lsp4j.jsonrpc.messages.{ResponseError, ResponseErrorCode}
import org.eclipse.lsp4j.jsonrpc.{MessageIssueException, ResponseErrorException}

import java.util.function.Function

// the default exception handler doesn't look deep enough in an exception chain to dig out the `ResponseErrorException`
// good that after all these years java libraries can still disappoint.
class ExceptionHandler(logger: Logger) extends Function[Throwable, ResponseError] {
  override def apply(th: Throwable): ResponseError =
    Throwables.tryExtract(classOf[ResponseErrorException])(th) match {
      case Some(found) => found.getResponseError
      case None =>
        Throwables.tryExtract(classOf[MessageIssueException])(th) match {
          case Some(mie) =>
            // parse and propagate? it has a similar shape as `ResponseError`
            logger.warn(s"Got error from bloop. getMessage: ${mie.getMessage}, getRpcMessage: ${mie.getRpcMessage}")
          case None =>
        }
        default(th)

    }

  def default(th: Throwable): ResponseError = {
    val header = "Internal error"
    logger.warn(header, th)
    val error = new ResponseError
    error.setMessage(header + ".")
    error.setCode(ResponseErrorCode.InternalError)
    error.setData(Throwables.asString(th))
    error
  }
}
