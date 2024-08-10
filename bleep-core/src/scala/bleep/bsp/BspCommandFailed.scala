package bleep
package bsp

import ch.epfl.scala.bsp4j
import org.eclipse.lsp4j.jsonrpc.messages.ResponseError

case class BspCommandFailed(what: String, projects: Array[model.CrossProjectName], reason: BspCommandFailed.Reason)
    extends BleepException(s"$what ${projects.map(_.value).mkString(", ")} ${reason.str}", reason.throwable.orNull)

object BspCommandFailed {
  sealed trait Reason {
    def str: String = this match {
      case NoDetails                  => ""
      case StatusCode(code)           => s"failed status code $code"
      case FoundResponseError(value)  => s"failed with response error $value"
      case FailedWithException(value) => s"failed with exception: ${value.getMessage}"
    }
    def throwable: Option[Throwable] = this match {
      case FailedWithException(value) => Some(value)
      case _                          => None
    }
  }
  case object NoDetails extends Reason
  case class StatusCode(code: bsp4j.StatusCode) extends Reason
  case class FoundResponseError(value: ResponseError) extends Reason
  case class FailedWithException(value: Throwable) extends Reason
}
