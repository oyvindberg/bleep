package bleep
package packaging

import coursier.core.Info

sealed trait PublishLayout

object PublishLayout {
  case class Maven(info: Info = Info.empty) extends PublishLayout

  case object Ivy extends PublishLayout
}
