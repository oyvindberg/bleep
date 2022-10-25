package bleep.logging

import fansi.Str
import sourcecode.Text

trait Pattern {
  def apply[T: Formatter](t: => Text[T], throwable: Option[Throwable], metadata: Metadata, ctx: Ctx, path: List[String]): Str
}
