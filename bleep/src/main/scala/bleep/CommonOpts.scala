package bleep

import com.monovore.decline.Opts
import cats.syntax.apply._
case class CommonOpts(noColor: Boolean, debug: Boolean)

object CommonOpts {
  val noColor: Opts[Boolean] = Opts.flag("no-color", "enable CI-friendly output").orFalse
  val debug: Opts[Boolean] = Opts.flag("debug", "enable more output").orFalse

  val opts: Opts[CommonOpts] =
    (noColor, debug).mapN { case (noColor, debug) =>
      CommonOpts(noColor = noColor, debug = debug)
    }
}
