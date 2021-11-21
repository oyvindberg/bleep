package bleep

import com.monovore.decline.Opts

case class CommonOpts()

object CommonOpts {
  val opts: Opts[CommonOpts] =
    Opts(CommonOpts())
}
