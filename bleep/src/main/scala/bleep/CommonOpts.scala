package bleep

import com.monovore.decline.Opts

case class CommonOpts(noColor: Boolean)

object CommonOpts {
  val opts: Opts[CommonOpts] =
    Opts.flag("no-color", "enable CI-friendly output").orNone.map { maybeNoColor =>
      CommonOpts(noColor = maybeNoColor.isDefined)
    }
}
