package bleep.logging

import java.io.{File, PrintWriter, StringWriter}

import fansi.{Color, EscapeAttr, Str}
import sourcecode.Text

trait Pattern {
  def apply[T: Formatter](t: => Text[T], throwable: Option[Throwable], metadata: Metadata, ctx: Ctx): Str
}

object Pattern {
  def prefixFor(l: LogLevel): String =
    f"[${l.name.value}%-5s]"

  @inline def colorFor(l: LogLevel): EscapeAttr =
    (l.level: @unchecked) match {
      // https://www.ditig.com/256-colors-cheat-sheet
      // chosen to be somewhat subtle (though I guess that's up for discussion), and to work both on white/black background
      case LogLevel.trace.level => Color.Full(223) /* 223 NavajoWhite1	#ffd7af	rgb(255,215,175)	hsl(30,100%,84%) */
      case LogLevel.debug.level => Color.Full(127) /* 127 Magenta3	#af00af	rgb(175,0,175)	hsl(300,100%,34%) */
//      case LogLevel.info.level  => Color.Full(126) /* 126 MediumVioletRed	#af0087	rgb(175,0,135)	hsl(13,100%,34%) */
      case LogLevel.info.level  => Color.Reset
      case LogLevel.warn.level  => Color.Full(173) /* 173 LightSalmon3	#d7875f	rgb(215,135,95)	hsl(20,60%,60%) */
      case LogLevel.error.level => Color.Full(124) /* 124 Red3	#af0000	rgb(175,0,0)	hsl(0,100%,34%) */
    }

  @inline def subtleColor: EscapeAttr =
    Color.Full(102)

  def formatThrowable(th: Throwable): String = {
    val sw = new StringWriter()
    val pw = new PrintWriter(sw)
    th.printStackTrace(pw)
    sw.toString
  }

  object default extends Pattern {
    override def apply[T: Formatter](t: => Text[T], throwable: Option[Throwable], m: Metadata, ctx: Ctx): Str = {
      val Color = colorFor(m.logLevel)
      val Subtle = subtleColor

      Str.join(
        Color(prefixFor(m.logLevel)),
        " ",
        Subtle(m.instant.toString),
        " ",
        Subtle(Formatter(new File(m.file.value))),
        ":",
        Subtle(Formatter(m.line.value)),
        " ",
        Color(Formatter(t.value)),
        " ",
        Subtle(Formatter(ctx)),
        throwable match {
          case None     => ""
          case Some(th) => Subtle(formatThrowable(th))
        }
      )
    }
  }
}
