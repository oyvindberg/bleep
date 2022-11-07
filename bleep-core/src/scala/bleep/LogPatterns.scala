package bleep

import bleep.logging._
import fansi.{Color, EscapeAttr, Str}
import sourcecode.Text

import java.io.File
import java.time.{Duration, Instant}

object LogPatterns {
  def prefixFor(l: LogLevel): String =
    f"[${l.name.value}%-5s]"

  def emojiFor(l: LogLevel) =
    l match {
      case LogLevel.debug => "\uD83D\uDCD8"
      case LogLevel.info  => "\uD83D\uDCD7"
      case LogLevel.warn  => "\uD83D\uDCD9"
      case LogLevel.error => "\uD83D\uDCD5"
    }

  def colorFor(l: LogLevel, noColor: Boolean): Option[EscapeAttr] =
    if (noColor) None
    else
      (l.level: @unchecked) match {
        // https://www.ditig.com/256-colors-cheat-sheet
        // chosen to be somewhat subtle (though I guess that's up for discussion), and to work both on white/black background
        case LogLevel.debug.level => Some(Color.Full(102))
//      case LogLevel.trace.level => Color.Full(223) /* 223 NavajoWhite1	#ffd7af	rgb(255,215,175)	hsl(30,100%,84%) */
//      case LogLevel.debug.level => Color.Full(139 /* 139 Grey63	#af87af	rgb(175,135,175)	hsl(300,20%,60%) */ )
        //      case LogLevel.info.level  => Color.Full(126) /* 126 MediumVioletRed	#af0087	rgb(175,0,135)	hsl(13,100%,34%) */
        case LogLevel.info.level  => None
        case LogLevel.warn.level  => Some(Color.Full(173)) /* 173 LightSalmon3	#d7875f	rgb(215,135,95)	hsl(20,60%,60%) */
        case LogLevel.error.level => Some(Color.Full(124)) /* 124 Red3	#af0000	rgb(175,0,0)	hsl(0,100%,34%) */
      }

  def subtleColor(noColor: Boolean): Option[EscapeAttr] =
    if (noColor) None else Some(Color.Full(102))

  private implicit class MaybeColorOps(private val maybeColor: Option[EscapeAttr]) extends AnyVal {
    def apply(str: Str): Str = maybeColor match {
      case Some(color) => color(str)
      case None        => str
    }
  }
  case class interface(t0: Instant, noColor: Boolean) extends Pattern {
    override def apply[T: Formatter](t: => Text[T], throwable: Option[Throwable], m: Metadata, ctx: Ctx, path: List[String]): Str = {
      val maybeColor = colorFor(m.logLevel, noColor)
      val maybeSubtleColor = subtleColor(noColor)
      val millis = Duration.between(t0, m.instant).toMillis

      Str.join(
        List(
          if (noColor) m.logLevel.bracketName else emojiFor(m.logLevel),
          " ",
          path match {
            case Nil      => ""
            case nonEmpty => maybeSubtleColor(nonEmpty.reverse.mkString("", " / ", ": "))
          },
          maybeColor(Formatter(t.value)),
          " ",
          maybeSubtleColor(Formatter(ctx.updated("t", Str(s"$millis ms")))),
          throwable match {
            case None     => ""
            case Some(th) => maybeColor(formatThrowable(th))
          }
        )
      )
    }
  }

  object logFile extends Pattern {
    override def apply[T: Formatter](t: => Text[T], throwable: Option[Throwable], m: Metadata, ctx: Ctx, path: List[String]): Str =
      List(
        prefixFor(m.logLevel),
        " ",
        m.instant.toString,
        " ",
        Formatter(new File(m.file.value)).plainText,
        ":",
        Formatter(m.line.value).plainText,
        " ",
        path match {
          case Nil      => ""
          case nonEmpty => nonEmpty.reverse.mkString("", " / ", ": ")
        },
        Formatter(t.value).plainText,
        " ",
        Formatter(ctx).plainText,
        throwable match {
          case None     => ""
          case Some(th) => formatThrowable(th)
        }
      ).mkString("")
  }
}
