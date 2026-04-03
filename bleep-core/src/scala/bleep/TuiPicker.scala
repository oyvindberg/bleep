package bleep

import bleep.testing.FancyBuildDisplay.Palette
import tui._
import tui.crossterm.CrosstermJni
import tui.widgets.{BlockWidget, ListWidget, ParagraphWidget}

/** Reusable single-select picker using tui-scala. Returns the selected index, or None if user cancelled (Esc). */
object TuiPicker {

  def pick(title: String, items: List[String]): Option[Int] =
    if (!testing.FancyBuildDisplay.isSupported) pickFallback(title, items)
    else pickTui(title, items)

  private def pickTui(title: String, items: List[String]): Option[Int] =
    withTerminal { (jni, terminal) =>
      val listState = ListWidget.State(selected = Some(0))
      var done = false
      var result: Option[Int] = None

      while (!done) {
        terminal.draw { f =>
          val chunks = Layout(
            direction = Direction.Vertical,
            constraints = Array(Constraint.Length(3), Constraint.Min(1))
          ).split(f.size)

          val titleWidget = ParagraphWidget(
            text = Text.nostyle(title),
            block = Some(BlockWidget(borders = Borders.ALL, title = Some(Spans.nostyle("Setup")))),
            style = Style(fg = Some(Palette.text), bg = Some(Palette.bg))
          )
          f.renderWidget(titleWidget, chunks(0))

          val listItems = items.zipWithIndex.map { case (item, _) =>
            ListWidget.Item(Text.nostyle(s"  $item"), Style(fg = Some(Palette.text), bg = Some(Palette.bg)))
          }.toArray

          val list = ListWidget(
            items = listItems,
            block = Some(BlockWidget(borders = Borders.ALL)),
            style = Style(fg = Some(Palette.text), bg = Some(Palette.bg)),
            highlightStyle = Style(fg = Some(Palette.bg), bg = Some(Palette.info), addModifier = Modifier.BOLD),
            highlightSymbol = Some("▸ ")
          )
          f.renderStatefulWidget(list, chunks(1))(listState)
        }: Unit

        if (jni.poll(new tui.crossterm.Duration(0, 100_000_000))) { // 100ms
          jni.read() match {
            case key: tui.crossterm.Event.Key =>
              key.keyEvent.code match {
                case _: tui.crossterm.KeyCode.Up =>
                  val cur = listState.selected.getOrElse(0)
                  listState.select(Some(math.max(0, cur - 1)))
                case _: tui.crossterm.KeyCode.Down =>
                  val cur = listState.selected.getOrElse(0)
                  listState.select(Some(math.min(items.length - 1, cur + 1)))
                case _: tui.crossterm.KeyCode.Enter =>
                  result = listState.selected
                  done = true
                case _: tui.crossterm.KeyCode.Esc =>
                  done = true
                case char: tui.crossterm.KeyCode.Char if char.c() == 'q' || char.c() == 'Q' =>
                  done = true
                case _ => ()
              }
            case _ => ()
          }
        }
      }
      result
    }

  /** Fallback for environments without TUI support (Windows, non-interactive terminals). */
  private def pickFallback(title: String, items: List[String]): Option[Int] = {
    System.err.println(title)
    items.zipWithIndex.foreach { case (item, idx) =>
      System.err.println(s"  ${idx + 1}) $item")
    }
    System.err.print("Enter number (or 0 to cancel): ")
    val line = scala.io.StdIn.readLine()
    scala.util.Try(line.trim.toInt).toOption match {
      case Some(n) if n >= 1 && n <= items.length => Some(n - 1)
      case _                                      => None
    }
  }
}
