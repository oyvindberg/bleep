package bleep

import bleep.testing.FancyBuildDisplay.Palette
import jatatui.core.layout.{Constraint, Direction, Layout}
import jatatui.core.style.{Modifier, Style}
import jatatui.core.text.{Line, Text}
import jatatui.crossterm.Jatatui
import jatatui.widgets.Borders
import jatatui.widgets.block.Block
import jatatui.widgets.list.{List as ListWidget, ListItem, ListState}

import java.util.Optional
import scala.jdk.CollectionConverters._

/** Reusable single-select picker using jatatui. Returns the selected index, or None if user cancelled (Esc). */
object TuiPicker {

  def pick(title: String, items: List[String]): Option[Int] =
    if (!testing.FancyBuildDisplay.isSupported) pickFallback(title, items)
    else pickTui(title, items)

  private def pickTui(title: String, items: List[String]): Option[Int] = {
    val terminal = Jatatui.init()
    try {
      val jni = terminal.backend().writer()
      val listState = ListState.empty()
      listState.select(Optional.of(Integer.valueOf(0)))
      var done = false
      var result: Option[Int] = None

      val baseStyle = Style.empty().withFg(Palette.text).withBg(Palette.bg)

      while (!done) {
        terminal.draw { f =>
          val chunks = Layout
            .of(Direction.Vertical, java.util.List.of[Constraint](new Constraint.Length(3), new Constraint.Min(1)))
            .split(f.area())

          val titleWidget = jatatui.widgets.paragraph.Paragraph
            .of(Text.raw(title))
            .withBlock(Block.empty().withBorders(Borders.ALL).withTitle(Line.raw("Setup")))
            .withStyle(baseStyle)
          f.renderWidget(titleWidget, chunks(0))

          val listItems = items.map(item => ListItem.of(Text.raw(s"  $item")).withStyle(baseStyle))

          val list = ListWidget
            .of(listItems.asJava)
            .withBlock(Block.empty().withBorders(Borders.ALL))
            .withStyle(baseStyle)
            .withHighlightStyle(Style.empty().withFg(Palette.bg).withBg(Palette.info).withAddModifier(Modifier.BOLD))
            .withHighlightSymbol("▸ ")
          f.renderStatefulWidget(list, chunks(1), listState)
        }: Unit

        if (jni.poll(new tui.crossterm.Duration(0, 100_000_000))) { // 100ms
          jni.read() match {
            case key: tui.crossterm.Event.Key =>
              key.keyEvent.code match {
                case _: tui.crossterm.KeyCode.Up =>
                  val cur = selectedIndex(listState)
                  listState.select(Optional.of(Integer.valueOf(math.max(0, cur - 1))))
                case _: tui.crossterm.KeyCode.Down =>
                  val cur = selectedIndex(listState)
                  listState.select(Optional.of(Integer.valueOf(math.min(items.length - 1, cur + 1))))
                case _: tui.crossterm.KeyCode.Enter =>
                  result = Some(selectedIndex(listState))
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
    } finally Jatatui.restore()
  }

  private def selectedIndex(listState: ListState): Int = {
    val selected = listState.selected()
    if (selected.isPresent) selected.get().intValue() else 0
  }

  /** Fallback for environments without TUI support (non-interactive terminals). */
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
