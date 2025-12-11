package bleep
package internal

import fansi.Str
import org.scalatest.funsuite.AnyFunSuite

class BspClientDisplayProgressTest extends AnyFunSuite {

  test("renderProgress fits all items when there's enough space") {
    val items = List(
      Str("project1: 50%"),
      Str("project2: 75%"),
      Str("project3: 100%")
    )
    val result = BspClientDisplayProgress.renderProgress(items, 0, 80)
    assert(result == "Compiling project1: 50%, project2: 75%, project3: 100%")
  }

  test("renderProgress truncates items when terminal is narrow") {
    val items = List(
      Str("project1: 50%"),
      Str("project2: 75%"),
      Str("project3: 100%")
    )
    val result = BspClientDisplayProgress.renderProgress(items, 0, 40)
    // "Compiling " = 10 chars, "" = 0 chars suffix, leaving 30 chars
    // "project1: 50%" = 13 chars (fits)
    // ", project2: 75%" = 15 chars (28 total, fits)
    // ", project3: 100%" = 16 chars (44 total, doesn't fit)
    assert(result == "Compiling project1: 50%, project2: 75% +1")
  }

  test("renderProgress handles very narrow terminals") {
    val items = List(
      Str("project1: 50%"),
      Str("project2: 75%")
    )
    val result = BspClientDisplayProgress.renderProgress(items, 0, 20)
    // Fixed width = 10 ("Compiling ") + 0 (""), available = 10, but min 20
    // So available = 20, but total terminal width is 20
    // "project1: 50%" = 13 chars (fits in 20 - 10 = 10? No, we ensure min 20 available)
    // Actually: terminalWidth=20, fixedWidth=10, availableWidth=max(20-10, 20)=20
    // So we have 20 chars available for content
    assert(result == "Compiling project1: 50% +1")
  }

  test("renderProgress shows remaining count in suffix") {
    val items = List(
      Str("project1: 50%"),
      Str("project2: 75%")
    )
    val result = BspClientDisplayProgress.renderProgress(items, 3, 80)
    assert(result == "Compiling project1: 50%, project2: 75% +3")
  }

  test("renderProgress handles empty item list") {
    val items = List.empty[Str]
    val result = BspClientDisplayProgress.renderProgress(items, 0, 80)
    assert(result == "Compiling ")
  }

  test("renderProgress handles empty item list with remaining items") {
    val items = List.empty[Str]
    val result = BspClientDisplayProgress.renderProgress(items, 5, 80)
    assert(result == "Compiling  +5")
  }

  test("renderProgress correctly calculates hidden count when truncating") {
    val items = List(
      Str("a: 1%"), // 5 chars
      Str("b: 2%"), // 5 chars
      Str("c: 3%"), // 5 chars
      Str("d: 4%") // 5 chars
    )
    val remainingCount = 2
    // Terminal width 30: "Compiling " (10) + " +2" (3) = 13 fixed chars, 17 available
    // But we ensure minimum 20 available, so availableWidth = max(30-13, 20) = 20
    // "a: 1%" = 5 (fits)
    // ", b: 2%" = 7 (12 total, fits)
    // ", c: 3%" = 7 (19 total, fits)
    // ", d: 4%" = 7 (26 total, doesn't fit)
    // Hidden: 1 item not shown + 2 remaining = 3 total
    val result = BspClientDisplayProgress.renderProgress(items, remainingCount, 30)
    assert(result == "Compiling a: 1%, b: 2%, c: 3% +3")
  }

  test("renderProgress handles items with ANSI codes correctly") {
    val items = List(
      fansi.Bold.On(Str("project1")) ++ Str(": 50%"),
      fansi.Color.Red(Str("project2")) ++ Str(": 75%")
    )
    val result = BspClientDisplayProgress.renderProgress(items, 0, 80)
    // The rendered string contains ANSI codes, so we check for the actual rendered output
    assert(result.contains(fansi.Bold.On(Str("project1")).render))
    assert(result.contains(fansi.Color.Red(Str("project2")).render))
    assert(result.contains(": 50%"))
    assert(result.contains(": 75%"))
  }

  test("renderProgress respects minimum available width of 20 chars") {
    val items = List(
      Str("very-long-project-name: 50%")
    )
    // Even with very narrow terminal, we ensure at least 20 chars for content
    val result = BspClientDisplayProgress.renderProgress(items, 0, 15)
    // fixedWidth = 10, availableWidth = max(15-10, 20) = 20
    // But the full string won't fit, so we get the hidden count
    assert(result == "Compiling  +1")
  }
}
