package bleep

import io.circe.parser.decode
import io.circe.syntax.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.Path
import scala.collection.immutable.SortedSet

class PathsByUsageTest extends AnyFunSuite with Matchers {

  private val src = Path.of("/ws/a/src")
  private val ap = Path.of("/ws/.bleep/projects/a/generated-sources/annotations")
  private val stamps = Path.of("/ws/.bleep/projects/a/generated-resources/stamps")

  private val tiered = PathsByUsage(
    List(
      PathsByUsage.Entry(src, Usage.Input),
      PathsByUsage.Entry(ap, Usage.Compile),
      PathsByUsage.Entry(stamps, Usage.Runtime)
    )
  )

  test("each usage admits its own tier and every narrower one") {
    tiered(Usage.Input) shouldBe List(src)
    tiered(Usage.Compile) shouldBe List(src, ap)
    tiered(Usage.Runtime) shouldBe List(src, ap, stamps)
  }

  test("entries keep construction order, and ++ is concatenation") {
    // The nested-build rewrite prepends bleep's own development classes so that they shadow. Anything that sorted or deduplicated here would quietly move them.
    val dev = Path.of("/dev/bleep-core/classes")
    val resolved = PathsByUsage.of(Usage.Compile, List(Path.of("/z.jar"), dev, Path.of("/a.jar")))

    val combined = PathsByUsage.of(Usage.Compile, List(dev)) ++ resolved

    combined(Usage.Compile) shouldBe List(dev, Path.of("/z.jar"), dev, Path.of("/a.jar"))
  }

  test("sortedDistinct reproduces exactly what a SortedSet would have") {
    // The main classpath and resource lists were built through `SortedSet`, so their order was by path, never dependency order. That has to survive.
    val paths = List(Path.of("/c"), Path.of("/a"), Path.of("/b"), Path.of("/a"))

    PathsByUsage.sortedDistinct(paths.map(PathsByUsage.Entry(_, Usage.Compile)))(Usage.Compile) shouldBe SortedSet.from(paths).toList
  }

  test("sortedDistinct keeps the narrowest usage when a path is tagged twice") {
    // If anything says the compiler sees a path, the compiler sees it.
    val twice = PathsByUsage.sortedDistinct(List(PathsByUsage.Entry(src, Usage.Runtime), PathsByUsage.Entry(src, Usage.Input)))

    twice.entries shouldBe List(PathsByUsage.Entry(src, Usage.Input))
  }

  test("round-trips through JSON, with the spelling that crosses the wire") {
    // `ResolvedProject` carries this to the BSP server, so the encoding is part of the client/server protocol.
    val json = tiered.asJson

    json.noSpaces should include(""""usage":"input"""")
    json.noSpaces should include(""""usage":"compile"""")
    json.noSpaces should include(""""usage":"runtime"""")
    decode[PathsByUsage](json.noSpaces) shouldBe Right(tiered)
  }

  test("an unknown usage fails to decode rather than being guessed") {
    decode[PathsByUsage]("""{"entries":[{"path":"/x","usage":"sometimes"}]}""").isLeft shouldBe true
  }

  test("DirsByOrigin.all is exactly byUsage, so the two can never disagree") {
    val dirs = ProjectPaths.DirsByOrigin(
      fromSourceLayout = SortedSet(src),
      fromJson = Map.empty,
      generated = Map.empty,
      annotationProcessing = Some(ap),
      ksp = Nil,
      stamps = Some(stamps)
    )

    List(Usage.Input, Usage.Compile, Usage.Runtime).foreach { usage =>
      withClue(s"$usage: ")(dirs.all(usage) shouldBe SortedSet.from(dirs.byUsage(usage)))
    }
    dirs.all(Usage.Input) shouldBe SortedSet(src)
    dirs.all(Usage.Compile) shouldBe SortedSet(src, ap)
    dirs.all(Usage.Runtime) shouldBe SortedSet(src, ap, stamps)
  }
}
