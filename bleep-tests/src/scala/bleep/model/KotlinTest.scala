package bleep
package model

import io.circe.syntax.*
import org.scalactic.TripleEqualsSupport
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.SortedMap

class KotlinTest extends AnyFunSuite with TripleEqualsSupport {
  private def kotlin(
      version: Option[VersionKotlin] = None,
      options: Options = Options.empty,
      jvmTarget: Option[String] = None,
      compilerPlugins: JsonSet[String] = JsonSet.empty,
      kspVersion: Option[String] = None,
      scanForSymbolProcessors: Option[Boolean] = None,
      symbolProcessors: JsonSet[Dep] = JsonSet.empty,
      symbolProcessorOptions: SymbolProcessorOptions = SymbolProcessorOptions.empty
  ): Kotlin =
    Kotlin(
      version = version,
      options = options,
      jvmTarget = jvmTarget,
      compilerPlugins = compilerPlugins,
      kspVersion = kspVersion,
      scanForSymbolProcessors = scanForSymbolProcessors,
      symbolProcessors = symbolProcessors,
      symbolProcessorOptions = symbolProcessorOptions,
      js = None,
      native = None
    )

  def roundtrip(k: Kotlin): Assertion = {
    val json = k.asJson
    val k2 = json.as[Kotlin].orThrowWithError("Failed to parse json")
    assert(k === k2)
  }

  private val room = Dep.Java("androidx.room", "room-compiler", "2.6.1")
  private val hilt = Dep.Java("com.google.dagger", "hilt-compiler", "2.51")

  test("Kotlin model KSP round-trip") {
    roundtrip(kotlin())
    roundtrip(kotlin(kspVersion = Some("1.0.29")))
    roundtrip(kotlin(scanForSymbolProcessors = Some(true)))
    roundtrip(kotlin(scanForSymbolProcessors = Some(false)))
    roundtrip(kotlin(symbolProcessors = JsonSet(room)))
    roundtrip(kotlin(symbolProcessors = JsonSet(room, hilt)))
    roundtrip(
      kotlin(
        version = Some(VersionKotlin.Kotlin23),
        kspVersion = Some("1.0.29"),
        scanForSymbolProcessors = Some(true),
        symbolProcessors = JsonSet(room),
        symbolProcessorOptions = SymbolProcessorOptions(SortedMap("room.schemaLocation" -> "./schemas", "room.incremental" -> "true"))
      )
    )
  }

  test("Kotlin model setlike — symbolProcessors union/intersect/removeAll") {
    val k1 = kotlin(symbolProcessors = JsonSet(room, hilt))
    val k2 = kotlin(symbolProcessors = JsonSet(hilt))

    assert(k1.union(k2).symbolProcessors.values === Set(room, hilt))
    assert(k1.intersect(k2).symbolProcessors.values === Set(hilt))
    assert(k1.removeAll(k2).symbolProcessors.values === Set(room))
    assert(k1.removeAll(k1).symbolProcessors.values === Set.empty[Dep])
  }

  test("Kotlin model setlike — symbolProcessorOptions union prefers child") {
    val parent = kotlin(symbolProcessorOptions = SymbolProcessorOptions(SortedMap("k1" -> "parent", "k2" -> "shared")))
    val child = kotlin(symbolProcessorOptions = SymbolProcessorOptions(SortedMap("k1" -> "child", "k3" -> "child-only")))

    val merged = child.union(parent)
    assert(merged.symbolProcessorOptions.value === SortedMap("k1" -> "child", "k2" -> "shared", "k3" -> "child-only"))
  }

  test("Kotlin model setlike — kspVersion / scanForSymbolProcessors union/intersect/removeAll") {
    val pinned = kotlin(kspVersion = Some("1.0.29"))
    val other = kotlin(kspVersion = Some("1.0.30"))
    val unset = kotlin(kspVersion = None)

    assert(pinned.union(other).kspVersion === Some("1.0.29")) // self wins via orElse
    assert(unset.union(pinned).kspVersion === Some("1.0.29"))
    assert(pinned.intersect(pinned).kspVersion === Some("1.0.29"))
    assert(pinned.intersect(other).kspVersion === None)
    assert(pinned.removeAll(pinned).kspVersion === None)
    assert(pinned.removeAll(other).kspVersion === Some("1.0.29"))

    val scanOn = kotlin(scanForSymbolProcessors = Some(true))
    val scanOff = kotlin(scanForSymbolProcessors = Some(false))
    assert(scanOn.union(scanOff).scanForSymbolProcessors === Some(true))
    assert(scanOn.intersect(scanOff).scanForSymbolProcessors === None)
  }

  test("Kotlin model isEmpty") {
    assert(kotlin().isEmpty)
    assert(!kotlin(kspVersion = Some("1.0.29")).isEmpty)
    assert(!kotlin(scanForSymbolProcessors = Some(true)).isEmpty)
    assert(!kotlin(symbolProcessors = JsonSet(room)).isEmpty)
    assert(!kotlin(symbolProcessorOptions = SymbolProcessorOptions(SortedMap("k" -> "v"))).isEmpty)
  }

  test("Kotlin.hasSymbolProcessing") {
    assert(!kotlin().hasSymbolProcessing)
    assert(!kotlin(kspVersion = Some("1.0.29")).hasSymbolProcessing) // kspVersion alone doesn't trigger
    assert(kotlin(scanForSymbolProcessors = Some(true)).hasSymbolProcessing)
    assert(!kotlin(scanForSymbolProcessors = Some(false)).hasSymbolProcessing)
    assert(kotlin(symbolProcessors = JsonSet(room)).hasSymbolProcessing)
  }
}
