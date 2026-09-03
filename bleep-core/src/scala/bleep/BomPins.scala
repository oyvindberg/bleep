package bleep

import java.nio.file.Path
import scala.collection.immutable.SortedMap
import scala.xml.Elem

/** Turns a set of Maven BOMs into the version pins Maven's dependencyManagement semantics imply.
  *
  * Why not coursier's own BOM support with `forceOverrideVersions`: as of 2.1.25-M26 that path trips over BOM entries that carry exclusions ("Cannot find
  * commons-logging:commons-logging in reconciled versions"), and the milestone cannot be traded away — it carries the multi-release coursier-paths jar the
  * Windows support depends on. Non-forced BOMs resolve cleanly but only fill in missing versions; Maven pins transitives outright. So the BOMs are applied
  * twice, each through a path that demonstrably works: non-forced `addBoms` for defaults and managed exclusions, plus a version map extracted here and fed to
  * `ResolutionParams.forceVersion` — the same battle-tested mechanism bleep already uses for Scala artifact pinning.
  *
  * The extraction follows enough of Maven's model resolution for real BOMs: `<dependencyManagement>` entries with versions become pins, entries with
  * `scope=import` recurse into the imported BOM, `<parent>` poms contribute their properties and management sections, and `${...}` references interpolate from
  * the accumulated properties. Depth-bounded, and a reference that cannot be interpolated simply contributes no pin — the dependency then resolves as if
  * unmanaged, which is the pre-BOM behavior.
  *
  * Maven's "a direct declaration wins over dependencyManagement" rule is the caller's job: exempt modules the project declares itself before applying the pins
  * as forced versions (forcing would otherwise override even explicit choices, which Maven does not do).
  */
object BomPins {

  /** (groupId, artifactId) -> version */
  type Pins = SortedMap[(String, String), String]

  /** @param fetchPom
    *   resolves the pom FILE for exact (groupId, artifactId, version) coordinates — supplied by the resolver so the fetch uses its repositories, credentials
    *   and cache. Returning None (pom genuinely unavailable) prunes that branch of the recursion; the resolution error the missing pom would cause surfaces
    *   later, from coursier, with its usual reporting.
    */
  def apply(boms: Iterable[model.Dep], fetchPom: (String, String, String) => Option[Path]): Pins = {
    var pins: Pins = SortedMap.empty
    // first-wins across sibling BOMs, matching Maven's rule that the first imported BOM to manage a module keeps it
    def addPin(g: String, a: String, v: String): Unit =
      if (!pins.contains((g, a))) pins = pins.updated((g, a), v)

    def process(g: String, a: String, v: String, depth: Int): Unit =
      if (depth <= 8) {
        fetchPom(g, a, v).foreach { pomFile =>
          val pom = scala.xml.XML.loadFile(pomFile.toFile)
          val props = effectiveProperties(pom, g, a, v, depth, fetchPom)

          def interpolate(s: String): Option[String] = {
            val out = PropRef.replaceAllIn(s, m => java.util.regex.Matcher.quoteReplacement(props.getOrElse(m.group(1), m.matched)))
            if (out.contains("${")) None else Some(out)
          }

          (pom \ "dependencyManagement" \ "dependencies" \ "dependency").foreach { d =>
            val entry = for {
              eg <- interpolate((d \ "groupId").text.trim)
              ea <- interpolate((d \ "artifactId").text.trim)
              ev <- interpolate((d \ "version").text.trim).filter(_.nonEmpty)
            } yield (eg, ea, ev)
            entry.foreach { case (eg, ea, ev) =>
              if ((d \ "scope").text.trim == "import") process(eg, ea, ev, depth + 1)
              else addPin(eg, ea, ev)
            }
          }

          // the parent pom's management applies too, after this pom's own entries (child wins)
          (pom \ "parent").headOption.foreach { parent =>
            val entry = for {
              pg <- interpolate((parent \ "groupId").text.trim)
              pa <- interpolate((parent \ "artifactId").text.trim)
              pv <- interpolate((parent \ "version").text.trim).filter(_.nonEmpty)
            } yield (pg, pa, pv)
            entry.foreach { case (pg, pa, pv) => process(pg, pa, pv, depth + 1) }
          }
        }
      }

    boms.foreach(bom => process(bom.organization.value, bom.baseModuleName.value, bom.version, depth = 0))
    pins
  }

  private val PropRef = "\\$\\{([^}]+)}".r

  /** The pom's own `<properties>` plus those of its parent chain (child wins), plus the builtin project coordinates. */
  private def effectiveProperties(
      pom: Elem,
      g: String,
      a: String,
      v: String,
      depth: Int,
      fetchPom: (String, String, String) => Option[Path]
  ): Map[String, String] = {
    def propsOf(p: Elem): Map[String, String] =
      (p \ "properties").flatMap(_.child).collect { case e: Elem => e.label -> e.text.trim }.toMap

    def parentChainProps(p: Elem, remaining: Int): Map[String, String] =
      if (remaining <= 0) Map.empty
      else
        (p \ "parent").headOption match {
          case None         => Map.empty
          case Some(parent) =>
            val pg = (parent \ "groupId").text.trim
            val pa = (parent \ "artifactId").text.trim
            val pv = (parent \ "version").text.trim
            if (pg.isEmpty || pa.isEmpty || pv.isEmpty || pv.contains("${")) Map.empty
            else
              fetchPom(pg, pa, pv) match {
                case None          => Map.empty
                case Some(pomFile) =>
                  val parentPom = scala.xml.XML.loadFile(pomFile.toFile)
                  parentChainProps(parentPom, remaining - 1) ++ propsOf(parentPom)
              }
        }

    parentChainProps(pom, remaining = 8 - depth) ++ propsOf(pom) ++ Map(
      "project.version" -> v,
      "project.groupId" -> g,
      "project.artifactId" -> a
    )
  }
}
