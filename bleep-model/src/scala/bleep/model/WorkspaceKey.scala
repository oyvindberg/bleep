package bleep.model

import java.nio.file.Path

/** Identity of one resolved build: a workspace root and the variant built within it.
  *
  * This is what a compile server's per-build state is keyed by, and it exists as a named type because two different caches key on it and they have to agree.
  * `BuildCache` holds the resolved `Started`; `bleep.analysis.AnalysisCache` holds the Zinc analyses read while compiling it. Those analyses live on disk under
  * `.bleep/projects/<project>/builds/<variant>/.zinc/`, i.e. inside the workspace and partitioned by variant — exactly this key — so the association is not a
  * convention, it is the layout.
  *
  * Keeping the two keyed alike is what lets dropping a build also drop its analyses. The reverse does not hold: analyses are cheap to re-read and expensive to
  * hold, a resolved build is the other way round, so they are evicted on separate schedules. See `BuildCache` for that asymmetry.
  */
case class WorkspaceKey(workspace: Path, variant: BuildVariant) {

  /** Short rendering for logs and metrics: the workspace's own directory name plus the variant. */
  def short: String = {
    val name = Option(workspace.getFileName).map(_.toString).getOrElse(workspace.toString)
    s"$name/${variant.toString}"
  }
}
