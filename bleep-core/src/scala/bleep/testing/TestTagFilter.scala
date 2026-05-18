package bleep.testing

/** Translates a `testTags` manifest (tag → set of FQDN patterns) into per-suite filtering. Operates at the suite-discovery boundary in
  * `MultiWorkspaceBspServer` — same place `--only` / `--exclude` regex filters apply — so the mechanism is independent of test framework. Works for ScalaTest,
  * JUnit, MUnit, utest, or anything else the runner discovers.
  *
  * Glob semantics for patterns:
  *   - `*` matches any sequence inside a single FQDN segment (no dots)
  *   - `**` matches anything including dots
  *   - everything else is literal
  *
  * So `bleep.foo.*` matches `bleep.foo.Bar` but not `bleep.foo.baz.Bar`. `**IT` matches any FQDN ending in `IT`. `bleep.**.HeavyTest` matches HeavyTest in any
  * sub-package of `bleep`.
  */
object TestTagFilter {

  /** Compile a glob pattern to a regex. Public for tests. Anchored at both ends. */
  def compileGlob(pattern: String): scala.util.matching.Regex = {
    val sb = new StringBuilder("^")
    var i = 0
    while (i < pattern.length) {
      val c = pattern.charAt(i)
      c match {
        case '*' if i + 1 < pattern.length && pattern.charAt(i + 1) == '*' =>
          sb.append(".*")
          i += 2
        case '*' =>
          sb.append("[^.]*")
          i += 1
        case '.' | '\\' | '+' | '(' | ')' | '[' | ']' | '{' | '}' | '^' | '$' | '|' | '?' =>
          sb.append('\\')
          sb.append(c)
          i += 1
        case _ =>
          sb.append(c)
          i += 1
      }
    }
    sb.append('$')
    sb.result().r
  }

  /** All tags that match the given suite FQDN, given the project's `testTags` manifest. A suite matches a tag if any of the tag's patterns matches the FQDN. */
  def tagsFor(suite: String, manifest: Map[String, Set[String]]): Set[String] = {
    val compiled = manifest.view.mapValues(_.map(compileGlob)).toMap
    compiled.collect { case (tag, patterns) if patterns.exists(_.matches(suite)) => tag }.toSet
  }

  /** Apply tag-based include/exclude filtering to a list of discovered suite FQDNs.
    *
    *   - `includeTags` empty → start from all suites
    *   - `includeTags` non-empty → start from suites matching any of the included tags (untagged suites are excluded when at least one include is specified)
    *   - `excludeTags` → remove any suite that matches any excluded tag
    *
    * Returns (kept, dropped) so callers can report what got filtered.
    */
  def filter(
      suites: List[String],
      manifest: Map[String, Set[String]],
      includeTags: Set[String],
      excludeTags: Set[String]
  ): (List[String], List[String]) = {
    val (kept, dropped) = suites.partition { suite =>
      val tags = tagsFor(suite, manifest)
      val includeOk = includeTags.isEmpty || tags.intersect(includeTags).nonEmpty
      val excludeOk = tags.intersect(excludeTags).isEmpty
      includeOk && excludeOk
    }
    (kept, dropped)
  }

  /** Pre-compile project pruning. Given a candidate set of projects (each carrying its own `testTags` map) and the user's `--only-tag` set, return the subset
    * of projects whose `testTags` declare at least one of the requested tags. Saves a lot of compile work in the common case `bleep test --only-tag slow` where
    * only one test project (the IT-housing one) actually has any slow tags.
    *
    * Empty `includeTags` means no narrowing — return all candidates.
    *
    * `--exclude-tag` deliberately does NOT prune projects: a project can have a mix of tagged and untagged tests, and the untagged ones still need to run.
    * Exclude-narrowing happens only at the suite level inside `filter` above.
    */
  def projectsRelevantForIncludes[ProjectName](
      candidates: Set[ProjectName],
      tagKeysByProject: ProjectName => Set[String],
      includeTags: Set[String]
  ): Set[ProjectName] =
    if (includeTags.isEmpty) candidates
    else candidates.filter(p => tagKeysByProject(p).intersect(includeTags).nonEmpty)

  /** Validate a manifest against the actually-discovered suite set. Returns a list of warning messages for patterns that match no discovered suite (drifted
    * entries). Empty list = clean.
    */
  def staleManifestEntries(manifest: Map[String, Set[String]], discoveredSuites: Set[String]): List[String] = {
    val warnings = List.newBuilder[String]
    manifest.foreach { case (tag, patterns) =>
      patterns.foreach { pattern =>
        val regex = compileGlob(pattern)
        if (!discoveredSuites.exists(regex.matches))
          warnings += s"testTags.$tag pattern '$pattern' matches no discovered suite"
      }
    }
    warnings.result()
  }
}
