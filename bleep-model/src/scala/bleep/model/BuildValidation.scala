package bleep.model

/** Versions a project must state before anything tries to compile it.
  *
  * Elaboration expands templates and spells every project out in full; it never invents a version. So a project that reaches the compile path without one is a
  * broken build, and the compile path says so by throwing. The point of checking here is *when* and *how*: at build load, naming every offending project at
  * once, instead of an `IllegalStateException` from inside the BSP server partway through a build.
  *
  * Deliberately narrow. It asserts only what the compile path actually requires — the JS and Native link paths, which need both the platform version and the
  * language version to pick a toolchain. A `scala:` block without a version on a JVM project is left alone: nothing downstream demands one, and rejecting it
  * here would fail builds that work today. `schema.json` marks none of these required, so this is the only thing standing between a silently-defaulted version
  * and a confusing mid-build crash.
  */
object BuildValidation {

  /** One message per problem, empty when the build is fine. Pure, so it can be tested without a workspace. */
  def missingVersions(build: Build.Exploded): List[String] =
    build.explodedProjects.toList
      .sortBy { case (name, _) => name.value }
      .flatMap { case (crossName, project) =>
        val platformName = project.platform.flatMap(_.name)
        // A project counts as Kotlin exactly when it has a Kotlin version, which is the same test the compile path uses to route it. So there is nothing to
        // check for Kotlin here: without a version it is not compiled as Kotlin at all.
        val isKotlin = project.kotlin.flatMap(_.version).isDefined

        def needs(field: String, present: Boolean, hint: String): Option[String] =
          if (present) None
          else Some(s"${crossName.value}: $field is not set, but $hint. Set it on the project or on a template it extends.")

        (platformName, isKotlin) match {
          case (Some(PlatformId.Js), false) =>
            List(
              needs("platform.jsVersion", project.platform.flatMap(_.jsVersion).isDefined, "the project targets Scala.js"),
              needs("scala.version", project.scala.flatMap(_.version).isDefined, "the project targets Scala.js")
            ).flatten
          case (Some(PlatformId.Native), false) =>
            List(
              needs("platform.nativeVersion", project.platform.flatMap(_.nativeVersion).isDefined, "the project targets Scala Native"),
              needs("scala.version", project.scala.flatMap(_.version).isDefined, "the project targets Scala Native")
            ).flatten
          case _ => Nil
        }
      }
}
