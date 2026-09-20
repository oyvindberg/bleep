package bleep
package rewrites

object Defaults {
  // values copied from bloops `Config.CompileSetup.empty`
  val DefaultCompileSetup = model.CompileSetup(
    order = Some(model.CompileOrder.JavaThenScala),
    addLibraryToBootClasspath = Some(true),
    addCompilerToClasspath = Some(false),
    addExtraJarsToClasspath = Some(false),
    manageBootClasspath = Some(true),
    filterLibraryFromClasspath = Some(true)
  )

  val Jvm: model.Platform =
    model.Platform(
      name = None,
      mainClass = None,
      jsVersion = None,
      jsKind = None,
      jsSplitStyle = None,
      jsEmitSourceMaps = None,
      jsJsdom = None,
      jsNodeVersion = None,
      jvmOptions = model.Options(Set(model.Options.Opt.Flag(s"-Duser.dir=${model.Replacements.known.BuildDir}"))),
      jvmRuntimeOptions = model.Options.empty,
      jvmEnvironment = model.EnvironmentVars.empty,
      jvmAgents = model.JsonSet.empty,
      nativeVersion = None,
      nativeGc = None,
      nativeBuildTarget = None,
      nativeLinkerReleaseMode = None,
      nativeLTO = None,
      nativeMultithreading = None,
      nativeOptimize = None,
      nativeEmbedResources = None,
      nativeUseIncrementalCompilation = None
    )

  object remove extends BuildRewrite {
    override val name = model.BuildRewriteName("defaults-remove")

    protected def newExplodedProjects(oldBuild: model.Build, buildPaths: BuildPaths): Map[model.CrossProjectName, model.Project] =
      oldBuild.explodedProjects.map { case (name, p) => (name, project(p)) }

    def project(proj: model.Project): model.Project =
      proj.copy(
        scala = proj.scala.map(ret => ret.copy(setup = ret.setup.map(setup => setup.removeAll(Defaults.DefaultCompileSetup)))),
        platform = proj.platform.map(x => x.removeAll(Defaults.Jvm)),
        `source-layout` = proj.`source-layout`.filterNot { sourceLayout =>
          sourceLayout == defaultSourceLayout(proj)
        }
      )
  }

  object add extends BuildRewrite {
    override val name = model.BuildRewriteName("defaults-add")

    protected def newExplodedProjects(oldBuild: model.Build, buildPaths: BuildPaths): Map[model.CrossProjectName, model.Project] =
      oldBuild.explodedProjects.map { case (name, p) => (name, project(p)) }

    def project(proj: model.Project): model.Project =
      proj.copy(
        scala = proj.scala.map(x => x.copy(setup = Some(x.setup.fold(DefaultCompileSetup)(_.union(DefaultCompileSetup))))),
        platform = proj.platform.map { x =>
          if (x.name.contains(model.PlatformId.Jvm)) {
            // The default `-Duser.dir=${BUILD_DIR}` preserves sbt's working-directory semantics
            // (tests run from the build root there). It is a DEFAULT: a build that states its own
            // `-Duser.dir` — the maven importer emits `${PROJECT_DIR}`, matching surefire's
            // `${basedir}` — must not end up with two competing flags, since Options.union keeps
            // both and the JVM then obeys whichever happens to render last.
            val declaresUserDir = x.jvmOptions.values.exists {
              case model.Options.Opt.Flag(name) => name.startsWith("-Duser.dir=")
              case _                            => false
            }
            val defaults = if (declaresUserDir) Defaults.Jvm.copy(jvmOptions = model.Options.empty) else Defaults.Jvm
            x.union(defaults)
          } else x
        },
        `source-layout` = proj.`source-layout`.orElse {
          Some(defaultSourceLayout(proj))
        }
      )
  }

  /** Determine the default source layout based on project language configuration. Priority: Kotlin > Scala > Java
    */
  private def defaultSourceLayout(proj: model.Project): model.SourceLayout =
    if (proj.kotlin.flatMap(_.version).isDefined) model.SourceLayout.Kotlin
    else if (proj.scala.isDefined) model.SourceLayout.Normal
    else model.SourceLayout.Java
}
