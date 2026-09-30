package bleep
package templates

/** Takes an exploded build, infers templates and applies them. also groups cross projects
  *
  * Mining ([[mineTemplates]]) runs over every cross project on its own, which finds what for instance all Scala 2.13 projects share, whether they are cross
  * built or not. [[structureTemplates]] then gives the templates a structure a reader can follow, groups the cross projects of a project into one, and turns
  * how projects are cross built into cross setup templates. [[nameTemplates]] names them.
  */
object templatesInfer {
  def apply(
      logger: TemplateLogger,
      build: model.Build.Exploded,
      ignoreWhenInferringTemplates: model.ProjectName => Boolean,
      costs: mineTemplates.Costs
  ): model.BuildFile = {
    // settings a project has no use for are left out, so they neither end up in templates nor keep projects from sharing one
    val input = build.explodedProjects.map { case (name, p) => (name, withoutDeadSettings(p)) }
    val groups = new Groups(input)
    // defaults nearly a whole group shares count as shared, the projects which differ get their own value back at the end
    val rows = overridableDefaults.assume(input, groups)

    val mined = mineTemplates[model.CrossProjectName](
      logger,
      rows,
      Map.empty,
      groups.all.map(_.members),
      users => groups.exactly(users).orElse(groups.nearly(users)).isDefined,
      cn => ignoreWhenInferringTemplates(cn.name),
      idPrefix = "mined-",
      costs
    )

    val structured = structureTemplates(rows, mined, groups, costs)

    val build1 =
      model.BuildFile(
        $schema = model.$schema,
        $version = build.$version,
        templates = model.JsonMap(structured.templates),
        scripts = model.JsonMap(build.scripts),
        resolvers = build.resolvers,
        projects = model.JsonMap(structured.projects),
        jvm = build.jvm,
        `remote-cache` = build.remoteCache
      )

    val build2 = garbageCollectTemplates(build1)
    val build3 = nameTemplates(build2, groups, input)
    val build4 = overridableDefaults.restore(build3, input)
    model.BuildFile.verifyTemplates(build4)
    verifyLossless(build.copy(explodedProjects = input), build4)
    build4
  }

  /** A project compiled as java has no use for scala compiler settings: without `scala.version` it is not compiled with scala, and only `skipStdlib` is still
    * read (see `ResolveProjects`). bleep's own java projects got scala flags from a template every project extended.
    */
  def withoutDeadSettings(p: model.Project): model.Project =
    p.scala match {
      case Some(scala) if scala.version.isEmpty =>
        val stillRead = scala.skipStdlib.map(skip => model.Scala(None, model.Options.empty, None, model.JsonSet.empty, None, Some(skip), None, None))
        p.copy(scala = stillRead)
      case _ => p
    }

  def keepOnly(existingTemplates: Set[model.TemplateId], p: model.Project): model.Project = {
    def go(p: model.Project): model.Project =
      p.copy(
        `extends` = p.`extends`.filter(existingTemplates.contains),
        cross = p.cross.map { case (crossId, p) => (crossId, go(p)) }
      )

    go(p)
  }

  /** Templating only moves settings around, so every project must explode back to exactly what it was before. Anything else is a bug in inference, and a build
    * file that silently means something different from the build it was inferred from is worse than no build file.
    */
  def verifyLossless(before: model.Build.Exploded, after: model.BuildFile): Unit = {
    // exploding a build file adds defaults, so add them on the before side too
    val beforeProjects = before.explodedProjects.map { case (name, p) => (name, rewrites.Defaults.add.project(p)) }
    val afterProjects = model.Build.FileBacked(after).dropBuildFile.dropTemplates.explodedProjects

    model.Build.diffProjects(beforeProjects, afterProjects) match {
      case empty if empty.isEmpty => ()
      case diffs                  =>
        val rendered = diffs.map { case (projectName, msg) => s"${projectName.value}: $msg" }.mkString("\n")
        throw new BleepException.Text(s"Template inference changed ${diffs.size} projects. This is a bug, please report it.\n$rendered")
    }
  }
}
