package bleep
package templates

/** Takes an exploded build, infers templates and applies them. also groups cross projects
  */
object templatesInfer {
  def apply(logger: TemplateLogger, build: model.Build.Exploded, ignoreWhenInferringTemplates: model.ProjectName => Boolean): model.BuildFile = {

    val s1 = Step.step[model.CrossProjectName](
      logger,
      initialProjects = build.explodedProjects.map { case (crossName, p) => (crossName, model.ProjectWithExploded(p, p)) },
      ignoreWhenInferringTemplates = cn => ignoreWhenInferringTemplates(cn.name),
      templatingContent = Step.RelevantProjectContent.RelevantParents,
      applicableTemplates = TemplateDefs.project
    )

    val groupedCrossProjects: Map[model.ProjectName, model.ProjectWithExploded] =
      groupCrossProjects(s1.templatedProjects)

    val s2 = Step.step[model.ProjectName](
      logger,
      initialProjects = groupedCrossProjects,
      ignoreWhenInferringTemplates,
      templatingContent = Step.RelevantProjectContent.Noop,
      applicableTemplates = TemplateDefs.crossProject
    )

    val finishedTemplates = {
      def clean(xs: Map[model.TemplateId, Option[model.TemplateWithExploded]]) =
        xs.collect { case (templateId, Some(p)) if !p.current.isEmpty => (templateId, p.current) }

      clean(s1.inferredTemplates) ++ clean(s2.inferredTemplates)
    }

    val finishedProjects = s2.templatedProjects.map { case (n, cp) => (n, keepOnly(finishedTemplates.keySet, cp.current)) }

    val build1 =
      model.BuildFile(
        $schema = model.$schema,
        $version = build.$version,
        templates = model.JsonMap(finishedTemplates),
        scripts = model.JsonMap(build.scripts),
        resolvers = build.resolvers,
        projects = model.JsonMap(finishedProjects),
        jvm = build.jvm
      )

    val build2 = inlineTrivialTemplates(build1)
    val build3 = garbageCollectTemplates(build2)
    model.BuildFile.verifyTemplates(build3)
    build3
  }

  def keepOnly(existingTemplates: Set[model.TemplateId], p: model.Project): model.Project = {
    def go(p: model.Project): model.Project =
      p.copy(
        `extends` = p.`extends`.filter(existingTemplates.contains),
        cross = p.cross.map { case (crossId, p) => (crossId, go(p)) }
      )

    go(p)
  }

}
