package bleep
package model

import bleep.internal.compat.OptionCompatOps
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

case class Project(
    `extends`: JsonSet[TemplateId],
    cross: JsonMap[CrossId, Project],
    folder: Option[RelPath],
    dependsOn: JsonSet[ProjectName],
    `source-layout`: Option[SourceLayout],
    `sbt-scope`: Option[String],
    sources: JsonSet[RelPath],
    resources: JsonSet[RelPath],
    dependencies: JsonSet[Dep],
    java: Option[Java],
    scala: Option[Scala],
    platform: Option[Platform],
    isTestProject: Option[Boolean],
    testFrameworks: JsonSet[TestFrameworkName],
    sourcegen: JsonSet[ScriptDef],
    libraryVersionSchemes: JsonSet[LibraryVersionScheme]
) extends SetLike[Project] {
  override def intersect(other: Project): Project =
    Project(
      `extends` = `extends`.intersect(other.`extends`),
      cross = cross.intersect(other.cross),
      folder = if (folder == other.folder) folder else None,
      dependsOn = dependsOn.intersect(other.dependsOn),
      `source-layout` = if (`source-layout` == other.`source-layout`) `source-layout` else None,
      `sbt-scope` = if (`sbt-scope` == other.`sbt-scope`) `sbt-scope` else None,
      sources = sources.intersect(other.sources),
      resources = resources.intersect(other.resources),
      dependencies = dependencies.intersect(other.dependencies),
      java = java.zipCompat(other.java).map { case (_1, _2) => _1.intersect(_2) },
      scala = scala.zipCompat(other.scala).map { case (_1, _2) => _1.intersect(_2) },
      platform = platform.zipCompat(other.platform).flatMap { case (_1, _2) => _1.intersectDropEmpty(_2) },
      isTestProject = if (isTestProject == other.isTestProject) isTestProject else None,
      testFrameworks = testFrameworks.intersect(other.testFrameworks),
      sourcegen = sourcegen.intersect(other.sourcegen),
      libraryVersionSchemes = libraryVersionSchemes.intersect(other.libraryVersionSchemes)
    )

  override def removeAll(other: Project): Project =
    Project(
      `extends` = `extends`.removeAll(other.`extends`),
      cross = cross.removeAll(other.cross),
      folder = if (folder == other.folder) None else folder,
      dependsOn = dependsOn.removeAll(other.dependsOn),
      `source-layout` = if (`source-layout` == other.`source-layout`) None else `source-layout`,
      `sbt-scope` = if (`sbt-scope` == other.`sbt-scope`) None else `sbt-scope`,
      sources = sources.removeAll(other.sources),
      resources = resources.removeAll(other.resources),
      dependencies = dependencies.removeAll(other.dependencies),
      java = removeAllFrom(java, other.java),
      scala = removeAllFrom(scala, other.scala),
      platform = (platform, other.platform) match {
        case (Some(one), Some(two)) => one.removeAllDropEmpty(two)
        case _                      => platform
      },
      isTestProject = if (isTestProject == other.isTestProject) None else isTestProject,
      testFrameworks = testFrameworks.removeAll(other.testFrameworks),
      sourcegen = sourcegen.removeAll(other.sourcegen),
      libraryVersionSchemes = libraryVersionSchemes.removeAll(other.libraryVersionSchemes)
    )

  override def union(other: Project): Project =
    Project(
      `extends` = `extends`.union(other.`extends`),
      cross = cross.union(other.cross),
      folder = folder.orElse(other.folder),
      dependsOn = dependsOn.union(other.dependsOn),
      `source-layout` = `source-layout`.orElse(other.`source-layout`),
      `sbt-scope` = `sbt-scope`.orElse(other.`sbt-scope`),
      sources = sources.union(other.sources),
      resources = resources.union(other.resources),
      dependencies = dependencies.union(other.dependencies),
      java = List(java, other.java).flatten.reduceOption(_ union _),
      scala = List(scala, other.scala).flatten.reduceOption(_ union _),
      // may throw
      platform = List(platform, other.platform).flatten.reduceOption(_ union _),
      isTestProject = isTestProject.orElse(other.isTestProject),
      testFrameworks = testFrameworks.union(other.testFrameworks),
      sourcegen = sourcegen.union(other.sourcegen),
      libraryVersionSchemes = libraryVersionSchemes.union(other.libraryVersionSchemes)
    )

  override def isEmpty: Boolean = this match {
    case Project(
          extends_,
          cross,
          folder,
          dependsOn,
          sourceLayout,
          sbtScope,
          sources,
          resources,
          dependencies,
          java,
          scala,
          platform,
          isTestProject,
          testFrameworks,
          sourceGeneratorsScripts,
          libraryVersionSchemes
        ) =>
      extends_.isEmpty &&
      cross.isEmpty &&
      folder.isEmpty &&
      dependsOn.isEmpty &&
      sourceLayout.isEmpty &&
      sbtScope.isEmpty &&
      sources.isEmpty &&
      resources.isEmpty &&
      dependencies.isEmpty &&
      java.fold(true)(_.isEmpty) &&
      scala.fold(true)(_.isEmpty) &&
      platform.fold(true)(_.isEmpty) &&
      isTestProject.isEmpty &&
      testFrameworks.isEmpty &&
      sourceGeneratorsScripts.isEmpty &&
      libraryVersionSchemes.isEmpty
  }
}

object Project {
  val empty = Project(
    `extends` = JsonSet.empty,
    cross = JsonMap.empty,
    folder = None,
    dependsOn = JsonSet.empty,
    `source-layout` = None,
    `sbt-scope` = None,
    sources = JsonSet.empty,
    resources = JsonSet.empty,
    dependencies = JsonSet.empty,
    java = None,
    scala = None,
    platform = None,
    isTestProject = None,
    testFrameworks = JsonSet.empty,
    sourcegen = JsonSet.empty,
    libraryVersionSchemes = JsonSet.empty
  )

  implicit def decodes(implicit templateIdDecoder: Decoder[TemplateId], projectNameDecoder: Decoder[ProjectName]): Decoder[Project] = {
    templateIdDecoder.discard()
    projectNameDecoder.discard()
    deriveDecoder
  }

  implicit val encodes: Encoder[Project] = deriveEncoder
}
