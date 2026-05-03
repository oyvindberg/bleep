package bleep.javaapi

import bleep.commands.PublishLocal
import bleep.packaging.ManifestCreator
import bleep.{model, Commands, Started}
import cats.data.NonEmptyList

import java.util.Optional
import scala.jdk.CollectionConverters.*

final class JCommands(started: Started) extends bleepscript.Commands {
  private val underlying = new Commands(started)

  override def compile(projects: java.util.List[bleepscript.CrossProjectName]): Unit =
    underlying.compile(projects.asScala.iterator.map(JModel.toCross).toList, watch = false)

  override def compile(projects: java.util.List[bleepscript.CrossProjectName], watch: Boolean): Unit =
    underlying.compile(projects.asScala.iterator.map(JModel.toCross).toList, watch = watch)

  override def test(projects: java.util.List[bleepscript.CrossProjectName]): Unit =
    underlying.test(projects.asScala.iterator.map(JModel.toCross).toList, watch = false, only = None, exclude = None)

  override def test(
      projects: java.util.List[bleepscript.CrossProjectName],
      watch: Boolean,
      only: Optional[java.util.List[String]],
      exclude: Optional[java.util.List[String]]
  ): Unit =
    underlying.test(
      projects.asScala.iterator.map(JModel.toCross).toList,
      watch = watch,
      only = toNel(only),
      exclude = toNel(exclude)
    )

  override def run(project: bleepscript.CrossProjectName): Unit =
    underlying.run(JModel.toCross(project), None, Nil, raw = false, watch = false)

  override def run(
      project: bleepscript.CrossProjectName,
      overrideMainClass: Optional[String],
      args: java.util.List[String],
      raw: Boolean,
      watch: Boolean
  ): Unit =
    underlying.run(
      JModel.toCross(project),
      toScalaOpt(overrideMainClass),
      args.asScala.toList,
      raw = raw,
      watch = watch
    )

  override def clean(projects: java.util.List[bleepscript.CrossProjectName]): Unit =
    underlying.clean(projects.asScala.iterator.map(JModel.toCross).toList)

  override def script(scriptName: String, args: java.util.List[String]): Unit =
    underlying.script(model.ScriptName(scriptName), args.asScala.toList, watch = false)

  override def script(scriptName: String, args: java.util.List[String], watch: Boolean): Unit =
    underlying.script(model.ScriptName(scriptName), args.asScala.toList, watch = watch)

  override def publishLocal(options: bleepscript.PublishOptions): Unit =
    publishLocal(options, watch = false)

  override def publishLocal(options: bleepscript.PublishOptions, watch: Boolean): Unit = {
    val target: PublishLocal.PublishTarget = options.target match {
      case _: bleepscript.PublishTarget.LocalIvy     => PublishLocal.LocalIvy
      case mf: bleepscript.PublishTarget.MavenFolder =>
        PublishLocal.CustomMaven(model.Repository.MavenFolder(None, mf.path))
    }
    val manifestCreator: ManifestCreator = toScalaOpt(options.manifestCreator) match {
      case Some(mc: JManifestCreator) => mc.underlying
      case Some(other)                => throw new RuntimeException(s"Unknown ManifestCreator impl: ${other.getClass}")
      case None                       => ManifestCreator.default
    }
    val opts = PublishLocal.Options(
      groupId = options.groupId,
      version = options.version,
      publishTarget = target,
      projects = options.projects.asScala.iterator.map(JModel.toCross).toArray,
      manifestCreator = manifestCreator
    )
    underlying.publishLocal(opts, watch = watch)
  }

  private def toScalaOpt[T](o: Optional[T]): Option[T] =
    if (o.isPresent) Some(o.get) else None

  private def toNel(o: Optional[java.util.List[String]]): Option[NonEmptyList[String]] =
    toScalaOpt(o).flatMap { list =>
      val scalaList = list.asScala.toList
      if (scalaList.isEmpty) None else Some(NonEmptyList.fromListUnsafe(scalaList))
    }
}
