package bleep.javaapi

import bleep.commands.{Publish, PublishLocal, PublishSonatype}
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
    underlying.test(
      projects.asScala.iterator.map(JModel.toCross).toList,
      watch = false,
      only = None,
      exclude = None,
      includeTags = None,
      excludeTags = None
    )

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
      exclude = toNel(exclude),
      includeTags = None,
      excludeTags = None
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
      case _: bleepscript.PublishTarget.Resolver =>
        throw new IllegalArgumentException(
          "publishLocal does not support Resolver targets. Use publish(...) instead."
        )
      case _: bleepscript.PublishTarget.SonatypeCentral =>
        throw new IllegalArgumentException(
          "publishLocal does not support SonatypeCentral. Use publishSonatype(...) or publish(...) instead."
        )
    }
    val manifestCreator: ManifestCreator = toScalaOpt(options.manifestCreator) match {
      case Some(mc: JManifestCreator) => mc.underlying
      case Some(other)                => throw new RuntimeException(s"Unknown ManifestCreator impl: ${other.getClass}")
      case None                       => ManifestCreator.default
    }
    val version = resolveVersion(options)
    val opts = PublishLocal.Options(
      groupId = options.groupId,
      version = version,
      publishTarget = target,
      projects = options.projects.asScala.iterator.map(JModel.toCross).toArray,
      manifestCreator = manifestCreator
    )
    underlying.publishLocal(opts, watch = watch)
  }

  override def publish(options: bleepscript.PublishOptions): Unit =
    publish(options, watch = false)

  override def publish(options: bleepscript.PublishOptions, watch: Boolean): Unit = {
    val manifestCreator: ManifestCreator = toScalaOpt(options.manifestCreator) match {
      case Some(mc: JManifestCreator) => mc.underlying
      case Some(other)                => throw new RuntimeException(s"Unknown ManifestCreator impl: ${other.getClass}")
      case None                       => ManifestCreator.default
    }
    val versionOverride = toScalaOpt(options.versionOverride)
    val versionFallback = toScalaOpt(options.versionFallback).map(supplier => () => supplier.get())

    options.target match {
      case _: bleepscript.PublishTarget.LocalIvy =>
        val target = PublishLocal.LocalIvy
        val version = resolveVersion(options)
        val opts = PublishLocal.Options(
          groupId = options.groupId,
          version = version,
          publishTarget = target,
          projects = options.projects.asScala.iterator.map(JModel.toCross).toArray,
          manifestCreator = manifestCreator
        )
        underlying.publishLocal(opts, watch = watch)

      case mf: bleepscript.PublishTarget.MavenFolder =>
        val target = PublishLocal.CustomMaven(model.Repository.MavenFolder(None, mf.path))
        val version = resolveVersion(options)
        val opts = PublishLocal.Options(
          groupId = options.groupId,
          version = version,
          publishTarget = target,
          projects = options.projects.asScala.iterator.map(JModel.toCross).toArray,
          manifestCreator = manifestCreator
        )
        underlying.publishLocal(opts, watch = watch)

      case r: bleepscript.PublishTarget.Resolver =>
        val opts = Publish.Options(
          versionOverride = versionOverride,
          versionFallback = versionFallback,
          assertRelease = options.assertRelease,
          dryRun = options.dryRun,
          target = Publish.Target.Resolver(model.ResolverName(r.name)),
          projectNames = options.projects.asScala.iterator.map(JModel.toCross).toArray,
          manifestCreator = manifestCreator
        )
        underlying.publish(opts, watch = watch)

      case _: bleepscript.PublishTarget.SonatypeCentral =>
        // PublishSonatype reads sonatypeProfileName / sonatypeCredentialHost from each project's
        // publishConfig in bleep.yaml. The SonatypeCentral target's fields are exposed on the
        // Java side for documentation and future use, but the Scala command treats bleep.yaml as
        // the source of truth so a script can't accidentally override what the build declares.
        runPublishSonatype(options, manifestCreator, versionOverride)
    }
  }

  override def publishSonatype(options: bleepscript.PublishOptions): Unit = {
    val manifestCreator: ManifestCreator = toScalaOpt(options.manifestCreator) match {
      case Some(mc: JManifestCreator) => mc.underlying
      case Some(other)                => throw new RuntimeException(s"Unknown ManifestCreator impl: ${other.getClass}")
      case None                       => ManifestCreator.default
    }
    val versionOverride = toScalaOpt(options.versionOverride)
    runPublishSonatype(options, manifestCreator, versionOverride)
  }

  private def runPublishSonatype(
      options: bleepscript.PublishOptions,
      manifestCreator: ManifestCreator,
      versionOverride: Option[String]
  ): Unit =
    underlying.publishSonatype(
      PublishSonatype.Options(
        versionOverride = versionOverride,
        assertRelease = options.assertRelease,
        projectNames = options.projects.asScala.iterator.map(JModel.toCross).toArray,
        manifestCreator = manifestCreator
      )
    )

  /** Resolve the version eagerly for the local-publish path, which expects a non-optional version. */
  private def resolveVersion(options: bleepscript.PublishOptions): String =
    toScalaOpt(options.versionOverride)
      .orElse(toScalaOpt(options.versionFallback).map(_.get()))
      .getOrElse(
        throw new IllegalArgumentException(
          "PublishOptions requires either an explicit version or a versionFallback"
        )
      )

  private def toScalaOpt[T](o: Optional[T]): Option[T] =
    if (o.isPresent) Some(o.get) else None

  private def toNel(o: Optional[java.util.List[String]]): Option[NonEmptyList[String]] =
    toScalaOpt(o).flatMap { list =>
      val scalaList = list.asScala.toList
      if (scalaList.isEmpty) None else Some(NonEmptyList.fromListUnsafe(scalaList))
    }
}
