package bleep.javaapi

import bleep.commands.{LinkOptions, Publish, PublishLocal, PublishSonatype, PublishVersion}
import bleep.packaging.ManifestCreator
import bleep.{model, Commands, Started}
import cats.data.NonEmptyList

import java.util.Optional
import scala.jdk.CollectionConverters.*

final class JCommands(started: Started) extends bleepscript.Commands {
  private val underlying = new Commands(started)

  override def compile(projects: java.util.List[bleepscript.CrossProjectName]): bleepscript.CompileReport =
    compile(projects, watch = false)

  override def compile(projects: java.util.List[bleepscript.CrossProjectName], watch: Boolean): bleepscript.CompileReport = {
    val summary = underlying.compile(projects.asScala.iterator.map(JModel.toCross).toList, watch = watch)
    // Deliberately narrower than the summary it comes from. `BuildSummary` is bleep's internal accounting and changes shape whenever the build display does; a
    // published Java interface should not.
    new bleepscript.CompileReport(
      summary.noOp,
      summary.upToDateProjects.map(JModel.crossProjectName).asJava,
      summary.compilesCompleted
    )
  }

  override def link(projects: java.util.List[bleepscript.CrossProjectName], options: bleepscript.LinkOptions): bleepscript.LinkReport =
    link(projects, options, watch = false)

  override def link(projects: java.util.List[bleepscript.CrossProjectName], options: bleepscript.LinkOptions, watch: Boolean): bleepscript.LinkReport = {
    val summary = underlying.link(
      projects.asScala.iterator.map(JModel.toCross).toList,
      LinkOptions(
        releaseMode = options.releaseMode,
        sourceMaps = toScalaOpt(options.sourceMaps).map(_.booleanValue),
        minify = toScalaOpt(options.minify).map(_.booleanValue),
        moduleKind = toScalaOpt(options.moduleKind).map {
          case bleepscript.LinkOptions.ModuleKind.NO_MODULE => LinkOptions.ModuleKind.NoModule
          case bleepscript.LinkOptions.ModuleKind.COMMON_JS => LinkOptions.ModuleKind.CommonJS
          case bleepscript.LinkOptions.ModuleKind.ES_MODULE => LinkOptions.ModuleKind.ESModule
        },
        lto = toScalaOpt(options.lto).map {
          case bleepscript.LinkOptions.LTO.NONE => LinkOptions.LTO.None
          case bleepscript.LinkOptions.LTO.THIN => LinkOptions.LTO.Thin
          case bleepscript.LinkOptions.LTO.FULL => LinkOptions.LTO.Full
        },
        optimize = toScalaOpt(options.optimize).map(_.booleanValue),
        debugInfo = toScalaOpt(options.debugInfo).map(_.booleanValue)
      ),
      watch = watch
    )
    new bleepscript.LinkReport(
      summary.linkedOutputs.map { out =>
        new bleepscript.LinkedOutput(JModel.crossProjectName(out.project), out.platform.wireValue, out.files.asJava)
      }.asJava
    )
  }

  override def test(projects: java.util.List[bleepscript.CrossProjectName]): bleepscript.TestReport =
    toTestReport(
      underlying.test(
        projects.asScala.iterator.map(JModel.toCross).toList,
        watch = false,
        only = None,
        exclude = None,
        includeTags = None,
        excludeTags = None
      )
    )

  override def test(
      projects: java.util.List[bleepscript.CrossProjectName],
      watch: Boolean,
      only: Optional[java.util.List[String]],
      exclude: Optional[java.util.List[String]]
  ): bleepscript.TestReport =
    toTestReport(
      underlying.test(
        projects.asScala.iterator.map(JModel.toCross).toList,
        watch = watch,
        only = toNel(only),
        exclude = toNel(exclude),
        includeTags = None,
        excludeTags = None
      )
    )

  /** Narrower than the summary it comes from, for the same reason [[bleepscript.CompileReport]] is. */
  private def toTestReport(summary: bleep.testing.BuildSummary): bleepscript.TestReport =
    new bleepscript.TestReport(summary.testsTotal, summary.testsPassed, summary.testsSkipped, summary.testsIgnored, summary.suitesTotal)

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
    val opts = PublishLocal.Options(
      groupId = options.groupId,
      version = publishVersion(options.version),
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
    options.target match {
      case _: bleepscript.PublishTarget.LocalIvy =>
        val target = PublishLocal.LocalIvy
        val opts = PublishLocal.Options(
          groupId = options.groupId,
          version = publishVersion(options.version),
          publishTarget = target,
          projects = options.projects.asScala.iterator.map(JModel.toCross).toArray,
          manifestCreator = manifestCreator
        )
        underlying.publishLocal(opts, watch = watch)

      case mf: bleepscript.PublishTarget.MavenFolder =>
        val target = PublishLocal.CustomMaven(model.Repository.MavenFolder(None, mf.path))
        val opts = PublishLocal.Options(
          groupId = options.groupId,
          version = publishVersion(options.version),
          publishTarget = target,
          projects = options.projects.asScala.iterator.map(JModel.toCross).toArray,
          manifestCreator = manifestCreator
        )
        underlying.publishLocal(opts, watch = watch)

      case r: bleepscript.PublishTarget.Resolver =>
        val opts = Publish.Options(
          version = publishVersion(options.version),
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
        runPublishSonatype(options, manifestCreator)
    }
  }

  override def publishSonatype(options: bleepscript.PublishOptions): Unit = {
    val manifestCreator: ManifestCreator = toScalaOpt(options.manifestCreator) match {
      case Some(mc: JManifestCreator) => mc.underlying
      case Some(other)                => throw new RuntimeException(s"Unknown ManifestCreator impl: ${other.getClass}")
      case None                       => ManifestCreator.default
    }
    runPublishSonatype(options, manifestCreator)
  }

  private def runPublishSonatype(
      options: bleepscript.PublishOptions,
      manifestCreator: ManifestCreator
  ): Unit =
    underlying.publishSonatype(
      PublishSonatype.Options(
        version = publishVersion(options.version),
        assertRelease = options.assertRelease,
        projectNames = options.projects.asScala.iterator.map(JModel.toCross).toArray,
        manifestCreator = manifestCreator
      )
    )

  private def publishVersion(v: bleepscript.PublishVersion): PublishVersion =
    v match {
      case s: bleepscript.PublishVersion.Specified => PublishVersion.Specified(s.value)
      case _: bleepscript.PublishVersion.Dynver    => PublishVersion.Dynver
    }

  private def toScalaOpt[T](o: Optional[T]): Option[T] =
    if (o.isPresent) Some(o.get) else None

  private def toNel(o: Optional[java.util.List[String]]): Option[NonEmptyList[String]] =
    toScalaOpt(o).flatMap { list =>
      val scalaList = list.asScala.toList
      if (scalaList.isEmpty) None else Some(NonEmptyList.fromListUnsafe(scalaList))
    }
}
