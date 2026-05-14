package bleep.javaapi

import bleep.packaging.{
  createJar as scalaCreateJar,
  packageLibraries,
  CoordinatesFor,
  IvyLayout,
  JarType as ScalaJarType,
  ManifestCreator as ScalaManifestCreator,
  MavenLayout,
  PackagedLibrary as ScalaPackagedLibrary,
  PublishLayout as ScalaPublishLayout
}
import bleep.nosbt.InteractionService
import bleep.plugin.pgp.PgpPlugin
import bleep.publishing.MavenRemotePublisher
import bleep.{constants, model, BleepException, Checksums, CredentialProvider, DiscardOps, FileSync, RelPath, Started}

import java.nio.file.Path
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

/** Bridge between Java {@code bleepscript.Packaging} surface and Scala {@code bleep.packaging.*}. */
object JPackaging {

  def packageProject(
      jstarted: bleepscript.Started,
      project: bleepscript.CrossProjectName,
      fallbackGroupId: String,
      version: String,
      layout: bleepscript.PublishLayout,
      manifestCreator: bleepscript.ManifestCreator
  ): bleepscript.PackagedLibrary = {
    val started = unwrapStarted(jstarted)
    val cross = JModel.toCross(project)
    val results = doPackage(started, Set(cross), fallbackGroupId, version, layout, manifestCreator)
    results.getOrElse(
      cross,
      throw new BleepException.Text(s"packageProject: project ${cross.value} did not produce a library")
    )
  }

  def packageProjects(
      jstarted: bleepscript.Started,
      projects: java.util.List[bleepscript.CrossProjectName],
      fallbackGroupId: String,
      version: String,
      layout: bleepscript.PublishLayout,
      manifestCreator: bleepscript.ManifestCreator
  ): java.util.Map[bleepscript.CrossProjectName, bleepscript.PackagedLibrary] = {
    val started = unwrapStarted(jstarted)
    val crosses = projects.asScala.iterator.map(JModel.toCross).toSet
    val results = doPackage(started, crosses, fallbackGroupId, version, layout, manifestCreator)
    results.iterator.map { case (cn, pl) => JModel.crossProjectName(cn) -> pl }.toMap.asJava
  }

  def createJar(
      jarType: bleepscript.JarType,
      manifestCreator: bleepscript.ManifestCreator,
      fromFolders: java.util.List[Path],
      projectName: java.util.Optional[bleepscript.CrossProjectName],
      mainClass: java.util.Optional[String]
  ): Array[Byte] = {
    val scalaType: ScalaJarType = jarType match {
      case bleepscript.JarType.JAR         => ScalaJarType.Jar
      case bleepscript.JarType.SOURCES_JAR => ScalaJarType.SourcesJar
      case bleepscript.JarType.DOCS_JAR    => ScalaJarType.DocsJar
    }
    val mc = unwrapManifest(manifestCreator)
    scalaCreateJar(
      scalaType,
      mc,
      fromFolders.asScala.toList,
      projectName = projectName.toScala.map(JModel.toCross),
      mainClass = mainClass.toScala
    )
  }

  def publishToLocalIvy(library: bleepscript.PackagedLibrary): Unit = {
    val files = filesFromJava(library)
    FileSync
      .syncBytes(
        constants.ivy2Path,
        files,
        deleteUnknowns = FileSync.DeleteUnknowns.No,
        soft = false
      )
      .discard()
  }

  def publishToLocalMaven(library: bleepscript.PackagedLibrary): Unit = {
    val files = filesFromJava(library)
    FileSync
      .syncBytes(
        constants.m2Path,
        files,
        deleteUnknowns = FileSync.DeleteUnknowns.No,
        soft = false
      )
      .discard()
  }

  def publishToFolder(library: bleepscript.PackagedLibrary, folder: Path): Unit = {
    val files = filesFromJava(library)
    FileSync
      .syncBytes(
        folder,
        files,
        deleteUnknowns = FileSync.DeleteUnknowns.No,
        soft = false
      )
      .discard()
  }

  def signArtifacts(
      jstarted: bleepscript.Started,
      library: bleepscript.PackagedLibrary
  ): bleepscript.PackagedLibrary = {
    val started = unwrapStarted(jstarted)
    val pgp = new PgpPlugin(
      logger = started.logger,
      maybeCredentials = None,
      interactionService = InteractionService.DoesNotMaskYourPasswordExclamationOneOne
    )
    val signed = pgp.signedArtifacts(filesFromJava(library))
    val javaFiles: java.util.Map[bleepscript.RelPath, Array[Byte]] =
      signed.iterator.map { case (rp, bytes) => JModel.relPath(rp) -> bytes }.toMap.asJava
    library.withFiles(javaFiles)
  }

  def publishToResolver(
      jstarted: bleepscript.Started,
      library: bleepscript.PackagedLibrary,
      resolverName: String
  ): Unit = {
    val started = unwrapStarted(jstarted)
    val name = model.ResolverName(resolverName)
    val resolvers = started.build.resolvers.values
    val repo = resolvers
      .find(_.name.contains(name))
      .getOrElse(
        throw new BleepException.Text(
          s"Resolver '$resolverName' not found in bleep.yaml. " +
            s"Available named resolvers: ${resolvers.flatMap(_.name.map(_.value)).mkString(", ")}"
        )
      )

    val files = filesFromJava(library)

    repo match {
      case model.Repository.MavenFolder(_, path) =>
        FileSync.syncBytes(path, files, deleteUnknowns = FileSync.DeleteUnknowns.No, soft = false).discard()

      case model.Repository.Maven(_, uri) =>
        publishHttp(started, files, uri)

      case model.Repository.Ivy(_, uri) =>
        publishHttp(started, files, uri)
    }
  }

  private def publishHttp(started: Started, files: Map[RelPath, Array[Byte]], repoUri: java.net.URI): Unit = {
    val credentialProvider = new CredentialProvider(started.logger, started.config.authentications)
    val resolved = MavenRemotePublisher.resolveTarget(repoUri, credentialProvider)
    val publisher = new MavenRemotePublisher(started.logger)
    val artifacts =
      if (resolved.uploadChecksums)
        Checksums(files, List(Checksums.Algorithm.Md5, Checksums.Algorithm.Sha1))
      else files
    publisher.publish(resolved.httpsUri, artifacts, resolved.authentication)
  }

  private def doPackage(
      started: Started,
      include: Set[model.CrossProjectName],
      fallbackGroupId: String,
      version: String,
      layout: bleepscript.PublishLayout,
      manifestCreator: bleepscript.ManifestCreator
  ): collection.SortedMap[model.CrossProjectName, bleepscript.PackagedLibrary] = {
    val publishLayout: ScalaPublishLayout = layout match {
      case _: bleepscript.PublishLayout.Maven => ScalaPublishLayout.Maven()
      case _: bleepscript.PublishLayout.Ivy   => ScalaPublishLayout.Ivy
    }
    val mc = unwrapManifest(manifestCreator)
    val packaged = packageLibraries(
      started,
      coordinatesFor = CoordinatesFor.FromModel(version = version, fallbackGroupId = fallbackGroupId),
      shouldInclude = include,
      publishLayout = publishLayout,
      manifestCreator = mc
    )
    packaged.map { case (cn, pl) => cn -> scalaToJavaLibrary(pl) }
  }

  private def scalaToJavaLibrary(pl: ScalaPackagedLibrary): bleepscript.PackagedLibrary = {
    val coords = depFromCoursier(pl.asDependency)
    val files: java.util.Map[bleepscript.RelPath, Array[Byte]] =
      pl.files.all.iterator.map { case (rp, bytes) => JModel.relPath(rp) -> bytes }.toMap.asJava
    pl.files match {
      case m: MavenLayout[bleep.RelPath @unchecked, Array[Byte] @unchecked] =>
        new bleepscript.PackagedLibrary(
          coords,
          JModel.relPath(m.jarFile._1),
          JModel.relPath(m.sourceFile._1),
          JModel.relPath(m.pomFile._1),
          JModel.relPath(m.docFile._1),
          java.util.Optional.empty[bleepscript.RelPath](),
          files
        )
      case i: IvyLayout[bleep.RelPath @unchecked, Array[Byte] @unchecked] =>
        new bleepscript.PackagedLibrary(
          coords,
          JModel.relPath(i.jarFile._1),
          JModel.relPath(i.sourceFile._1),
          JModel.relPath(i.pomFile._1),
          JModel.relPath(i.docFile._1),
          java.util.Optional.of(JModel.relPath(i.ivyFile._1)),
          files
        )
      case other =>
        throw new RuntimeException(s"Unexpected Layout type from packageLibraries: ${other.getClass}")
    }
  }

  /** Convert a resolved coursier Dependency to a flat Java Dep.Java. The cross-version, if any, is already baked into the moduleName. */
  private def depFromCoursier(d: coursier.core.Dependency): bleepscript.Dep =
    new bleepscript.Dep.Java(
      d.module.organization.value,
      d.module.name.value,
      d.version,
      true
    )

  private def filesFromJava(library: bleepscript.PackagedLibrary): Map[RelPath, Array[Byte]] =
    library.files.asScala.iterator.map { case (jrp, bytes) =>
      JModel.toRelPath(jrp) -> bytes
    }.toMap

  private def unwrapStarted(s: bleepscript.Started): Started = s match {
    case js: JStarted => js.underlying
    case _            => throw new RuntimeException(s"Unknown Started impl: ${s.getClass}")
  }

  private def unwrapManifest(m: bleepscript.ManifestCreator): ScalaManifestCreator = m match {
    case jm: JManifestCreator => jm.underlying
    case _                    => throw new RuntimeException(s"Unknown ManifestCreator impl: ${m.getClass}")
  }
}
