package bleep

import bleep.internal.codecs._
import bleep.internal.{ScalaVersions, ShortenAndSortJson}
import coursier.core._
import io.circe.syntax._
import io.circe.{Decoder, DecodingFailure, Encoder, Json}

sealed trait Dep {
  val organization: Organization
  val version: String
  val attributes: Map[String, String]
  val configuration: Configuration
  val exclusions: JsonMap[Organization, JsonSet[ModuleName]]
  val publication: Publication
  val transitive: Boolean

  val repr: String
  def isSimple: Boolean
  def dependency(versions: ScalaVersions): Either[String, Dependency]
  def dependencyForce(projectName: model.CrossProjectName, versions: ScalaVersions): Dependency =
    dependency(versions) match {
      case Left(err)    => throw new BuildException.Text(projectName, err)
      case Right(value) => value
    }

  def withConfiguration(newConfiguration: Configuration): Dep
  def withVersion(version: String): Dep

  final def forceDependency(versions: ScalaVersions): Dependency =
    dependency(versions) match {
      case Left(err)         => throw new RuntimeException(s"Unexpected: $err")
      case Right(dependency) => dependency
    }
}

object Dep {
  def Java(org: String, name: String, version: String): Dep.JavaDependency =
    Dep.JavaDependency(Organization(org), ModuleName(name), version)

  def Scala(org: String, name: String, version: String): Dep.ScalaDependency =
    Dep.ScalaDependency(Organization(org), ModuleName(name), version, fullCrossVersion = false)

  def ScalaFullVersion(org: String, name: String, version: String): Dep.ScalaDependency =
    Dep.ScalaDependency(Organization(org), ModuleName(name), version, fullCrossVersion = true)

  def parse(input: String): Either[String, Dep] =
    input.split(":") match {
      case Array(org, "", "", name, version) => Right(ScalaFullVersion(org, name, version))
      case Array(org, "", name, version)     => Right(Scala(org, name, version))
      case Array(org, name, version)         => Right(Java(org, name, version))
      case _                                 => Left(s"Not a valid dependency string: $input")
    }

  object defaults {
    val forceJvm: Boolean = false
    val for3Use213: Boolean = false
    val for213Use3: Boolean = false
    val attributes: Map[String, String] = Map.empty
    val configuration: Configuration = Configuration.empty
    val exclusions: JsonMap[Organization, JsonSet[ModuleName]] = JsonMap.empty
    val publication: Publication = Publication.empty
    val transitive: Boolean = true
    val fullCrossVersion: Boolean = false
    val isSbtPlugin: Boolean = false
  }

  val SbtPluginAttrs: Map[String, String] = Map("sbtVersion" -> "1.0", "scalaVersion" -> "2.12")

  final case class JavaDependency(
      organization: Organization,
      moduleName: ModuleName,
      version: String,
      attributes: Map[String, String] = defaults.attributes,
      configuration: Configuration = defaults.configuration,
      exclusions: JsonMap[Organization, JsonSet[ModuleName]] = defaults.exclusions,
      publication: Publication = defaults.publication,
      transitive: Boolean = defaults.transitive,
      isSbtPlugin: Boolean = defaults.isSbtPlugin
  ) extends Dep {
    def isSimple: Boolean =
      attributes == defaults.attributes &&
        configuration == defaults.configuration &&
        exclusions == defaults.exclusions &&
        publication == defaults.publication &&
        transitive == defaults.transitive &&
        isSbtPlugin == defaults.isSbtPlugin

    val repr: String =
      organization.value + ":" + moduleName.value + ":" + version

    override def dependency(versions: ScalaVersions): Either[String, Dependency] =
      Right(this.dependency)

    def dependency: Dependency =
      new Dependency(
        module = new Module(
          organization,
          moduleName,
          if (isSbtPlugin) Dep.SbtPluginAttrs ++ attributes else attributes
        ),
        version = version,
        configuration = configuration,
        exclusions = exclusions.value.flatMap { case (org, moduleNames) => moduleNames.values.map(moduleName => (org, moduleName)) }.toSet,
        publication = publication,
        optional = false, // todo: we now express this in configuration. also here?
        transitive = transitive
      )

    override def withConfiguration(newConfiguration: Configuration): Dep.JavaDependency =
      copy(configuration = newConfiguration)

    override def withVersion(version: String): Dep.JavaDependency = copy(version = version)
  }

  final case class ScalaDependency(
      organization: Organization,
      baseModuleName: ModuleName,
      version: String,
      fullCrossVersion: Boolean,
      forceJvm: Boolean = defaults.forceJvm,
      for3Use213: Boolean = defaults.for3Use213,
      for213Use3: Boolean = defaults.for213Use3,
      attributes: Map[String, String] = defaults.attributes,
      configuration: Configuration = defaults.configuration,
      exclusions: JsonMap[Organization, JsonSet[ModuleName]] = defaults.exclusions,
      publication: Publication = defaults.publication,
      transitive: Boolean = defaults.transitive
  ) extends Dep {
    def isSimple: Boolean =
      forceJvm == defaults.forceJvm &&
        for3Use213 == defaults.for3Use213 &&
        for213Use3 == defaults.for213Use3 &&
        attributes == defaults.attributes &&
        configuration == defaults.configuration &&
        exclusions == defaults.exclusions &&
        publication == defaults.publication &&
        transitive == defaults.transitive

    val repr: String =
      s"${organization.value}${if (fullCrossVersion) ":::" else "::"}${baseModuleName.value}:$version"

    def dependency(versions: ScalaVersions): Either[String, Dependency] =
      versions match {
        case withScala: ScalaVersions.WithScala =>
          withScala.moduleName(
            baseModuleName,
            needsScala = true,
            needsFullCrossVersion = fullCrossVersion,
            forceJvm = forceJvm,
            for3Use213 = for3Use213,
            for213Use3 = for213Use3
          ) match {
            case Some(moduleName) =>
              Right(
                new Dependency(
                  module = new Module(organization, moduleName, attributes),
                  version = version,
                  configuration = configuration,
                  exclusions = exclusions.value.flatMap { case (org, moduleNames) => moduleNames.values.map(moduleName => (org, moduleName)) }.toSet,
                  publication = publication,
                  optional = false, // todo: we now express this in configuration. also here?
                  transitive = transitive
                )
              )
            case None =>
              Left(s"Cannot include dependency with $withScala")
          }

        case ScalaVersions.Java =>
          Left(s"You need to configure a scala version to resolve $repr")
      }

    override def withConfiguration(newConfiguration: Configuration): Dep.ScalaDependency =
      copy(configuration = newConfiguration)

    override def withVersion(version: String): Dep.ScalaDependency = copy(version = version)
  }

  implicit val decodes: Decoder[Dep] =
    Decoder.instance { c =>
      def parseR(input: String): Decoder.Result[Dep] =
        parse(input).left.map(err => DecodingFailure(err, c.history))

      val fromString: Decoder.Result[Dep] =
        c.as[String].flatMap(parseR)

      val full: Decoder.Result[Dep] =
        c.get[String]("module").flatMap(parseR).flatMap {
          case dependency: Dep.JavaDependency =>
            for {
              attributes <- c.get[Option[Map[String, String]]]("attributes")
              configuration <- c.get[Option[Configuration]]("configuration")
              exclusions <- c.get[JsonMap[Organization, JsonSet[ModuleName]]]("exclusions")
              publicationC = c.downField("publication")
              publicationName <- publicationC.get[Option[String]]("name")
              publicationType <- publicationC.get[Option[Type]]("type")
              publicationExt <- publicationC.get[Option[Extension]]("ext")
              publicationClassifier <- publicationC.get[Option[Classifier]]("classifier")
              transitive <- c.get[Option[Boolean]]("transitive")
              isSbtPlugin <- c.get[Option[Boolean]]("isSbtPlugin")
            } yield Dep.JavaDependency(
              organization = dependency.organization,
              moduleName = dependency.moduleName,
              version = dependency.version,
              attributes = attributes.getOrElse(dependency.attributes),
              configuration = configuration.getOrElse(dependency.configuration),
              exclusions = exclusions,
              publication = Publication(
                name = publicationName.getOrElse(dependency.publication.name),
                `type` = publicationType.getOrElse(dependency.publication.`type`),
                ext = publicationExt.getOrElse(dependency.publication.ext),
                classifier = publicationClassifier.getOrElse(dependency.publication.classifier)
              ),
              transitive = transitive.getOrElse(dependency.transitive),
              isSbtPlugin = isSbtPlugin.getOrElse(dependency.isSbtPlugin)
            )

          case dependency: Dep.ScalaDependency =>
            for {
              forceJvm <- c.get[Option[Boolean]]("forceJvm")
              for3Use213 <- c.get[Option[Boolean]]("for3Use213")
              for213Use3 <- c.get[Option[Boolean]]("for213Use3")
              attributes <- c.get[Option[Map[String, String]]]("attributes")
              configuration <- c.get[Option[Configuration]]("configuration")
              exclusions <- c.get[JsonMap[Organization, JsonSet[ModuleName]]]("exclusions")
              publicationC = c.downField("publication")
              publicationName <- publicationC.get[Option[String]]("name")
              publicationType <- publicationC.get[Option[Type]]("type")
              publicationExt <- publicationC.get[Option[Extension]]("ext")
              publicationClassifier <- publicationC.get[Option[Classifier]]("classifier")
              transitive <- c.get[Option[Boolean]]("transitive")
            } yield Dep.ScalaDependency(
              organization = dependency.organization,
              baseModuleName = dependency.baseModuleName,
              version = dependency.version,
              fullCrossVersion = dependency.fullCrossVersion,
              forceJvm = forceJvm.getOrElse(dependency.forceJvm),
              for3Use213 = for3Use213.getOrElse(dependency.for3Use213),
              for213Use3 = for213Use3.getOrElse(dependency.for213Use3),
              attributes = attributes.getOrElse(dependency.attributes),
              configuration = configuration.getOrElse(dependency.configuration),
              exclusions = exclusions,
              publication = Publication(
                name = publicationName.getOrElse(dependency.publication.name),
                `type` = publicationType.getOrElse(dependency.publication.`type`),
                ext = publicationExt.getOrElse(dependency.publication.ext),
                classifier = publicationClassifier.getOrElse(dependency.publication.classifier)
              ),
              transitive = transitive.getOrElse(dependency.transitive)
            )
        }

      fromString match {
        case Left(_)          => full
        case right @ Right(_) => right
      }
    }

  private implicit val publicationEncoder: Encoder[Publication] = Encoder.instance(p =>
    Json.obj(
      "name" := (if (p.name == Dep.defaults.publication.name) Json.Null else p.name.asJson),
      "type" := (if (p.`type` == Dep.defaults.publication.`type`) Json.Null else p.`type`.asJson),
      "ext" := (if (p.ext == Dep.defaults.publication.ext) Json.Null else p.ext.asJson),
      "classifier" := (if (p.classifier == Dep.defaults.publication.classifier) Json.Null else p.classifier.asJson)
    )
  )

  implicit val encodes: Encoder[Dep] =
    Encoder.instance {
      case d if d.isSimple => d.repr.asJson
      case x: Dep.JavaDependency =>
        Json
          .obj(
            "module" := x.repr.asJson,
            "attributes" := (if (x.attributes == Dep.defaults.attributes) Json.Null else x.attributes.asJson),
            "configuration" := (if (x.configuration == Dep.defaults.configuration) Json.Null else x.configuration.asJson),
            "exclusions" := (if (x.exclusions == Dep.defaults.exclusions) Json.Null else x.exclusions.asJson),
            "publication" := (if (x.publication == Dep.defaults.publication) Json.Null else x.publication.asJson),
            "transitive" := (if (x.transitive == Dep.defaults.transitive) Json.Null else x.transitive.asJson),
            "isSbtPlugin" := (if (x.isSbtPlugin == Dep.defaults.isSbtPlugin) Json.Null else x.isSbtPlugin.asJson)
          )
          .foldWith(ShortenAndSortJson)
      case x: ScalaDependency =>
        Json
          .obj(
            "forceJvm" := (if (x.forceJvm == Dep.defaults.forceJvm) Json.Null else x.forceJvm.asJson),
            "for3Use213" := (if (x.for3Use213 == Dep.defaults.for3Use213) Json.Null else x.for3Use213.asJson),
            "for213Use3" := (if (x.for213Use3 == Dep.defaults.for213Use3) Json.Null else x.for213Use3.asJson),
            "module" := x.repr.asJson,
            "attributes" := (if (x.attributes == Dep.defaults.attributes) Json.Null else x.attributes.asJson),
            "configuration" := (if (x.configuration == Dep.defaults.configuration) Json.Null else x.configuration.asJson),
            "exclusions" := (if (x.exclusions == Dep.defaults.exclusions) Json.Null else x.exclusions.asJson),
            "publication" := (if (x.publication == Dep.defaults.publication) Json.Null else x.publication.asJson),
            "transitive" := (if (x.transitive == Dep.defaults.transitive) Json.Null else x.transitive.asJson)
          )
          .foldWith(ShortenAndSortJson)
    }

  implicit val ordering: Ordering[Dep] =
    (x: Dep, y: Dep) =>
      x.repr.compare(y.repr) match {
        case 0 => x.toString.compareTo(y.toString) // this is rather slow
        case n => n
      }
}
