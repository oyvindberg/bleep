package bleep

import bleep.internal.{ScalaVersions, ShortenAndSortJson}
import bleep.internal.codecs._
import coursier.{ModuleName, Organization}
import coursier.core.{Versions => _, _}
import io.circe.syntax._
import io.circe.{Decoder, DecodingFailure, Encoder, Json}

sealed trait Dep {
  val organization: Organization
  val version: String
  val attributes: Map[String, String]
  val configuration: Configuration
  val exclusions: List[JavaOrScalaModule]
  val publication: Publication
  val optional: Boolean
  val transitive: Boolean

  final def repr: String =
    this match {
      case x: Dep.JavaDependency =>
        x.organization.value + ":" + x.moduleName.value + ":" + version

      case x: Dep.ScalaDependency =>
        x.organization.value +
          (if (x.fullCrossVersion) ":::" else "::") +
          x.baseModuleName.value +
          (if (x.withPlatformSuffix) "::" else ":") +
          version
    }

  def isSimple: Boolean

  // traverse is always the answer, unless you need unorderedFlatTraverse without parallelism
  final def translatedExclusions(versions: ScalaVersions): Either[String, Set[(Organization, ModuleName)]] = {
    var error: Option[String] = None
    val rights = Set.newBuilder[(Organization, ModuleName)]

    exclusions.foreach {
      case JavaOrScalaModule.JavaModule(module) =>
        rights += ((module.organization, module.name))

      case mod @ JavaOrScalaModule.ScalaModule(baseModule, fullCrossVersion) =>
        versions match {
          case withScala: ScalaVersions.WithScala =>
            // there is no place in the format to indicate platform. we exclude both when needed
            withScala.moduleName(baseModule.name, needsScala = true, fullCrossVersion, needsPlatformSuffix = false) match {
              case Some(moduleName) => rights += ((baseModule.organization, moduleName))
              case None             => error = Some(s"Cannot include dependency with $withScala")
            }

            withScala.moduleName(baseModule.name, needsScala = true, fullCrossVersion, needsPlatformSuffix = true) match {
              case Some(moduleName) =>
                rights += ((baseModule.organization, moduleName))
              case None => () // it's ok if this fails
            }

          case ScalaVersions.Java =>
            error = Some(s"It doesn't make sense to exclude $mod in a pure java project")
        }
    }

    error.toLeft(rights.result())
  }

  final def dependency(versions: ScalaVersions): Either[String, Dependency] =
    this match {
      case x: Dep.JavaDependency =>
        val module = Module(x.organization, x.moduleName, x.attributes)
        translatedExclusions(versions).map { exclusions =>
          new Dependency(module, version, configuration = configuration, exclusions, publication, optional, transitive)
        }
      case x: Dep.ScalaDependency =>
        versions match {
          case withScala: ScalaVersions.WithScala =>
            withScala.moduleName(x.baseModuleName, needsScala = true, x.fullCrossVersion, x.withPlatformSuffix) match {
              case Some(moduleName) =>
                val module = Module(x.organization, moduleName, x.attributes)
                translatedExclusions(versions).map(exclusions => new Dependency(module, version, configuration, exclusions, publication, optional, transitive))
              case None =>
                Left(s"Cannot include dependency with $withScala")
            }

          case ScalaVersions.Java =>
            Left(s"You need to configure a scala version to resolve ${x.repr}")
        }
    }

  final def dependency(versions: ScalaVersions.WithScala): Dependency =
    dependency(versions: ScalaVersions) match {
      case Left(err)  => sys.error(s"should not happen (tm). got error $err")
      case Right(dep) => dep
    }
}

object Dep {
  def Java(org: String, name: String, version: String): Dep.JavaDependency =
    Dep.JavaDependency(Organization(org), ModuleName(name), version)

  def Scala(org: String, name: String, version: String): Dep =
    Dep.ScalaDependency(Organization(org), ModuleName(name), version, fullCrossVersion = false, withPlatformSuffix = false)

  def ScalaFullVersion(org: String, name: String, version: String): Dep =
    Dep.ScalaDependency(Organization(org), ModuleName(name), version, fullCrossVersion = true, withPlatformSuffix = false)

  def parse(input: String): Either[String, Dep] =
    input.split(":") match {
      case Array(org, "", "", name, version) =>
        Right(Dep.ScalaDependency(Organization(org), ModuleName(name), version, fullCrossVersion = true, withPlatformSuffix = false))
      case Array(org, "", name, version) =>
        Right(Dep.ScalaDependency(Organization(org), ModuleName(name), version, fullCrossVersion = false, withPlatformSuffix = false))
      case Array(org, "", name, "", version) =>
        Right(Dep.ScalaDependency(Organization(org), ModuleName(name), version, fullCrossVersion = false, withPlatformSuffix = true))
      case Array(org, name, version) =>
        Right(Dep.JavaDependency(Organization(org), ModuleName(name), version))
      case _ =>
        Left(s"Not a valid dependency string: $input")
    }

  object defaults {
    val attributes: Map[String, String] = Map.empty
    val configuration: Configuration = Configuration.empty
    val exclusions: List[JavaOrScalaModule] = Nil
    val publication: Publication = Publication.empty
    val optional: Boolean = false
    val transitive: Boolean = true
    val fullCrossVersion: Boolean = false
    val withPlatformSuffix: Boolean = false
  }

  case class JavaDependency(
      organization: Organization,
      moduleName: ModuleName,
      version: String,
      attributes: Map[String, String] = defaults.attributes,
      configuration: Configuration = defaults.configuration,
      exclusions: List[JavaOrScalaModule] = defaults.exclusions,
      publication: Publication = defaults.publication,
      optional: Boolean = defaults.optional,
      transitive: Boolean = defaults.transitive
  ) extends Dep {
    final def isSimple: Boolean =
      attributes == defaults.attributes &&
        configuration == defaults.configuration &&
        exclusions == defaults.exclusions &&
        publication == defaults.publication &&
        optional == defaults.optional &&
        transitive == defaults.transitive
  }

  case class ScalaDependency(
      organization: Organization,
      baseModuleName: ModuleName,
      version: String,
      fullCrossVersion: Boolean,
      withPlatformSuffix: Boolean,
      attributes: Map[String, String] = defaults.attributes,
      configuration: Configuration = defaults.configuration,
      exclusions: List[JavaOrScalaModule] = defaults.exclusions,
      publication: Publication = defaults.publication,
      optional: Boolean = defaults.optional,
      transitive: Boolean = defaults.transitive
  ) extends Dep {
    final def isSimple: Boolean =
      attributes == defaults.attributes &&
        configuration == defaults.configuration &&
        exclusions == defaults.exclusions &&
        publication == defaults.publication &&
        optional == defaults.optional &&
        transitive == defaults.transitive
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
              exclusions <- c.get[Option[List[JavaOrScalaModule]]]("exclusions")
              publicationC = c.downField("publication")
              publicationName <- publicationC.get[Option[String]]("name")
              publicationType <- publicationC.get[Option[Type]]("type")
              publicationExt <- publicationC.get[Option[Extension]]("ext")
              publicationClassifier <- publicationC.get[Option[Classifier]]("classifier")
              optional <- c.get[Option[Boolean]]("optional")
              transitive <- c.get[Option[Boolean]]("transitive")
            } yield Dep.JavaDependency(
              organization = dependency.organization,
              moduleName = dependency.moduleName,
              version = dependency.version,
              attributes = attributes.getOrElse(dependency.attributes),
              configuration = configuration.getOrElse(dependency.configuration),
              exclusions = exclusions.getOrElse(dependency.exclusions),
              publication = Publication(
                name = publicationName.getOrElse(dependency.publication.name),
                `type` = publicationType.getOrElse(dependency.publication.`type`),
                ext = publicationExt.getOrElse(dependency.publication.ext),
                classifier = publicationClassifier.getOrElse(dependency.publication.classifier)
              ),
              optional = optional.getOrElse(dependency.optional),
              transitive = transitive.getOrElse(dependency.transitive)
            )

          case dependency: Dep.ScalaDependency =>
            for {
              attributes <- c.get[Option[Map[String, String]]]("attributes")
              configuration <- c.get[Option[Configuration]]("configuration")
              exclusions <- c.get[Option[List[JavaOrScalaModule]]]("exclusions")
              publicationC = c.downField("publication")
              publicationName <- publicationC.get[Option[String]]("name")
              publicationType <- publicationC.get[Option[Type]]("type")
              publicationExt <- publicationC.get[Option[Extension]]("ext")
              publicationClassifier <- publicationC.get[Option[Classifier]]("classifier")
              optional <- c.get[Option[Boolean]]("optional")
              transitive <- c.get[Option[Boolean]]("transitive")
            } yield Dep.ScalaDependency(
              organization = dependency.organization,
              baseModuleName = dependency.baseModuleName,
              version = dependency.version,
              fullCrossVersion = dependency.fullCrossVersion,
              withPlatformSuffix = dependency.withPlatformSuffix,
              attributes = attributes.getOrElse(dependency.attributes),
              configuration = configuration.getOrElse(dependency.configuration),
              exclusions = exclusions.getOrElse(dependency.exclusions),
              publication = Publication(
                name = publicationName.getOrElse(dependency.publication.name),
                `type` = publicationType.getOrElse(dependency.publication.`type`),
                ext = publicationExt.getOrElse(dependency.publication.ext),
                classifier = publicationClassifier.getOrElse(dependency.publication.classifier)
              ),
              optional = optional.getOrElse(dependency.optional),
              transitive = transitive.getOrElse(dependency.transitive)
            )
        }

      fromString.orElse(full)
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
            "optional" := (if (x.optional == Dep.defaults.optional) Json.Null else x.optional.asJson),
            "transitive" := (if (x.transitive == Dep.defaults.transitive) Json.Null else x.transitive.asJson)
          )
          .foldWith(ShortenAndSortJson)
      case x: ScalaDependency =>
        Json
          .obj(
            "module" := x.repr.asJson,
            "attributes" := (if (x.attributes == Dep.defaults.attributes) Json.Null else x.attributes.asJson),
            "configuration" := (if (x.configuration == Dep.defaults.configuration) Json.Null else x.configuration.asJson),
            "exclusions" := (if (x.exclusions == Dep.defaults.exclusions) Json.Null else x.exclusions.asJson),
            "publication" := (if (x.publication == Dep.defaults.publication) Json.Null else x.publication.asJson),
            "optional" := (if (x.optional == Dep.defaults.optional) Json.Null else x.optional.asJson),
            "transitive" := (if (x.transitive == Dep.defaults.transitive) Json.Null else x.transitive.asJson)
          )
          .foldWith(ShortenAndSortJson)
    }

  implicit val ordering: Ordering[Dep] =
    Ordering.by(dep => (dep.repr, dep.toString))
}
