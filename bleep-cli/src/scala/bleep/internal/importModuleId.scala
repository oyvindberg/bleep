package bleep.internal

import bleep.logging.Logger
import bleep.model.{Dep, JsonMap, JsonSet}
import bleep.model
import coursier.core.{Classifier, Configuration, Extension, Module, ModuleName, Organization, Publication, Type}
import sbt.librarymanagement

object importModuleId {
  def apply(logger: Logger, moduleID: librarymanagement.ModuleID, platformId: Option[model.PlatformId]): Either[String, Dep] = {
    val ctxLogger = logger.withContext(moduleID.name)

    val configuration = moduleID.configurations.fold(Configuration.empty)(Configuration(_))

    moduleID.inclusions.foreach { rule =>
      ctxLogger.warn(s"Dropping inclusion rule $rule")
    }
    moduleID.branchName.foreach { branchName =>
      ctxLogger.warn(s"Dropping branchName $branchName")
    }

    val exclusions: JsonMap[Organization, JsonSet[ModuleName]] = {
      def mapRule(rule: librarymanagement.InclExclRule): Either[String, (Organization, ModuleName)] =
        if (rule.configurations.nonEmpty) Left(s"Configurations in rule not supported: ${rule.configurations}")
        else if (rule.artifact != "*") Left(s"Only artifact = '*' is supported, not ${rule.artifact}")
        else if (rule.crossVersion != librarymanagement.Disabled) Left(s"Only ModuleVersion.Disabled is supported: ${rule.crossVersion}")
        else Right((Organization(rule.organization), ModuleName(rule.name)))

      JsonMap {
        moduleID.exclusions
          .flatMap { rule =>
            mapRule(rule) match {
              case Left(msg) =>
                ctxLogger.warn(s"Dropping exclusion rule $rule: $msg")
                Nil
              case Right(tuple) => List(tuple)
            }
          }
          .groupBy { case (org, _) => org }
          .map { case (org, tuples) => (org, JsonSet.fromIterable(tuples.map(_._2))) }
      }
    }

    val publication =
      moduleID.explicitArtifacts.toList match {
        case a :: tail =>
          if (tail.nonEmpty)
            ctxLogger.warn(s"Dropping ${tail.length} explicitArtifacts")

          Publication(
            a.name,
            `type` = Type(a.`type`),
            ext = Extension(a.extension),
            classifier = a.classifier.fold(Classifier.empty)(Classifier.apply)
          )
        case Nil => Publication.empty
      }

    JavaOrScalaModule.parse(platformId, moduleID.organization, moduleID.name, moduleID.crossVersion).map {
      case x: JavaOrScalaModule.JavaModule =>
        val (isSbtPlugin, attrs) = extractIsSbt(moduleID.extraAttributes)
        Dep.JavaDependency(
          organization = x.module.organization,
          moduleName = x.module.name,
          version = moduleID.revision,
          attributes = attrs,
          configuration = configuration,
          exclusions = exclusions,
          publication = publication,
          transitive = moduleID.isTransitive,
          isSbtPlugin = isSbtPlugin
        )
      case x: JavaOrScalaModule.ScalaModule =>
        Dep.ScalaDependency(
          organization = x.baseModule.organization,
          baseModuleName = x.baseModule.name,
          version = moduleID.revision,
          fullCrossVersion = x.fullCrossVersion,
          forceJvm = x.forceJvm,
          for3Use213 = x.for3Use213,
          for213Use3 = x.for213Use3,
          attributes = moduleID.extraAttributes,
          configuration = configuration,
          exclusions = exclusions,
          publication = publication,
          transitive = moduleID.isTransitive
        )
    }
  }

  sealed trait JavaOrScalaModule
  object JavaOrScalaModule {
    case class JavaModule(module: Module) extends JavaOrScalaModule
    case class ScalaModule(baseModule: Module, fullCrossVersion: Boolean, forceJvm: Boolean, for3Use213: Boolean, for213Use3: Boolean) extends JavaOrScalaModule

    def parse(
        platform: Option[model.PlatformId],
        _org: String,
        _name: String,
        crossVersion: librarymanagement.CrossVersion
    ): Either[String, JavaOrScalaModule] = {
      val org = Organization(_org)
      val name = ModuleName(_name)

      def isForceJvm(prefix: String): Boolean =
        platform match {
          case Some(model.PlatformId.Js)     => prefix.isEmpty
          case Some(model.PlatformId.Native) => prefix.isEmpty
          case _                             => false
        }

      val mod = Module(org, name, Map.empty)

      crossVersion match {
        case librarymanagement.Disabled =>
          Right(JavaOrScalaModule.JavaModule(mod))
        case x: librarymanagement.Binary =>
          Right(JavaOrScalaModule.ScalaModule(mod, fullCrossVersion = false, forceJvm = isForceJvm(x.prefix), for3Use213 = false, for213Use3 = false))
        case x: librarymanagement.Full =>
          Right(JavaOrScalaModule.ScalaModule(mod, fullCrossVersion = true, forceJvm = isForceJvm(x.prefix), for3Use213 = false, for213Use3 = false))
        case _: librarymanagement.Patch =>
          Right(JavaOrScalaModule.ScalaModule(mod, fullCrossVersion = true, forceJvm = true, for3Use213 = false, for213Use3 = false))
        case x: librarymanagement.For2_13Use3 =>
          Right(JavaOrScalaModule.ScalaModule(mod, fullCrossVersion = false, forceJvm = isForceJvm(x.prefix), for3Use213 = false, for213Use3 = true))
        case x: librarymanagement.For3Use2_13 =>
          Right(JavaOrScalaModule.ScalaModule(mod, fullCrossVersion = false, forceJvm = isForceJvm(x.prefix), for3Use213 = true, for213Use3 = false))
        case x =>
          Left(s"CrossVersion $x is not supported")
      }
    }
  }

  def extractIsSbt(attrs: Map[String, String]): (Boolean, Map[String, String]) = {
    var hasSbtVersion = false
    var hasScalaVersion = false
    val b = Map.newBuilder[String, String]
    attrs.foreach { case (k, v) =>
      k.split(":").last match {
        case "sbtVersion"   => hasSbtVersion = true
        case "scalaVersion" => hasScalaVersion = true
        case _              => b += ((k, v))
      }
    }

    if (hasSbtVersion && hasScalaVersion) (true, b.result())
    else (false, attrs)
  }
}
