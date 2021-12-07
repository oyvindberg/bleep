package bleep

import cats.syntax.traverse._
import coursier.core.{Attributes, Classifier, Configuration, Dependency, ModuleName}
import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import io.circe.syntax._

sealed trait JavaOrScalaDependency {
  def module: JavaOrScalaModule
  def version: String
  def exclude: Set[JavaOrScalaModule]
  def addExclude(excl: JavaOrScalaModule*): JavaOrScalaDependency
  def dependency(scalaVersion: Versions.Scala, platformName: String): Dependency

  def withPlatform(platformSuffix: String): JavaOrScalaDependency

  def withUnderlyingDependency(f: Dependency => Dependency): JavaOrScalaDependency
}

object JavaOrScalaDependency {

  def apply(mod: JavaOrScalaModule, dep: Dependency): JavaOrScalaDependency =
    mod match {
      case j: JavaOrScalaModule.JavaModule =>
        JavaDependency(dep.withModule(j.module), Set.empty)
      case s: JavaOrScalaModule.ScalaModule =>
        ScalaDependency(dep.withModule(s.baseModule), s.fullCrossVersion, withPlatformSuffix = false, Set.empty)
    }

  case class JavaDependency(dependency: Dependency, exclude: Set[JavaOrScalaModule]) extends JavaOrScalaDependency {
    def module: JavaOrScalaModule.JavaModule =
      JavaOrScalaModule.JavaModule(dependency.module)
    def version: String =
      dependency.version
    def dependency(scalaVersion: Versions.Scala, platformName: String): Dependency =
      dependency

    def withPlatform(platformSuffix: String): JavaDependency =
      this

    def addExclude(excl: JavaOrScalaModule*): JavaDependency =
      copy(exclude = exclude ++ excl)

    def withUnderlyingDependency(f: Dependency => Dependency): JavaDependency =
      copy(dependency = f(dependency))
  }

  case class ScalaDependency(
      baseDependency: Dependency,
      fullCrossVersion: Boolean,
      withPlatformSuffix: Boolean,
      exclude: Set[JavaOrScalaModule]
  ) extends JavaOrScalaDependency {
    def module: JavaOrScalaModule.ScalaModule =
      // FIXME withPlatformSuffix not supported in JavaOrScalaModule.ScalaModule
      JavaOrScalaModule.ScalaModule(baseDependency.module, fullCrossVersion)
    def repr: String =
      s"$module:${if (withPlatformSuffix) ":" else ""}${baseDependency.version}"
    def version: String =
      baseDependency.version
    def dependency(scalaVersion: Versions.Scala, platformName: String): Dependency = {
      val platformSuffix =
        if (withPlatformSuffix && platformName.nonEmpty) "_" + platformName
        else ""
      val scalaSuffix =
        if (fullCrossVersion) "_" + scalaVersion.scalaVersion
        else "_" + scalaVersion.binVersion

      val newName = baseDependency.module.name.value + platformSuffix + scalaSuffix

      baseDependency.withModule(baseDependency.module.withName(ModuleName(newName)))
    }

    def withPlatform(platformSuffix: String): ScalaDependency =
      if (withPlatformSuffix)
        withUnderlyingDependency { dep =>
          dep.withModule(
            dep.module.withName(
              ModuleName(dep.module.name.value + platformSuffix)
            )
          )
        }
      else
        this

    def addExclude(excl: JavaOrScalaModule*): ScalaDependency =
      copy(exclude = exclude ++ excl)

    def withUnderlyingDependency(f: Dependency => Dependency): ScalaDependency =
      copy(baseDependency = f(baseDependency))
  }

  implicit val decodes: Decoder[JavaOrScalaDependency] =
    Decoder.instance { c =>
      def parseDep(s: String): Decoder.Result[JavaOrScalaDependency] =
        internal.DependencyParser.javaOrScalaDependencyParams(s, Configuration.empty).map(_._1).left.map(err => DecodingFailure(err, c.history))
      def parseModule(modules: List[String]): Decoder.Result[Seq[JavaOrScalaModule]] =
        modules.traverse(internal.ModuleParser.javaOrScalaModule).left.map(err => DecodingFailure(err, c.history))

      val fromString: Decoder.Result[JavaOrScalaDependency] =
        c.as[String].flatMap(parseDep)

      val full: Decoder.Result[JavaOrScalaDependency] =
        for {
          dependency <- c.get[String]("module").flatMap(parseDep)
          exclude <- c.get[Option[List[String]]]("excludes").map(_.getOrElse(Nil)).flatMap(parseModule)
          classifier <- c.get[Option[String]]("classifier")
          optional <- c.get[Option[Boolean]]("optional").map(_.getOrElse(false))
        } yield dependency
          .addExclude(exclude: _*)
          .withUnderlyingDependency(d =>
            classifier.map(cls => d.withAttributes(d.attributes.withClassifier(Classifier(cls)))).getOrElse(d).withOptional(optional)
          )

      fromString.orElse(full)
    }

  // warning: this assumes a scala version in a context where we don't know.
  // current usage looks safe, but perhaps some refactoring can change this
  private def unsafeAsDependency(dep: JavaOrScalaDependency) =
    dep match {
      case d: JavaOrScalaDependency.JavaDependency  => d.dependency
      case d: JavaOrScalaDependency.ScalaDependency => d.dependency(Versions.Scala213, "flaff")
    }

  implicit val encodes: Encoder[JavaOrScalaDependency] =
    Encoder.instance { d =>
      val dep = unsafeAsDependency(d)

      val needsFull = d.exclude.nonEmpty || dep.optional || dep.attributes.classifier.nonEmpty

      val jsonString = d match {
        case dep: JavaOrScalaDependency.JavaDependency =>
          Json.fromString(s"${dep.module}:${dep.dependency.version}")
        case dep: JavaOrScalaDependency.ScalaDependency =>
          Json.fromString(dep.withUnderlyingDependency(_.withOptional(false).withAttributes(Attributes.empty)).repr)
      }
      if (needsFull) {
        Json
          .obj(
            "module" := jsonString,
            "excludes" := Option.when(d.exclude.nonEmpty)(d.exclude.map(_.toString)),
            "optional" := dep.optional,
            "classifier" := Option.when(dep.attributes.classifier.nonEmpty)(dep.attributes.classifier.value)
          )
          .dropNullValues
      } else jsonString
    }

  implicit val orderingDep: Ordering[JavaOrScalaDependency] =
    Ordering.by { incompleteDep =>
      val dep = unsafeAsDependency(incompleteDep)
      (dep.module.repr, dep.version, dep.configuration)
    }
}
