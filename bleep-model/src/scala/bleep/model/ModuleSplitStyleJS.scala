package bleep.model

import io.circe.*
import io.circe.syntax.*

sealed abstract class ModuleSplitStyleJS(val id: String)
object ModuleSplitStyleJS {
  case object FewestModules extends ModuleSplitStyleJS("FewestModules")
  case object SmallestModules extends ModuleSplitStyleJS("SmallestModules")
  final case class SmallModulesFor(packages: List[String]) extends ModuleSplitStyleJS("SmallModulesFor")

  object SmallModulesFor {
    val id: String = SmallModulesFor(List.empty).id
  }

  val All: List[String] =
    List(FewestModules.id, SmallestModules.id, SmallModulesFor.id)

  implicit val encodeModuleSplitStyleJS: Encoder[ModuleSplitStyleJS] = Encoder.instance {
    case ModuleSplitStyleJS.FewestModules   => Json.obj(("splitStyle", Json.fromString("FewestModules")))
    case ModuleSplitStyleJS.SmallestModules => Json.obj(("splitStyle", Json.fromString("SmallestModules")))
    case m: ModuleSplitStyleJS.SmallModulesFor =>
      Json.obj(("splitStyle", Json.fromString("SmallModulesFor")), ("packages", m.packages.asJson))
  }

  implicit val decodeModuleSplitStyleJS: Decoder[ModuleSplitStyleJS] = Decoder.instance { h =>
    h.get[String]("splitStyle").flatMap {
      case "FewestModules"   => Right(ModuleSplitStyleJS.FewestModules)
      case "SmallestModules" => Right(ModuleSplitStyleJS.SmallestModules)
      case "SmallModulesFor" => h.get[List[String]]("packages").map(packages => ModuleSplitStyleJS.SmallModulesFor(packages))
      case _                 => Left(DecodingFailure("ModuleSplitStyleJS", h.history))
    }
  }
}
