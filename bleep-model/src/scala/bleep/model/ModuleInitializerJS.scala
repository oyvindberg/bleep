package bleep.model

import io.circe.*
import io.circe.generic.semiauto.deriveCodec

/** A static method the linked Scala.js program calls on start, after bleep's own initializer (the main class, or the test bridge). Scala.js's
  * `ModuleInitializer`.
  *
  * @param className
  *   the class (for a Scala `object`, its name without the trailing `$`)
  * @param method
  *   a static method of it. With `args` absent it takes no parameters (`def m(): Unit`, Scala.js's `ModuleInitializer.mainMethod`); with `args` present, even
  *   empty, it takes them as `Array[String]` (`def m(args: Array[String]): Unit`, `ModuleInitializer.mainMethodWithArgs`). Both forms exist, so absent and
  *   empty are not the same thing.
  */
case class ModuleInitializerJS(className: String, method: String, args: Option[List[String]])

object ModuleInitializerJS {
  implicit val codec: Codec.AsObject[ModuleInitializerJS] = deriveCodec
}
