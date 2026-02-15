package bleep.bsp

import com.github.plokhotnyuk.jsoniter_scala.core.*

/** Kotlin-specific data for BuildTarget.data when dataKind = "kotlin".
  *
  * This extends the BSP protocol with Kotlin-specific information, similar to how ScalaBuildTarget provides Scala-specific data.
  */
final case class KotlinBuildTarget(
    /** Kotlin version (e.g., "2.0.0", "2.1.0") */
    kotlinVersion: String,

    /** Target JVM version (e.g., "1.8", "11", "17") */
    jvmTarget: String,

    /** Additional Kotlin compiler options */
    kotlincOptions: List[String],

    /** Whether this target uses the K2 compiler (Kotlin 2.x) */
    isK2: Boolean
)

object KotlinBuildTarget {
  given codec: JsonValueCodec[KotlinBuildTarget] = new JsonValueCodec[KotlinBuildTarget] {
    def nullValue: KotlinBuildTarget = null
    def encodeValue(x: KotlinBuildTarget, out: JsonWriter): Unit = {
      out.writeObjectStart()
      out.writeKey("kotlinVersion")
      out.writeVal(x.kotlinVersion)
      out.writeKey("jvmTarget")
      out.writeVal(x.jvmTarget)
      out.writeKey("kotlincOptions")
      out.writeArrayStart()
      x.kotlincOptions.foreach(out.writeVal)
      out.writeArrayEnd()
      out.writeKey("isK2")
      out.writeVal(x.isK2)
      out.writeObjectEnd()
    }
    def decodeValue(in: JsonReader, default: KotlinBuildTarget): KotlinBuildTarget = {
      var kotlinVersion: String = null
      var jvmTarget: String = null
      var kotlincOptions: List[String] = Nil
      var isK2: Boolean = false
      if in.isNextToken('{') then {
        if !in.isNextToken('}') then {
          in.rollbackToken()
          var continue = true
          while continue do {
            val key = in.readKeyAsString()
            key match {
              case "kotlinVersion" => kotlinVersion = in.readString(null)
              case "jvmTarget"     => jvmTarget = in.readString(null)
              case "kotlincOptions" =>
                val builder = List.newBuilder[String]
                if in.isNextToken('[') then
                  if !in.isNextToken(']') then {
                    in.rollbackToken()
                    var arrContinue = true
                    while arrContinue do {
                      builder += in.readString(null)
                      arrContinue = in.isNextToken(',')
                    }
                    if !in.isCurrentToken(']') then in.arrayEndOrCommaError()
                  }
                kotlincOptions = builder.result()
              case "isK2" => isK2 = in.readBoolean()
              case _      => in.skip()
            }
            continue = in.isNextToken(',')
          }
          if !in.isCurrentToken('}') then in.objectEndOrCommaError()
        }
      } else in.readNullOrTokenError(default, '{')
      KotlinBuildTarget(kotlinVersion, jvmTarget, kotlincOptions, isK2)
    }
  }
}

/** Constants for Kotlin BSP support */
object KotlinBuildTargetDataKind {

  /** The dataKind value for Kotlin build targets */
  val Kotlin = "kotlin"
}

/** The language ID for Kotlin in BSP */
object KotlinLanguageId {
  val Kotlin = "kotlin"
}
