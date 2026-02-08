package ch.epfl.scala.bsp

import com.github.plokhotnyuk.jsoniter_scala.core.*

/** Raw JSON bytes, used for extensible data fields in BSP protocol. This is a replacement for jsonrpc4s.RawJson that works with Scala 3.
  */
final case class RawJson(value: Array[Byte]) {
  override def equals(other: Any): Boolean = other match {
    case that: RawJson => java.util.Arrays.equals(value, that.value)
    case _             => false
  }
  override def hashCode(): Int = java.util.Arrays.hashCode(value)
  override def toString: String = new String(value, "UTF-8")
}

object RawJson {
  def apply(s: String): RawJson = RawJson(s.getBytes("UTF-8"))

  implicit val codec: JsonValueCodec[RawJson] = new JsonValueCodec[RawJson] {
    def nullValue: RawJson = null
    def encodeValue(x: RawJson, out: JsonWriter): Unit = out.writeRawVal(x.value)
    def decodeValue(in: JsonReader, default: RawJson): RawJson = RawJson(in.readRawValAsBytes())
  }
}
