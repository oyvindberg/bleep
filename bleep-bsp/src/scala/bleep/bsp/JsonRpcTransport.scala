package bleep.bsp

import com.github.plokhotnyuk.jsoniter_scala.core.*
import ch.epfl.scala.bsp.RawJson

import java.io.{BufferedInputStream, BufferedOutputStream, InputStream, OutputStream}
import java.nio.charset.StandardCharsets

/** JSON-RPC 2.0 message types */
sealed trait JsonRpcMessage

final case class JsonRpcRequest(
    jsonrpc: String,
    id: Option[RpcId],
    method: String,
    params: Option[RawJson]
) extends JsonRpcMessage

final case class JsonRpcResponse(
    jsonrpc: String,
    id: RpcId,
    result: Option[RawJson],
    error: Option[JsonRpcError]
) extends JsonRpcMessage

final case class JsonRpcNotification(
    jsonrpc: String,
    method: String,
    params: Option[RawJson]
) extends JsonRpcMessage

final case class JsonRpcError(
    code: Int,
    message: String,
    data: Option[RawJson]
)

/** RPC ID can be either a number or a string */
enum RpcId {
  case IntId(value: Int)
  case StringId(value: String)

  /** Canonical string key for this ID — just the raw value, no wrapper name. */
  def key: String = this match {
    case IntId(v)    => v.toString
    case StringId(v) => v
  }
}

object RpcId {
  given codec: JsonValueCodec[RpcId] = new JsonValueCodec[RpcId] {
    def nullValue: RpcId = null
    def encodeValue(x: RpcId, out: JsonWriter): Unit = x match {
      case IntId(v)    => out.writeVal(v)
      case StringId(v) => out.writeVal(v)
    }
    def decodeValue(in: JsonReader, default: RpcId): RpcId =
      if in.isNextToken('"') then {
        in.rollbackToken()
        StringId(in.readString(null))
      } else {
        in.rollbackToken()
        IntId(in.readInt())
      }
  }
}

object JsonRpcCodecs {
  import RpcId.given

  given errorCodec: JsonValueCodec[JsonRpcError] = new JsonValueCodec[JsonRpcError] {
    def nullValue: JsonRpcError = null
    def encodeValue(x: JsonRpcError, out: JsonWriter): Unit = {
      out.writeObjectStart()
      out.writeKey("code")
      out.writeVal(x.code)
      out.writeKey("message")
      out.writeVal(x.message)
      x.data.foreach { d =>
        out.writeKey("data")
        out.writeRawVal(d.value)
      }
      out.writeObjectEnd()
    }
    def decodeValue(in: JsonReader, default: JsonRpcError): JsonRpcError = {
      var code = 0
      var message: String = null
      var data: Option[RawJson] = None
      if in.isNextToken('{') then {
        if !in.isNextToken('}') then {
          in.rollbackToken()
          var continue = true
          while continue do {
            val key = in.readKeyAsString()
            key match {
              case "code"    => code = in.readInt()
              case "message" => message = in.readString(null)
              case "data"    => data = Some(RawJson(in.readRawValAsBytes()))
              case _         => in.skip()
            }
            continue = in.isNextToken(',')
          }
          if !in.isCurrentToken('}') then in.objectEndOrCommaError()
        }
      } else { val _ = in.readNullOrTokenError(default, '{') }
      JsonRpcError(code, message, data)
    }
  }

  given requestCodec: JsonValueCodec[JsonRpcRequest] = new JsonValueCodec[JsonRpcRequest] {
    def nullValue: JsonRpcRequest = null
    def encodeValue(x: JsonRpcRequest, out: JsonWriter): Unit = {
      out.writeObjectStart()
      out.writeKey("jsonrpc")
      out.writeVal(x.jsonrpc)
      x.id.foreach { i =>
        out.writeKey("id")
        summon[JsonValueCodec[RpcId]].encodeValue(i, out)
      }
      out.writeKey("method")
      out.writeVal(x.method)
      x.params.foreach { p =>
        out.writeKey("params")
        out.writeRawVal(p.value)
      }
      out.writeObjectEnd()
    }
    def decodeValue(in: JsonReader, default: JsonRpcRequest): JsonRpcRequest = {
      var jsonrpc: String = null
      var id: Option[RpcId] = None
      var method: String = null
      var params: Option[RawJson] = None
      if in.isNextToken('{') then {
        if !in.isNextToken('}') then {
          in.rollbackToken()
          var continue = true
          while continue do {
            val key = in.readKeyAsString()
            key match {
              case "jsonrpc" => jsonrpc = in.readString(null)
              case "id"      => id = Some(summon[JsonValueCodec[RpcId]].decodeValue(in, null))
              case "method"  => method = in.readString(null)
              case "params"  => params = Some(RawJson(in.readRawValAsBytes()))
              case _         => in.skip()
            }
            continue = in.isNextToken(',')
          }
          if !in.isCurrentToken('}') then in.objectEndOrCommaError()
        }
      } else in.readNullOrTokenError(default, '{')
      JsonRpcRequest(jsonrpc, id, method, params)
    }
  }

  given responseCodec: JsonValueCodec[JsonRpcResponse] = new JsonValueCodec[JsonRpcResponse] {
    def nullValue: JsonRpcResponse = null
    def encodeValue(x: JsonRpcResponse, out: JsonWriter): Unit = {
      out.writeObjectStart()
      out.writeKey("jsonrpc")
      out.writeVal(x.jsonrpc)
      out.writeKey("id")
      summon[JsonValueCodec[RpcId]].encodeValue(x.id, out)
      x.result.foreach { r =>
        out.writeKey("result")
        out.writeRawVal(r.value)
      }
      x.error.foreach { e =>
        out.writeKey("error")
        summon[JsonValueCodec[JsonRpcError]].encodeValue(e, out)
      }
      out.writeObjectEnd()
    }
    def decodeValue(in: JsonReader, default: JsonRpcResponse): JsonRpcResponse = {
      var jsonrpc: String = null
      var id: RpcId = null
      var result: Option[RawJson] = None
      var error: Option[JsonRpcError] = None
      if in.isNextToken('{') then {
        if !in.isNextToken('}') then {
          in.rollbackToken()
          var continue = true
          while continue do {
            val key = in.readKeyAsString()
            key match {
              case "jsonrpc" => jsonrpc = in.readString(null)
              case "id"      => id = summon[JsonValueCodec[RpcId]].decodeValue(in, null)
              case "result"  => result = Some(RawJson(in.readRawValAsBytes()))
              case "error"   => error = Some(summon[JsonValueCodec[JsonRpcError]].decodeValue(in, null))
              case _         => in.skip()
            }
            continue = in.isNextToken(',')
          }
          if !in.isCurrentToken('}') then in.objectEndOrCommaError()
        }
      } else in.readNullOrTokenError(default, '{')
      JsonRpcResponse(jsonrpc, id, result, error)
    }
  }

  given notificationCodec: JsonValueCodec[JsonRpcNotification] = new JsonValueCodec[JsonRpcNotification] {
    def nullValue: JsonRpcNotification = null
    def encodeValue(x: JsonRpcNotification, out: JsonWriter): Unit = {
      out.writeObjectStart()
      out.writeKey("jsonrpc")
      out.writeVal(x.jsonrpc)
      out.writeKey("method")
      out.writeVal(x.method)
      x.params.foreach { p =>
        out.writeKey("params")
        out.writeRawVal(p.value)
      }
      out.writeObjectEnd()
    }
    def decodeValue(in: JsonReader, default: JsonRpcNotification): JsonRpcNotification = {
      var jsonrpc: String = null
      var method: String = null
      var params: Option[RawJson] = None
      if in.isNextToken('{') then {
        if !in.isNextToken('}') then {
          in.rollbackToken()
          var continue = true
          while continue do {
            val key = in.readKeyAsString()
            key match {
              case "jsonrpc" => jsonrpc = in.readString(null)
              case "method"  => method = in.readString(null)
              case "params"  => params = Some(RawJson(in.readRawValAsBytes()))
              case _         => in.skip()
            }
            continue = in.isNextToken(',')
          }
          if !in.isCurrentToken('}') then in.objectEndOrCommaError()
        }
      } else in.readNullOrTokenError(default, '{')
      JsonRpcNotification(jsonrpc, method, params)
    }
  }
}

/** Result of reading a JSON-RPC message */
enum ReadResult {
  case Message(request: JsonRpcRequest)
  case EndOfStream
  case ParseError(error: String)
  case IoError(cause: Throwable)
  case InvalidMessage(reason: String)
}

/** JSON-RPC 2.0 transport over streams using LSP-style Content-Length framing.
  *
  * Message format:
  * ```
  * Content-Length: <length>\r\n
  * Content-Type: application/vscode-jsonrpc; charset=utf-8\r\n
  * \r\n
  * <json body>
  * ```
  *
  * Thread safety: All public methods are synchronized to ensure safe concurrent access.
  */
class JsonRpcTransport(
    in: InputStream,
    out: OutputStream
) {
  import JsonRpcCodecs.given

  private val bufferedIn = new BufferedInputStream(in)
  private val bufferedOut = new BufferedOutputStream(out)

  private val HeaderPattern = "Content-Length: (\\d+)".r.pattern

  /** Read lock for synchronizing reads (separate from write lock) */
  private val readLock = new AnyRef

  /** Read the next JSON-RPC message from the input stream.
    *
    * @return
    *   ReadResult indicating success, end of stream, or type of error
    */
  def readMessageTyped(): ReadResult = readLock.synchronized {
    try {
      val headerLine = readLine()
      if headerLine == null then return ReadResult.EndOfStream

      val matcher = HeaderPattern.matcher(headerLine)
      if !matcher.matches() then return ReadResult.ParseError(s"Invalid header: $headerLine")

      val contentLength =
        try matcher.group(1).toInt
        catch { case _: NumberFormatException => return ReadResult.ParseError(s"Invalid Content-Length: ${matcher.group(1)}") }

      // Reject oversized messages (256MB limit)
      if (contentLength > 256 * 1024 * 1024)
        return ReadResult.ParseError(s"Content-Length too large: $contentLength")
      if (contentLength < 0)
        return ReadResult.ParseError(s"Negative Content-Length: $contentLength")

      // Skip remaining headers until empty line
      var line = readLine()
      while line != null && line.nonEmpty do line = readLine()
      if line == null then return ReadResult.EndOfStream

      // Read content
      val content = new Array[Byte](contentLength)
      var read = 0
      while read < contentLength do {
        val r = bufferedIn.read(content, read, contentLength - read)
        if r < 0 then return ReadResult.IoError(new java.io.EOFException("Unexpected end of stream"))
        read += r
      }

      val request = readFromArray[JsonRpcRequest](content)

      // Validate required JSON-RPC fields
      if request.jsonrpc == null then return ReadResult.InvalidMessage("Missing required field: jsonrpc")
      if request.jsonrpc != "2.0" then return ReadResult.InvalidMessage(s"Invalid jsonrpc version: ${request.jsonrpc}")
      if request.method == null then return ReadResult.InvalidMessage("Missing required field: method")

      ReadResult.Message(request)
    } catch {
      case e: java.io.EOFException =>
        ReadResult.EndOfStream
      case e: com.github.plokhotnyuk.jsoniter_scala.core.JsonReaderException =>
        ReadResult.ParseError(e.getMessage)
      case e: java.io.IOException =>
        ReadResult.IoError(e)
      case e: Exception =>
        ReadResult.IoError(e)
    }
  }

  /** Read the next JSON-RPC message from the input stream. Returns None if the stream is closed.
    *
    * @deprecated
    *   Use readMessageTyped() for proper error handling
    */
  def readMessage(): Option[JsonRpcRequest] =
    readMessageTyped() match {
      case ReadResult.Message(request) => Some(request)
      case ReadResult.EndOfStream      => None
      case ReadResult.ParseError(error) =>
        System.err.println(s"[BSP] Parse error: $error")
        None
      case ReadResult.IoError(cause) =>
        System.err.println(s"[BSP] IO error: ${cause.getMessage}")
        cause.printStackTrace(System.err)
        None
      case ReadResult.InvalidMessage(reason) =>
        System.err.println(s"[BSP] Invalid message: $reason")
        None
    }

  /** Send a JSON-RPC response */
  def sendResponse(response: JsonRpcResponse): Unit =
    sendMessage(writeToArray(response))

  /** Send a JSON-RPC notification */
  def sendNotification(notification: JsonRpcNotification): Unit =
    sendMessage(writeToArray(notification))

  private def sendMessage(content: Array[Byte]): Unit = synchronized {
    val header = s"Content-Length: ${content.length}\r\nContent-Type: application/vscode-jsonrpc; charset=utf-8\r\n\r\n"
    bufferedOut.write(header.getBytes(StandardCharsets.US_ASCII))
    bufferedOut.write(content)
    bufferedOut.flush()
  }

  /** Maximum header line length (1MB). Protects against malicious clients sending endless bytes without CRLF. */
  private val MaxHeaderLineLength = 1024 * 1024

  private def readLine(): String = {
    val sb = new StringBuilder
    var prev = -1
    var curr = bufferedIn.read()
    while curr != -1 do {
      if prev == '\r' && curr == '\n' then return sb.dropRight(1).toString
      if sb.length >= MaxHeaderLineLength then throw new java.io.IOException(s"Header line exceeds maximum length ($MaxHeaderLineLength bytes)")
      sb.append(curr.toChar)
      prev = curr
      curr = bufferedIn.read()
    }
    if sb.isEmpty then null else sb.toString
  }

  def close(): Unit = {
    bufferedIn.close()
    bufferedOut.close()
  }
}

/** Standard JSON-RPC error codes */
object JsonRpcErrorCodes {
  val ParseError = -32700
  val InvalidRequest = -32600
  val MethodNotFound = -32601
  val InvalidParams = -32602
  val InternalError = -32603
  val ServerNotInitialized = -32002
  val UnknownErrorCode = -32001
  val RequestCancelled = -32800
}
