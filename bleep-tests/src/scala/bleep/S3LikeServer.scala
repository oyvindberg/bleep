package bleep

import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}

import java.net.{InetSocketAddress, URI}
import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters.*

/** A throwaway in-process S3-compatible HTTP server — receives PUT uploads, serves GET/HEAD requests, stores everything in memory.
  *
  * Faithful enough to exercise the full remote-cache stack (URI parsing, key layout, AWS SigV4 header serialization, archive round-trip). Does NOT verify the
  * AWS signature on incoming requests — we're testing bleep's cache code, not the SigV4 implementation. The S3Client always emits a syntactically-valid
  * `Authorization` header; the server just ignores it.
  *
  * URI shape: `http://127.0.0.1:<port>/<bucket>/<key>` — same layout `S3Client.fromConfig` builds when the configured URI is a non-`s3:` scheme.
  */
final class S3LikeServer(bucket: String) {

  private val store = new ConcurrentHashMap[String, Array[Byte]]()

  private val server: HttpServer = {
    val s = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)
    s.createContext("/", new BucketHandler)
    s.setExecutor(null)
    s.start()
    s
  }

  /** Base URI to put in `remote-cache.uri` of a workspace bleep.yaml. The bucket is part of the path; whatever you append becomes the key prefix. */
  val baseUri: URI = URI.create(s"http://127.0.0.1:${server.getAddress.getPort}/$bucket")

  def stop(): Unit = server.stop(0)

  /** Snapshot of every key currently stored, sorted. Used by tests to assert what was uploaded. */
  def keys: List[String] = store.keys().asScala.toList.sorted

  /** Raw bytes stored at a key, or None. Use to inspect the archive a Push uploaded. */
  def get(key: String): Option[Array[Byte]] = Option(store.get(key))

  /** Object count — sanity assertion that Push wrote something. */
  def size: Int = store.size

  private final class BucketHandler extends HttpHandler {
    override def handle(exchange: HttpExchange): Unit =
      try {
        val rawPath = exchange.getRequestURI.getRawPath.stripPrefix("/")
        val key = rawPath.stripPrefix(s"$bucket/")
        if (rawPath == key) {
          // Path didn't begin with our bucket — refuse so misconfigurations surface immediately.
          exchange.sendResponseHeaders(404, -1)
          return
        }
        exchange.getRequestMethod match {
          case "PUT" =>
            val body = exchange.getRequestBody.readAllBytes()
            store.put(key, body)
            exchange.sendResponseHeaders(200, -1)
          case "HEAD" =>
            if (store.containsKey(key)) exchange.sendResponseHeaders(200, -1)
            else exchange.sendResponseHeaders(404, -1)
          case "GET" =>
            Option(store.get(key)) match {
              case Some(bytes) =>
                exchange.sendResponseHeaders(200, bytes.length.toLong)
                val os = exchange.getResponseBody
                try os.write(bytes)
                finally os.close()
              case None =>
                exchange.sendResponseHeaders(404, -1)
            }
          case _ =>
            exchange.sendResponseHeaders(405, -1)
        }
      } finally exchange.close()
  }
}

object S3LikeServer {

  /** Run `body` against a freshly-started server. Always stops it, even on exception. */
  def withServer[T](bucket: String)(body: S3LikeServer => T): T = {
    val srv = new S3LikeServer(bucket)
    try body(srv)
    finally srv.stop()
  }
}
