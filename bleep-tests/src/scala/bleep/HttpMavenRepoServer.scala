package bleep

import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}

import java.net.{InetSocketAddress, URI}
import java.nio.file.{Files, Path}
import java.util.Base64
import java.util.concurrent.atomic.AtomicReference
import scala.collection.mutable

/** A throwaway in-process HTTP Maven repository — receives PUT uploads, serves GET fetches, requires HTTP Basic Auth on every request.
  *
  * Faithful enough to exercise the full publish stack (URI-keyed exact-match auth lookup, named-resolver → publish-subcommand wiring, PUT upload of every file
  * in the Maven layout). Skips HTTPS/SSL — the wire format above the transport is identical and SSL plumbing is a separate failure mode.
  */
class HttpMavenRepoServer(user: String, password: String) {
  private val repoDir: Path = Files.createTempDirectory("bleep-http-maven-")
  private val expectedAuth: String =
    "Basic " + Base64.getEncoder.encodeToString(s"$user:$password".getBytes("UTF-8"))
  private val unauthorizedAttempts: AtomicReference[List[String]] = new AtomicReference(Nil)

  private val server: HttpServer = {
    val s = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)
    s.createContext("/", new RepoHandler)
    s.setExecutor(null) // synchronous: the test waits on individual requests
    s.start()
    s
  }

  /** Repository base URI — already started, ready to receive PUT/GET. */
  val repoUri: URI = URI.create(s"http://127.0.0.1:${server.getAddress.getPort}/repo")

  def stop(): Unit = {
    server.stop(0)
    bleep.internal.FileUtils.deleteDirectory(repoDir)
  }

  /** Path of an uploaded artifact on the server's disk. */
  def storedFile(relPath: String): Path = repoDir.resolve(relPath)

  /** Every URI that received a request without a valid Authorization header. Useful to assert that a 401 happened (e.g. when verifying creds are required). */
  def unauthorizedRequests: List[String] = unauthorizedAttempts.get()

  private final class RepoHandler extends HttpHandler {
    override def handle(exchange: HttpExchange): Unit =
      try {
        val auth = Option(exchange.getRequestHeaders.getFirst("Authorization")).getOrElse("")
        if (auth != expectedAuth) {
          val rec = s"${exchange.getRequestMethod} ${exchange.getRequestURI}"
          unauthorizedAttempts.updateAndGet(rec :: _)
          exchange.getResponseHeaders.add("WWW-Authenticate", "Basic realm=\"bleep-test\"")
          exchange.sendResponseHeaders(401, -1)
          return
        }

        val rel = exchange.getRequestURI.getPath.stripPrefix("/repo/").stripPrefix("/")

        exchange.getRequestMethod match {
          case "PUT" =>
            val body = exchange.getRequestBody.readAllBytes()
            val target = repoDir.resolve(rel)
            Option(target.getParent).foreach(Files.createDirectories(_))
            Files.write(target, body)
            exchange.sendResponseHeaders(201, -1)
          case "GET" | "HEAD" =>
            val target = repoDir.resolve(rel)
            if (Files.isRegularFile(target)) {
              val bytes = Files.readAllBytes(target)
              exchange.sendResponseHeaders(200, if (exchange.getRequestMethod == "HEAD") -1 else bytes.length.toLong)
              if (exchange.getRequestMethod == "GET") {
                val os = exchange.getResponseBody
                try os.write(bytes)
                finally os.close()
              }
            } else exchange.sendResponseHeaders(404, -1)
          case _ =>
            exchange.sendResponseHeaders(405, -1)
        }
      } finally exchange.close()
  }
}

object HttpMavenRepoServer {

  /** Run `body` against a freshly-started server. Always stops the server, even on exception. */
  def withServer[T](user: String, password: String)(body: HttpMavenRepoServer => T): T = {
    val srv = new HttpMavenRepoServer(user, password)
    try body(srv)
    finally srv.stop()
  }

  /** Inventory of every file the server received, sorted. */
  def storedFiles(server: HttpMavenRepoServer): List[String] = {
    val root = server.storedFile("")
    if (!Files.isDirectory(root)) Nil
    else {
      val acc = mutable.ListBuffer.empty[String]
      Files.walk(root).forEach { p =>
        if (Files.isRegularFile(p)) acc += root.relativize(p).toString.replace('\\', '/')
      }
      acc.sorted.toList
    }
  }
}
