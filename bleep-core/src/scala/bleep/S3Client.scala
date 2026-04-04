package bleep

import ryddig.Logger

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneOffset}
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

/** Minimal S3 REST client using AWS Signature V4 and Java HttpClient. Zero external dependencies.
  *
  * Supports any S3-compatible service (AWS S3, MinIO, Cloudflare R2, etc.).
  */
class S3Client(
    logger: Logger,
    bucket: String,
    region: String,
    endpoint: URI,
    accessKeyId: String,
    secretAccessKey: String
) {
  private val httpClient: HttpClient = HttpClient.newBuilder().build()
  private val service = "s3"

  /** Check if an object exists. */
  def headObject(key: String): Boolean = {
    val uri = objectUri(key)
    val request = signedRequest("HEAD", uri, Array.emptyByteArray)
    val response = httpClient.send(request, HttpResponse.BodyHandlers.discarding())
    response.statusCode() == 200
  }

  /** Download an object. Throws on non-200. */
  def getObject(key: String): Array[Byte] = {
    val uri = objectUri(key)
    val request = signedRequest("GET", uri, Array.emptyByteArray)
    val response = httpClient.send(request, HttpResponse.BodyHandlers.ofByteArray())
    response.statusCode() match {
      case 200  => response.body()
      case code => throw new BleepException.Text(s"S3 GET $key failed: HTTP $code")
    }
  }

  /** Upload an object. Throws on non-2xx. */
  def putObject(key: String, content: Array[Byte]): Unit = {
    val uri = objectUri(key)
    val request = signedRequest("PUT", uri, content)
    val response = httpClient.send(request, HttpResponse.BodyHandlers.ofString())
    response.statusCode() match {
      case code if code >= 200 && code < 300 =>
        logger.debug(s"S3 PUT $key: $code (${content.length} bytes)")
      case code =>
        throw new BleepException.Text(s"S3 PUT $key failed: HTTP $code\n${response.body()}")
    }
  }

  private def objectUri(key: String): URI =
    endpoint.resolve(s"/$bucket/$key")

  // ============================================================================
  // AWS Signature V4
  // ============================================================================

  private def signedRequest(method: String, uri: URI, payload: Array[Byte]): HttpRequest = {
    val now = Instant.now()
    val dateStamp = DateTimeFormatter.ofPattern("yyyyMMdd").withZone(ZoneOffset.UTC).format(now)
    val amzDate = DateTimeFormatter.ofPattern("yyyyMMdd'T'HHmmss'Z'").withZone(ZoneOffset.UTC).format(now)
    val host = uri.getHost + (if (uri.getPort > 0) s":${uri.getPort}" else "")
    val path = uri.getRawPath

    val payloadHash = sha256Hex(payload)
    val headers = Map(
      "host" -> host,
      "x-amz-content-sha256" -> payloadHash,
      "x-amz-date" -> amzDate
    )

    val signedHeaderKeys = headers.keys.toList.sorted.mkString(";")
    val canonicalHeaders = headers.toList.sortBy(_._1).map { case (k, v) => s"$k:$v\n" }.mkString

    val canonicalRequest = List(
      method,
      path,
      "", // query string (empty)
      canonicalHeaders,
      signedHeaderKeys,
      payloadHash
    ).mkString("\n")

    val credentialScope = s"$dateStamp/$region/$service/aws4_request"
    val stringToSign = List(
      "AWS4-HMAC-SHA256",
      amzDate,
      credentialScope,
      sha256Hex(canonicalRequest.getBytes(StandardCharsets.UTF_8))
    ).mkString("\n")

    val signingKey = {
      val kDate = hmacSha256(s"AWS4$secretAccessKey".getBytes(StandardCharsets.UTF_8), dateStamp)
      val kRegion = hmacSha256(kDate, region)
      val kService = hmacSha256(kRegion, service)
      hmacSha256(kService, "aws4_request")
    }

    val signature = Checksums.byteArrayToHexString(hmacSha256(signingKey, stringToSign))
    val authorization = s"AWS4-HMAC-SHA256 Credential=$accessKeyId/$credentialScope, SignedHeaders=$signedHeaderKeys, Signature=$signature"

    val builder = HttpRequest
      .newBuilder(uri)
      .method(method, if (payload.isEmpty && method != "PUT") HttpRequest.BodyPublishers.noBody() else HttpRequest.BodyPublishers.ofByteArray(payload))
      .header("Authorization", authorization)
      .header("x-amz-content-sha256", payloadHash)
      .header("x-amz-date", amzDate)

    builder.build()
  }

  private def sha256Hex(data: Array[Byte]): String = {
    val md = MessageDigest.getInstance("SHA-256")
    Checksums.byteArrayToHexString(md.digest(data))
  }

  private def hmacSha256(key: Array[Byte], data: String): Array[Byte] = {
    val mac = Mac.getInstance("HmacSHA256")
    mac.init(new SecretKeySpec(key, "HmacSHA256"))
    mac.doFinal(data.getBytes(StandardCharsets.UTF_8))
  }
}

object S3Client {

  /** Create an S3Client from a remote cache config and credentials.
    *
    * Parses `s3://bucket/prefix` URIs. For non-S3 URIs, uses the URI directly as the endpoint.
    */
  def fromConfig(
      logger: Logger,
      cacheConfig: model.RemoteCacheConfig,
      credentials: model.RemoteCacheCredentials
  ): S3Client = {
    val uri = cacheConfig.uri
    val region = cacheConfig.region.getOrElse("us-east-1")

    if (uri.getScheme == "s3") {
      val bucket = uri.getHost
      val endpoint = URI.create(s"https://s3.$region.amazonaws.com")
      new S3Client(logger, bucket, region, endpoint, credentials.accessKeyId, credentials.secretAccessKey)
    } else {
      // Non-S3 URI (e.g. MinIO, R2) — use URI as endpoint, extract bucket from first path segment
      val path = uri.getPath.stripPrefix("/")
      val slashIdx = path.indexOf('/')
      val bucket = if (slashIdx > 0) path.substring(0, slashIdx) else path
      new S3Client(
        logger,
        bucket,
        region,
        URI.create(s"${uri.getScheme}://${uri.getHost}:${uri.getPort}"),
        credentials.accessKeyId,
        credentials.secretAccessKey
      )
    }
  }

  /** Resolve the key prefix from the config URI (the path part after the bucket). */
  def keyPrefix(cacheConfig: model.RemoteCacheConfig): String = {
    val uri = cacheConfig.uri
    val path =
      if (uri.getScheme == "s3") uri.getPath
      else {
        val fullPath = uri.getPath.stripPrefix("/")
        val slashIdx = fullPath.indexOf('/')
        if (slashIdx > 0) fullPath.substring(slashIdx) else ""
      }
    path.stripPrefix("/").stripSuffix("/")
  }
}
