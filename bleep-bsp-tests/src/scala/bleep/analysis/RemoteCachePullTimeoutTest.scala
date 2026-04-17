package bleep.analysis

import bleep.S3Client
import bleep.model.BspServerConfig
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Configuration tests for the remote-cache pull timeout:
  *   - Default is 30s.
  *   - `effectiveRemoteCacheRequestTimeoutSeconds` returns configured value when set, default otherwise.
  *   - Connect timeout is 10s, fixed.
  */
class RemoteCachePullTimeoutTest extends AnyFunSuite with Matchers {

  test("default request timeout is 30 seconds") {
    BspServerConfig.DefaultRemoteCacheRequestTimeoutSeconds shouldBe 30
  }

  test("BspServerConfig.default exposes no override, returns default via effective*") {
    val cfg = BspServerConfig.default
    cfg.remoteCacheRequestTimeoutSeconds shouldBe None
    cfg.effectiveRemoteCacheRequestTimeoutSeconds shouldBe 30
  }

  test("configured timeout propagates via effectiveRemoteCacheRequestTimeoutSeconds") {
    val cfg = BspServerConfig.default.copy(remoteCacheRequestTimeoutSeconds = Some(120))
    cfg.effectiveRemoteCacheRequestTimeoutSeconds shouldBe 120
  }

  test("connect timeout is 10 seconds (fixed)") {
    S3Client.ConnectTimeoutSeconds shouldBe 10
  }
}
