package bleep
package mavenimport

/** Reads what `mvn dependency:list` logs, per module:
  * {{{
  * [INFO] --- dependency:3.7.0:list (default-cli) @ feign-core ---
  * [INFO] The following files have been resolved:
  * [INFO]    org.jspecify:jspecify:jar:1.0.0:compile -- module org.jspecify
  * [INFO]    io.github.openfeign:feign-core:test-jar:tests:13.16-SNAPSHOT:test
  * [INFO]
  * }}}
  */
object parseDependencyList {
  private val Header = """.*--- dependency:[^:]+:list \(default-cli\) @ (\S+) ---.*""".r
  private val Entry = """\[INFO\] {4}(\S+).*""".r

  /** by artifactId */
  def apply(output: String): Map[String, List[MavenResolvedDependency]] = {
    val result = Map.newBuilder[String, List[MavenResolvedDependency]]
    var module: Option[String] = None
    var entries: Option[List[MavenResolvedDependency]] = None

    def finish(): Unit = {
      (module, entries) match {
        case (Some(m), Some(es)) => result += (m -> es.reverse)
        case _                   => ()
      }
      entries = None
    }

    output.linesIterator.foreach {
      case Header(artifactId) =>
        finish()
        module = Some(artifactId)
      case line if line.contains("The following files have been resolved:") =>
        entries = Some(Nil)
      case Entry("none") if entries.isDefined      => ()
      case Entry(coordinates) if entries.isDefined =>
        val dep = coordinates.split(':') match {
          case Array(g, a, t, v, s)    => MavenResolvedDependency(g, a, t, "", v, s)
          case Array(g, a, t, c, v, s) => MavenResolvedDependency(g, a, t, c, v, s)
          case _                       => throw new BleepException.Text(s"Could not understand `$coordinates` in the output of mvn dependency:list")
        }
        entries = entries.map(dep :: _)
      case _ if entries.isDefined => finish()
      case _                      => ()
    }
    finish()
    result.result()
  }
}
