package bleep

import java.nio.file.{Files, Path}
import javax.xml.parsers.DocumentBuilderFactory
import org.w3c.dom.{Element, Node}
import scala.jdk.CollectionConverters.*

/** Reads back the JUnit XML that a `bleep test` run wrote, so a test can assert on what actually executed.
  *
  * Every other assertion channel available to an IT is too weak for platform test runners. `commands.test` throwing is a two-state signal, and the failure mode
  * these tests exist to catch — a suite that is discovered, reports success, and runs nothing — does not throw. Log scraping would work but couples the
  * assertion to the summary's wording. The XML carries per-case names and statuses and is written by the same collector for JVM, Scala.js and Scala Native, so
  * one reader serves every platform and a passing count means the same thing on all of them.
  */
object JUnitReports {

  /** `status` is the JUnit XML shape: a `<testcase>` with no child element passed; otherwise the child's tag name (`failure`, `error`, `skipped`). */
  /** `detail` is the `message` attribute and the element's own text together.
    *
    * Both matter and frameworks disagree about which to use: munit puts a full diff in the attribute, while ScalaTest and hedgehog leave it empty and write the
    * failure into the body. Reading only one of the two makes half the frameworks look like they report failures with nothing to say.
    */
  case class Case(name: String, className: String, status: String, message: Option[String], detail: String)

  /** `systemOut` and `systemErr` are the `<system-out>` / `<system-err>` sections: everything the test program printed.
    *
    * Parsed because two platforms used to drop it entirely and nothing noticed — Scala.js emitted no `<system-out>` element at all, and Scala Native's binary
    * wrote to a detached daemon's file descriptors. Both looked exactly like a test that printed nothing.
    */
  case class Suite(
      name: String,
      tests: Int,
      failures: Int,
      errors: Int,
      skipped: Int,
      cases: List[Case],
      systemOut: String,
      systemErr: String
  ) {
    def passed: Int = cases.count(_.status == "passed")
    def describe: String =
      s"$name: tests=$tests failures=$failures errors=$errors skipped=$skipped cases=[${cases.map(c => s"${c.name}:${c.status}").mkString(", ")}]"
  }

  /** Read every `TEST-*.xml` under `dir`. Returns empty when the directory does not exist — a run that produced no reports at all is a legitimate (and
    * assertable) outcome, not an error to raise here.
    */
  def read(dir: Path): List[Suite] =
    if (!Files.isDirectory(dir)) Nil
    else {
      val files = Files.list(dir).iterator().asScala.filter(p => p.getFileName.toString.endsWith(".xml")).toList.sortBy(_.getFileName.toString)
      files.flatMap(readFile)
    }

  private def readFile(file: Path): List[Suite] = {
    val factory = DocumentBuilderFactory.newInstance()
    factory.setNamespaceAware(false)
    val doc = factory.newDocumentBuilder().parse(file.toFile)
    elements(doc.getDocumentElement, "testsuite").map { ts =>
      Suite(
        name = ts.getAttribute("name"),
        tests = intAttr(ts, "tests"),
        failures = intAttr(ts, "failures"),
        errors = intAttr(ts, "errors"),
        skipped = intAttr(ts, "skipped"),
        systemOut = elements(ts, "system-out").map(_.getTextContent).mkString("\n"),
        systemErr = elements(ts, "system-err").map(_.getTextContent).mkString("\n"),
        cases = elements(ts, "testcase").map { tc =>
          val child = childElements(tc).headOption
          Case(
            name = tc.getAttribute("name"),
            className = tc.getAttribute("classname"),
            status = child.map(_.getTagName).getOrElse("passed"),
            message = child.map(_.getAttribute("message")).filter(_.nonEmpty),
            detail = child.map(c => (c.getAttribute("message") + "\n" + c.getTextContent).trim).getOrElse("")
          )
        }
      )
    }
  }

  private def intAttr(e: Element, name: String): Int = {
    val raw = e.getAttribute(name)
    if (raw.isEmpty) 0 else raw.toInt
  }

  private def elements(parent: Element, tag: String): List[Element] =
    childElements(parent).filter(_.getTagName == tag)

  private def childElements(parent: Element): List[Element] = {
    val nodes = parent.getChildNodes
    (0 until nodes.getLength).iterator
      .map(nodes.item)
      .collect { case e: Element if e.getNodeType == Node.ELEMENT_NODE => e }
      .toList
  }
}
