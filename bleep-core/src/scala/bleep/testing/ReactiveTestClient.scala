package bleep.testing

import bleep.model.CrossProjectName
import ch.epfl.scala.bsp4j

import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters._

/** A BSP BuildClient that tracks compilation and notifies when test projects are ready.
  *
  * This wraps another BuildClient (for display) and adds reactive test scheduling.
  */
/** Compile failure information */
case class CompileFailure(
    project: String,
    errors: List[String]
)

class ReactiveTestClient(
    delegate: bsp4j.BuildClient,
    testTargets: Set[bsp4j.BuildTargetIdentifier],
    targetToProject: bsp4j.BuildTargetIdentifier => Option[CrossProjectName],
    onTestReady: (CrossProjectName, bsp4j.BuildTargetIdentifier) => Unit,
    onCompileStart: (String, bsp4j.BuildTargetIdentifier) => Unit,
    onCompileProgress: (String, Int) => Unit,
    onCompileFinish: (String, bsp4j.BuildTargetIdentifier, Boolean, List[String]) => Unit
) extends bsp4j.BuildClient {

  // Track compile start times for duration calculation
  private val compileStartTimes = new ConcurrentHashMap[String, Long]()

  // Track which targets are currently compiling (by URI)
  private val compiling = ConcurrentHashMap.newKeySet[String]()

  // Track which test targets have completed successfully (by URI)
  private val completedTests = ConcurrentHashMap.newKeySet[String]()

  // Track diagnostics (errors) per project
  private val projectDiagnostics = new ConcurrentHashMap[String, java.util.List[String]]()

  // Test target project names for fast lookup (extract from URI after ?id=)
  private val testProjectNames: Set[String] = testTargets.map(t => extractProjectName(t))

  override def onBuildShowMessage(params: bsp4j.ShowMessageParams): Unit =
    delegate.onBuildShowMessage(params)

  override def onBuildLogMessage(params: bsp4j.LogMessageParams): Unit =
    delegate.onBuildLogMessage(params)

  override def onBuildTaskStart(params: bsp4j.TaskStartParams): Unit = {
    extractTarget(params.getData).foreach { target =>
      val projectName = extractProjectName(target)
      compiling.add(projectName)
      compileStartTimes.put(projectName, System.currentTimeMillis())
      // Clear previous diagnostics when compilation starts
      projectDiagnostics.remove(projectName)
      onCompileStart(projectName, target)
    }
    delegate.onBuildTaskStart(params)
  }

  override def onBuildTaskProgress(params: bsp4j.TaskProgressParams): Unit = {
    // Extract progress percentage if available
    val total = params.getTotal
    val progress = params.getProgress
    if (total > 0 && progress >= 0) {
      val percent = ((progress * 100) / total).toInt
      extractTarget(params.getData).foreach { target =>
        val projectName = extractProjectName(target)
        onCompileProgress(projectName, percent)
      }
    }
    delegate.onBuildTaskProgress(params)
  }

  override def onBuildTaskFinish(params: bsp4j.TaskFinishParams): Unit = {
    val extracted = extractTarget(params.getData)
    extracted match {
      case Some(target) =>
        val projectName = extractProjectName(target)
        compiling.remove(projectName)
        compileStartTimes.remove(projectName)
        val success = params.getStatus == bsp4j.StatusCode.OK

        // Get accumulated errors for this project
        val errors: List[String] = Option(projectDiagnostics.get(projectName))
          .map(_.asScala.toList)
          .getOrElse(Nil)

        onCompileFinish(projectName, target, success, errors)

        // If this is a test project and it compiled successfully, notify
        val isTestProject = testProjectNames.contains(projectName)
        if (isTestProject && success) {
          if (completedTests.add(projectName)) {
            targetToProject(target).foreach { project =>
              onTestReady(project, target)
            }
          }
        }
      case None =>
        ()
    }
    delegate.onBuildTaskFinish(params)
  }

  override def onBuildPublishDiagnostics(params: bsp4j.PublishDiagnosticsParams): Unit = {
    // Capture error diagnostics per project
    val target = params.getBuildTarget
    val projectName = extractProjectName(target)

    params.getDiagnostics.asScala.foreach { diagnostic =>
      if (diagnostic.getSeverity == bsp4j.DiagnosticSeverity.ERROR) {
        val errorList = projectDiagnostics.computeIfAbsent(
          projectName,
          _ => new java.util.ArrayList[String]()
        )
        // Format: file:///path/to/file.scala:line:column: message (clickable in terminals)
        val pos = diagnostic.getRange.getStart
        val uri = Option(params.getTextDocument.getUri).getOrElse("unknown")
        val line = pos.getLine + 1
        val col = pos.getCharacter + 1
        val msg = s"$uri:$line:$col: ${diagnostic.getMessage}"
        errorList.add(msg)
        ()
      }
    }
    delegate.onBuildPublishDiagnostics(params)
  }

  override def onBuildTargetDidChange(params: bsp4j.DidChangeBuildTarget): Unit =
    delegate.onBuildTargetDidChange(params)

  private def extractTarget(data: AnyRef): Option[bsp4j.BuildTargetIdentifier] =
    data match {
      case obj: com.google.gson.JsonObject =>
        obj.get("target") match {
          case target: com.google.gson.JsonObject =>
            target.get("uri") match {
              case str: com.google.gson.JsonPrimitive =>
                Some(new bsp4j.BuildTargetIdentifier(str.getAsString))
              case _ => None
            }
          case _ => None
        }
      case _ => None
    }

  private def extractProjectName(target: bsp4j.BuildTargetIdentifier): String = {
    // Extract project name from target URI like "file://.../bleep-core?id=bleep-core"
    val uri = target.getUri
    val queryIdx = uri.indexOf("?id=")
    if (queryIdx >= 0) {
      uri.substring(queryIdx + 4)
    } else {
      // Fallback: use last path segment
      val lastSlash = uri.lastIndexOf('/')
      if (lastSlash >= 0) uri.substring(lastSlash + 1) else uri
    }
  }

  /** Check if all test projects have completed */
  def allTestsCompiled: Boolean =
    testProjectNames.forall(completedTests.contains)

  /** Get the set of completed test project names */
  def getCompletedTestNames: Set[String] =
    completedTests.asScala.toSet
}
