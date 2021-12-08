package bleep

import bleep.internal.stderr
import ch.epfl.scala.bsp4j

import java.util.concurrent.CompletableFuture
import scala.jdk.CollectionConverters._

package object bsp {

  implicit class Ext[T](private val f: CompletableFuture[T]) extends AnyVal {
    def logF: CompletableFuture[T] =
      f.handle { (res, _) =>
        stderr.log(res)
        res
      }
  }

  implicit class BuildTargetIdentifierExt(
      private val item: bsp4j.BuildTargetIdentifier
  ) extends AnyVal {
    def duplicate(): bsp4j.BuildTargetIdentifier =
      new bsp4j.BuildTargetIdentifier(item.getUri)
  }

  implicit class SourceItemExt(private val item: bsp4j.SourceItem) extends AnyVal {
    def duplicate(): bsp4j.SourceItem =
      new bsp4j.SourceItem(item.getUri, item.getKind, item.getGenerated)
  }

  implicit class SourcesItemExt(private val item: bsp4j.SourcesItem) extends AnyVal {
    def duplicate(): bsp4j.SourcesItem = {
      val other = new bsp4j.SourcesItem(
        item.getTarget,
        item.getSources.asScala.map(_.duplicate()).asJava
      )
      for (roots <- Option(item.getRoots))
        other.setRoots(roots.asScala.toList.asJava)
      other
    }
  }

  implicit class SourcesResultExt(private val res: bsp4j.SourcesResult) extends AnyVal {
    def duplicate(): bsp4j.SourcesResult =
      new bsp4j.SourcesResult(res.getItems().asScala.toList.map(_.duplicate()).asJava)
  }

  implicit class BuildTargetCapabilitiesExt(
      private val capabilities: bsp4j.BuildTargetCapabilities
  ) extends AnyVal {
    def duplicate(): bsp4j.BuildTargetCapabilities =
      new bsp4j.BuildTargetCapabilities(
        capabilities.getCanCompile,
        capabilities.getCanTest,
        capabilities.getCanRun
      )
  }
  implicit class BuildTargetExt(private val target: bsp4j.BuildTarget) extends AnyVal {
    def duplicate(): bsp4j.BuildTarget = {
      val target0 = new bsp4j.BuildTarget(
        target.getId.duplicate(),
        target.getTags.asScala.toList.asJava,
        target.getLanguageIds.asScala.toList.asJava,
        target.getDependencies.asScala.toList.map(_.duplicate()).asJava,
        target.getCapabilities.duplicate()
      )
      target0.setBaseDirectory(target.getBaseDirectory)
      target0.setData(target.getData) // FIXME Duplicate this when we can too?
      target0.setDataKind(target.getDataKind)
      target0.setDisplayName(target.getDisplayName)
      target0
    }
  }
  implicit class WorkspaceBuildTargetsResultExt(
      private val res: bsp4j.WorkspaceBuildTargetsResult
  ) extends AnyVal {
    def duplicate(): bsp4j.WorkspaceBuildTargetsResult =
      new bsp4j.WorkspaceBuildTargetsResult(res.getTargets.asScala.toList.map(_.duplicate()).asJava)
  }

  implicit class LocationExt(private val loc: bsp4j.Location) extends AnyVal {
    def duplicate(): bsp4j.Location =
      new bsp4j.Location(loc.getUri, loc.getRange.duplicate())
  }
  implicit class DiagnosticRelatedInformationExt(
      private val info: bsp4j.DiagnosticRelatedInformation
  ) extends AnyVal {
    def duplicate(): bsp4j.DiagnosticRelatedInformation =
      new bsp4j.DiagnosticRelatedInformation(info.getLocation.duplicate(), info.getMessage)
  }
  implicit class PositionExt(private val pos: bsp4j.Position) extends AnyVal {
    def duplicate(): bsp4j.Position =
      new bsp4j.Position(pos.getLine, pos.getCharacter)
  }
  implicit class RangeExt(private val range: bsp4j.Range) extends AnyVal {
    def duplicate(): bsp4j.Range =
      new bsp4j.Range(range.getStart.duplicate(), range.getEnd.duplicate())
  }
  implicit class DiagnosticExt(private val diag: bsp4j.Diagnostic) extends AnyVal {
    def duplicate(): bsp4j.Diagnostic = {
      val diag0 = new bsp4j.Diagnostic(diag.getRange.duplicate(), diag.getMessage)
      diag0.setCode(diag.getCode)
      diag0.setRelatedInformation(Option(diag.getRelatedInformation).map(_.duplicate()).orNull)
      diag0.setSeverity(diag.getSeverity)
      diag0.setSource(diag.getSource)
      diag0
    }
  }

}
