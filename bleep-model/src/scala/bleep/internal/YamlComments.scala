package bleep.internal

import bleep.internal.yamlemitter.CommentPlacingEmitter
import bleep.model
import io.circe.{Json, JsonObject}
import org.snakeyaml.engine.v2.api.{DumpSettings, LoadSettings, StreamDataWriter}
import org.snakeyaml.engine.v2.comments.{CommentLine, CommentType}
import org.snakeyaml.engine.v2.common.{FlowStyle, ScalarStyle}
import org.snakeyaml.engine.v2.composer.Composer
import org.snakeyaml.engine.v2.nodes.{MappingNode, Node, NodeTuple, ScalarNode, SequenceNode, Tag}
import org.snakeyaml.engine.v2.parser.ParserImpl
import org.snakeyaml.engine.v2.scanner.StreamReader
import org.snakeyaml.engine.v2.serializer.Serializer

import java.io.StringWriter
import java.util.Optional
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/** Carries the comments of a YAML file over to a rewritten version of it.
  *
  * Comments never enter the build model. They are read from the file about to be overwritten, keyed by where they sit in the document, and put back on the node
  * at the same place in the new document. A place is a path of mapping keys and sequence items. A sequence item is named by what it is rather than by its
  * position, so a dependency keeps its comment when the list is re-sorted or its version is bumped.
  *
  * snakeyaml hangs a comment on a node: comment lines above a mapping entry on its key node, a comment at the end of a line on the value node, comment lines
  * above a sequence item on the item. This mirrors that exactly, with one exception: comment lines above a mapping inside a sequence are moved from its first
  * key to the item itself, because key sorting may put a different key first.
  *
  * A comment whose place is gone from the new document is moved to a sequence item of the same identity under a key of the same name, if there is exactly one
  * (a dependency hoisted into a template). Anything else is returned as an orphan, for the caller to report.
  */
object YamlComments {
  sealed trait Segment
  object Segment {
    case class Key(name: String) extends Segment
    case class Item(identity: String) extends Segment
  }

  /** The node a comment hangs on: the key node of the mapping entry at `path`, or the value node at `path`. */
  case class Anchor(path: Vector[Segment], onKey: Boolean) {
    def render: String = {
      val segments = path.map {
        case Segment.Key(name)      => name
        case Segment.Item(identity) => s"[$identity]"
      }
      (if (segments.isEmpty) "<document>" else segments.mkString("/")) + (if (onKey) " (key)" else "")
    }
  }

  sealed trait Line {
    def toCommentLine: CommentLine
  }
  object Line {
    case object Blank extends Line {
      override def toCommentLine: CommentLine = new CommentLine(Optional.empty(), Optional.empty(), "", CommentType.BLANK_LINE)
    }
    case class Block(text: String) extends Line {
      override def toCommentLine: CommentLine = new CommentLine(Optional.empty(), Optional.empty(), text, CommentType.BLOCK)
    }
    case class InLine(text: String) extends Line {
      override def toCommentLine: CommentLine = new CommentLine(Optional.empty(), Optional.empty(), text, CommentType.IN_LINE)
    }

    def from(commentLine: CommentLine): Line =
      commentLine.getCommentType match {
        case CommentType.BLANK_LINE => Blank
        case CommentType.BLOCK      => Block(commentLine.getValue)
        case CommentType.IN_LINE    => InLine(commentLine.getValue)
      }
  }

  /** What snakeyaml attaches to one node: lines above it, at the end of its line, and after it (only ever the document's last node). */
  case class Comments(block: List[Line], inLine: List[Line], end: List[Line]) {
    def isEmpty: Boolean = block.isEmpty && inLine.isEmpty && end.isEmpty
    def ++(other: Comments): Comments = Comments(block ++ other.block, inLine ++ other.inLine, end ++ other.end)
    def hasText: Boolean = (block ++ inLine ++ end).exists(_ != Line.Blank)
    def render: String =
      (block ++ inLine ++ end)
        .collect {
          case Line.Block(text)  => s"#$text"
          case Line.InLine(text) => s"#$text"
        }
        .mkString("\n")
  }

  object Comments {
    val empty: Comments = Comments(Nil, Nil, Nil)

    def of(node: Node): Comments =
      Comments(lines(node.getBlockComments), lines(node.getInLineComments), lines(node.getEndComments))

    private def lines(commentLines: java.util.List[CommentLine]): List[Line] =
      if (commentLines == null) Nil else commentLines.asScala.toList.map(Line.from)
  }

  case class Index(byAnchor: Map[Anchor, Comments])
  object Index {
    val empty: Index = Index(Map.empty)
  }

  case class Orphan(anchor: Anchor, comments: Comments)

  case class Printed(yaml: String, orphans: List[Orphan])

  /** Sequences under these keys hold dependencies, identified by module rather than by their full text, which includes the version. */
  private val DependencyKeys: Set[String] =
    Set("dependencies", "boms", "jvmAgents", "annotationProcessors", "compilerPlugins", "symbolProcessors")

  private def identity(parentKey: Option[String], item: Json): String =
    parentKey match {
      case Some(key) if DependencyKeys(key) =>
        item.as[model.Dep] match {
          case Right(dep) =>
            val module = dep match {
              case java: model.Dep.JavaDependency   => s"${java.organization.value}:${java.moduleName.value}"
              case scala: model.Dep.ScalaDependency =>
                s"${scala.organization.value}${if (scala.fullCrossVersion) ":::" else "::"}${scala.baseModuleName.value}"
            }
            if (dep.configuration.value.isEmpty) module else s"$module (${dep.configuration.value})"
          case Left(error) => throw new IllegalArgumentException(s"Could not read ${item.noSpaces} under `$key` as a dependency", error)
        }
      case _ =>
        // sorted and shortened like the printed build, so the source's key order and explicit empty values do not matter
        item.foldWith(ShortenAndSortJson(List("item"))).noSpaces
    }

  private def parentKey(path: Vector[Segment]): Option[String] =
    path.lastOption.collect { case Segment.Key(name) => name }

  /** Collect the comments of `source`, which must be a single YAML document. */
  def extract(source: String): Index = {
    val loadSettings = LoadSettings.builder().setParseComments(true).build()
    val composer = new Composer(loadSettings, new ParserImpl(loadSettings, new StreamReader(loadSettings, source)))
    val root = composer.getSingleNode.orElseThrow(() => new IllegalArgumentException("Cannot collect comments from a YAML file without a document"))
    val rootJson = bleep.yaml.parse(source) match {
      case Right(json) => json
      case Left(error) => throw error
    }

    val byAnchor = mutable.LinkedHashMap.empty[Anchor, Comments]
    def record(anchor: Anchor, comments: Comments): Unit =
      if (!comments.isEmpty) byAnchor.update(anchor, byAnchor.getOrElse(anchor, Comments.empty) ++ comments)

    /* Comment lines indented deeper than the sibling they precede continue the previous sibling instead:
     *
     *   - org.scalameta:svm-subs:101.0.0
     *     # note: weird binary incompatibility when bumping this for scala3
     *   - org.slf4j:slf4j-api:2.0.17
     *
     * snakeyaml hangs them on the next sibling. They are moved to the end of the previous sibling's line, which only a scalar has.
     */
    def takeContinuation(node: Node, siblingColumn: Int, previous: Option[(Vector[Segment], Node)]): Unit =
      previous match {
        case Some((previousPath, _: ScalarNode)) if node.getBlockComments != null =>
          val (continuation, rest) = node.getBlockComments.asScala.toList.span { line =>
            line.getCommentType == CommentType.BLOCK && line.getStartMark.orElseThrow().getColumn > siblingColumn
          }
          record(Anchor(previousPath, onKey = false), Comments(Nil, continuation.map(line => Line.InLine(line.getValue)), Nil))
          node.setBlockComments(rest.asJava)
        case _ => ()
      }

    def walk(node: Node, json: Json, path: Vector[Segment], isSequenceItem: Boolean): Unit = {
      record(Anchor(path, onKey = false), Comments.of(node))
      node match {
        case mapping: MappingNode =>
          val obj = json.asObject.getOrElse(throw new IllegalStateException(s"YAML mapping at ${Anchor(path, onKey = false).render} parsed as ${json.name}"))
          var previous = Option.empty[(Vector[Segment], Node)]
          mapping.getValue.asScala.zipWithIndex.foreach { case (tuple, index) =>
            val key = tuple.getKeyNode match {
              case scalar: ScalarNode => scalar.getValue
              case other              => throw new IllegalArgumentException(s"Non-scalar mapping key at ${Anchor(path, onKey = false).render}: $other")
            }
            val keyPath = path :+ Segment.Key(key)
            takeContinuation(tuple.getKeyNode, tuple.getKeyNode.getStartMark.orElseThrow().getColumn, previous)
            previous = Some((keyPath, tuple.getValueNode))
            val keyComments = Comments.of(tuple.getKeyNode)
            if (isSequenceItem && index == 0) {
              record(Anchor(path, onKey = false), Comments(keyComments.block, Nil, Nil))
              record(Anchor(keyPath, onKey = true), keyComments.copy(block = Nil))
            } else record(Anchor(keyPath, onKey = true), keyComments)
            val value = obj(key).getOrElse(throw new IllegalStateException(s"Key `$key` at ${Anchor(path, onKey = false).render} missing from parsed JSON"))
            walk(tuple.getValueNode, value, keyPath, isSequenceItem = false)
          }
        case sequence: SequenceNode =>
          val items = json.asArray.getOrElse(throw new IllegalStateException(s"YAML sequence at ${Anchor(path, onKey = false).render} parsed as ${json.name}"))
          val dashColumn = sequence.getStartMark.orElseThrow().getColumn
          var previous = Option.empty[(Vector[Segment], Node)]
          sequence.getValue.asScala.zip(items).foreach { case (itemNode, itemJson) =>
            val itemPath = path :+ Segment.Item(identity(parentKey(path), itemJson))
            val commentsHolder = itemNode match {
              case mapping: MappingNode if !mapping.getValue.isEmpty => mapping.getValue.get(0).getKeyNode
              case other                                             => other
            }
            takeContinuation(commentsHolder, dashColumn, previous)
            previous = Some((itemPath, itemNode))
            walk(itemNode, itemJson, itemPath, isSequenceItem = true)
          }
        case _: ScalarNode => ()
        case other         => throw new IllegalArgumentException(s"Unexpected YAML node at ${Anchor(path, onKey = false).render}: $other")
      }
    }

    walk(root, rootJson, Vector.empty, isSequenceItem = false)
    Index(byAnchor.toMap)
  }

  // the settings circe-yaml's `Printer.Config(preserveOrder = true, dropNullKeys = true)` resolves to, plus comments
  private val dumpSettings: DumpSettings =
    DumpSettings
      .builder()
      .setIndent(2)
      .setWidth(80)
      .setSplitLines(true)
      .setIndicatorIndent(0)
      .setDefaultScalarStyle(ScalarStyle.PLAIN)
      .setExplicitStart(false)
      .setExplicitEnd(false)
      .setBestLineBreak("\n")
      .setDumpComments(true)
      .build()

  private class StringStreamWriter extends StringWriter with StreamDataWriter {
    override def flush(): Unit = super.flush()
  }

  /** Print `json` as YAML, putting back the comments in `index`. Output for an empty index is the same as circe-yaml's printer. */
  def print(json: Json, index: Index): Printed = {
    val keyNodes = mutable.LinkedHashMap.empty[Anchor, Node]
    val valueNodes = mutable.LinkedHashMap.empty[Anchor, Node]

    def isBad(s: String): Boolean = s.indexOf('\u0085') >= 0 || s.indexOf('﻿') >= 0
    def scalarStyle(value: String): ScalarStyle = if (isBad(value)) ScalarStyle.DOUBLE_QUOTED else ScalarStyle.PLAIN
    def stringScalarStyle(value: String): ScalarStyle =
      if (isBad(value)) ScalarStyle.DOUBLE_QUOTED else if (value.indexOf('\n') >= 0) ScalarStyle.LITERAL else ScalarStyle.PLAIN

    def build(json: Json, path: Vector[Segment]): Node = {
      val node: Node = json.fold(
        new ScalarNode(Tag.NULL, "null", scalarStyle("null")),
        bool => new ScalarNode(Tag.BOOL, bool.toString, scalarStyle(bool.toString)),
        number => new ScalarNode(if (number.toString.contains(".")) Tag.FLOAT else Tag.INT, number.toString, scalarStyle(number.toString)),
        str => new ScalarNode(Tag.STR, str, stringScalarStyle(str)),
        array => new SequenceNode(Tag.SEQ, array.map(item => build(item, path :+ Segment.Item(identity(parentKey(path), item)))).asJava, FlowStyle.BLOCK),
        obj => buildMapping(obj, path)
      )
      valueNodes.update(Anchor(path, onKey = false), node)
      node
    }

    def buildMapping(obj: JsonObject, path: Vector[Segment]): Node = {
      val tuples = obj.toList.collect {
        case (key, value) if !value.isNull =>
          val keyPath = path :+ Segment.Key(key)
          val keyNode = new ScalarNode(Tag.STR, key, scalarStyle(key))
          keyNodes.update(Anchor(keyPath, onKey = true), keyNode)
          new NodeTuple(keyNode, build(value, keyPath))
      }
      new MappingNode(Tag.MAP, tuples.asJava, FlowStyle.BLOCK)
    }

    val root = build(json, Vector.empty)

    def nodeAt(anchor: Anchor): Option[Node] =
      if (anchor.onKey) keyNodes.get(anchor) else valueNodes.get(anchor)

    // a sequence item whose place is gone: the one item with the same identity under a key of the same name
    def relocated(anchor: Anchor): Option[Anchor] =
      anchor.path.lastOption match {
        case Some(item: Segment.Item) if !anchor.onKey =>
          val key = parentKey(anchor.path.init)
          valueNodes.keys.filter(candidate => candidate.path.lastOption.contains(item) && parentKey(candidate.path.init) == key).toList match {
            case List(only) => Some(only)
            case _          => None
          }
        case _ => None
      }

    val attached = mutable.LinkedHashMap.empty[Anchor, List[Comments]]
    val orphans = List.newBuilder[Orphan]
    index.byAnchor.foreach { case (anchor, comments) =>
      val target = if (nodeAt(anchor).isDefined) Some(anchor) else relocated(anchor)
      target match {
        case Some(target) =>
          // several projects' comments on one dependency land on the same hoisted item; keep one copy of each
          val existing = attached.getOrElse(target, Nil)
          if (!existing.contains(comments)) attached.update(target, existing :+ comments)
        case None =>
          if (comments.hasText) orphans += Orphan(anchor, comments)
      }
    }
    attached.foreach { case (anchor, commentss) =>
      val node = nodeAt(anchor).get
      val comments = commentss.reduce(_ ++ _)
      node.setBlockComments(comments.block.map(_.toCommentLine).asJava)
      node.setInLineComments(comments.inLine.map(_.toCommentLine).asJava)
      node.setEndComments(comments.end.map(_.toCommentLine).asJava)
    }

    val writer = new StringStreamWriter
    val serializer = new Serializer(dumpSettings, new CommentPlacingEmitter(dumpSettings, writer))
    serializer.emitStreamStart()
    serializer.serializeDocument(root)
    serializer.emitStreamEnd()
    Printed(writer.toString, orphans.result())
  }
}
