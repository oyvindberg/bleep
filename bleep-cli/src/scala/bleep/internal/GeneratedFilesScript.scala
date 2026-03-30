package bleep
package internal

import scala.collection.immutable.SortedSet

object GeneratedFilesScript {
  @FunctionalInterface
  trait Repr[T] {
    def str(t: T, indent: Int): String
  }

  object Repr {
    def apply[T: Repr]: Repr[T] = implicitly[Repr[T]]
    def str[T: Repr](t: T, indent: Int): String = Repr[T].str(t, indent)

    implicit val boolean: Repr[Boolean] =
      (b, _) => b.toString

    /** awful everything. How to put an arbitrary string and put inside a scala string without changing it? it's a long story:
      *   - to not make multiline strings awfully ugly, triple quotes are used
      *   - if the string included triple quotes, we escape them with interpolation. I have found no other way. this forces us into using the `s` interpolator
      *   - because of that `$` need to be escaped
      *   - backslashes are always a problem
      *
      * in total this was improvised and is most certainly very wrong in many ways. PRs welcome :D
      */
    implicit val string: Repr[String] = {
      val pipe = "|"
      val space = " "
      val tripleQuote = "\"\"\""
      // JVM constant pool limit is 65535 bytes per UTF8 string.
      // Stay well under to account for multi-byte chars.
      val MaxChunkBytes = 40000
      (str, indent) =>
        // Always use triple-quoted string with s-interpolator and stripMargin.
        // In Scala 3, s"""...""" processes standard escape sequences (\n, \", \\, etc.),
        // so backslashes must be doubled to survive evaluation.
        // $ must be escaped as $$, and """ must use interpolation.
        def renderChunk(chunk: String): String =
          chunk
            .replace("\\", "\\\\")
            .replace("$", "$$")
            .replace(tripleQuote, s"$${\"\\\"\" * 3}")
            .split("\n", -1)
            .mkString(
              start = s"s$tripleQuote$pipe",
              sep = s"\n${space * indent}$pipe",
              end = s"$tripleQuote.stripMargin"
            )

        // Split into chunks on line boundaries to stay under JVM string limit
        val chunks = List.newBuilder[String]
        val current = new StringBuilder
        for (line <- str.split("\n", -1)) {
          if (current.nonEmpty && current.length + line.length + 1 > MaxChunkBytes) {
            chunks += current.toString
            current.clear()
          }
          if (current.nonEmpty) current.append('\n')
          current.append(line)
        }
        if (current.nonEmpty) chunks += current.toString

        chunks.result() match {
          case List(one) => renderChunk(one)
          case more      => more.map(renderChunk).mkString("(", " + ", ")")
        }
    }

    implicit def opt[T: Repr]: Repr[Option[T]] = {
      case (Some(t), indent) => s"Some(${Repr[T].str(t, indent)})"
      case (None, _)         => "None"
    }

    implicit val crossId: Repr[bleep.model.CrossId] = { case (bleep.model.CrossId(value), indent) =>
      s"bleep.model.CrossId(${str(value, indent)})"
    }

    implicit val projectName: Repr[bleep.model.ProjectName] = { case (bleep.model.ProjectName(value), indent) =>
      s"bleep.model.ProjectName(${str(value, indent)})"
    }

    implicit val crossProjectName: Repr[bleep.model.CrossProjectName] = { case (bleep.model.CrossProjectName(projectName, crossId), indent) =>
      s"bleep.model.CrossProjectName(${str(projectName, indent)}, ${str(crossId, indent)})"
    }

    implicit def tuple2[T1: Repr, T2: Repr]: Repr[(T1, T2)] = { case ((t1, t2), indent) =>
      s"(${str(t1, indent)}, ${str(t2, indent)})"
    }

    implicit def map[K: Repr, V: Repr]: Repr[Map[K, V]] = { case (map, indent) =>
      val space = "\n" + (" " * indent)
      map.map(str(_, indent + 2)).mkString(s"Map($space", s",$space", s"$space)")
    }

    implicit def set[T: Repr]: Repr[Set[T]] = { case (set, indent) =>
      set.map(Repr[T].str(_, indent + 2)).mkString("Set(", ", ", ")")
    }

    implicit def relPath: Repr[RelPath] = { case (relPath, indent) =>
      s"bleep.RelPath.force(${str(relPath.toString, indent)})"
    }

    implicit def generatedFile: Repr[GeneratedFile] = { case (GeneratedFile(isResource, fromRelPath, toRelPath), indent) =>
      s"bleep.GeneratedFile(${str(isResource, indent)}, ${str(fromRelPath, indent)}, ${str(toRelPath, indent)})"
    }
  }

  case class ImportedGeneratorScript(pkgs: List[String], className: String, contents: String) {
    def qname = (pkgs :+ className).mkString(".")
  }

  def titleCase(str: String) = {
    def go(str: String) = str.head.toTitleCase.toString + str.drop(1)
    str.split("[-_]").map(go).mkString("")
  }

  def apply(pkgs: List[String], generatedFiles: Map[model.CrossProjectName, Vector[GeneratedFile]]): Map[model.ProjectName, ImportedGeneratorScript] =
    generatedFiles
      .filter { case (_, files) => files.nonEmpty }
      .groupBy { case (model.CrossProjectName(name, _), _) => name }
      .map { case (projectName, filesByCross: Map[model.CrossProjectName, Vector[GeneratedFile]]) =>
        val className = s"GenerateFor${titleCase(projectName.value)}"
        val allFiles: Vector[GeneratedFile] =
          filesByCross.flatMap { case (_, files) => files }.toVector.distinct.sorted

        val copies = allFiles.map { file =>
          val projectsWithFile: SortedSet[model.CrossProjectName] =
            SortedSet.empty[model.CrossProjectName] ++ filesByCross.collect { case (crossName, files) if files.contains(file) => crossName }

          def dir = if (file.isResource) "resources" else "sources"

          s"""
    targets.foreach { target =>
      if (${Repr.str(projectsWithFile.map(_.value).toSet, 6)}.contains(target.project.value)) {
        val to = target.$dir.resolve(${Repr.str(file.toRelPath.toString, 8)})
        started.logger.withContext("project", target.project.value).warn(s"Writing $$to")
        val content = ${Repr.str(file.contents, 6)}
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }
"""
        }

        val script =
          s"""
package ${pkgs.mkString(".")}

import bleep.{BleepCodegenScript, Commands, Started}

import java.nio.file.Files

object $className extends BleepCodegenScript("$className") {
  override def run(started: Started, commands: Commands, targets: List[Target], args: List[String]): Unit = {
    started.logger.error("This script is a placeholder! You'll need to replace the contents with code which actually generates the files you want")
${copies.mkString("\n\n")}
  }
}"""
        (projectName, ImportedGeneratorScript(pkgs, className, script))
      }
}
