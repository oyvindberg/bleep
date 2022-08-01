package bleep.internal

import bleep.model

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
      val quote = "\""
      val space = " "
      val tripleQuote = quote * 3
      val triggersTripleQuote = Set('\n', '\\', '"')
      val MaxStringLiteral = 5000
      (str, indent) =>
        val cleaned = str.replace("\\", "\\\\")
        val groups = cleaned
          .grouped(MaxStringLiteral)
          .map { str =>
            if (str.exists(triggersTripleQuote))
              str
                .replace("$", "$$")
                .replace(tripleQuote, s"$${\"\\\"\" * 3}")
                .split("\n")
                .mkString(
                  start = s"s$tripleQuote$pipe",
                  sep = s"\n${space * indent}$pipe",
                  end = s"$tripleQuote.stripMargin"
                )
            else s"$quote$cleaned$quote"
          }
          .toList

        groups match {
          case List(one) => one
          case more =>
            more.mkString(
              "new String(",
              ") + new String(",
              ")"
            )
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

    implicit def vector[T: Repr]: Repr[Vector[T]] = { case (vector, indent) =>
      val space = "\n" + (" " * indent)
      vector.map(Repr[T].str(_, indent + 2)).mkString(s"Vector($space", s",$space  ", ")")
    }

    implicit def relPath: Repr[bleep.RelPath] = { case (relPath, indent) =>
      s"bleep.RelPath.force(${str(relPath.toString, indent)})"
    }

    implicit def generatedFile: Repr[GeneratedFile] = { case (GeneratedFile(isResource, fromRelPath, toRelPath), indent) =>
      s"bleep.GeneratedFile(${str(isResource, indent)}, ${str(fromRelPath, indent)}, ${str(toRelPath, indent)})"
    }
  }

  def apply(generatedFiles: Map[bleep.model.CrossProjectName, Vector[GeneratedFile]]): Option[(String, String)] =
    if (generatedFiles.isEmpty) None
    else {

      val byFile: Map[GeneratedFile, Vector[model.CrossProjectName]] =
        generatedFiles.toVector.flatMap { case (crossName, files) => files.map(file => file -> crossName) }.groupMap(_._1)(_._2)

      // important for snapshot tests
      val sorted = byFile.toVector.sortBy(x => (x._2.head.value, x._1.toRelPath))

      val copies = sorted.map { case (file, forProjects) =>
        def dir =
          if (file.isResource) "generatedResourcesDir" else "generatedSourcesDir"

        s"""
    ${Repr.str(forProjects, 6)}.foreach { crossName =>
      val to = started.buildPaths.$dir(crossName).resolve(${Repr.str(file.toRelPath.toString, 8)})
      started.logger.withContext(crossName).warn(s"Writing $$to")
      val content = ${Repr.str(file.contents, 6)}
      Files.createDirectories(to.getParent)
      Files.writeString(to, content)
    }
"""
      }

      val script = s"""
package scripts

import java.nio.file.Files

object GenerateResources extends App {
  bleep.bootstrap.forScript("GenerateResources") { (started, commands) =>
    started.logger.error("This script is a placeholder! You'll need to replace the contents with code which actually generates the files you want")

    ${copies.mkString("\n\n")}
  }
}"""

      Some(("scripts.GenerateResources", script))
    }
}
