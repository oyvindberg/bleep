package bleep.model

import io.circe.{Decoder, Encoder, Json}

/** A description of javac/scalac options.
  *
  * Because some options have arguments we need to take some care when operating on them as sets
  */
case class Options(values: Set[Options.Opt]) extends SetLike[Options] {
  def render: List[String] = values.toList.sorted.flatMap(_.render)
  def isEmpty: Boolean = values.isEmpty

  override def union(other: Options) = new Options(values ++ other.values)
  override def intersect(other: Options) = new Options(values.intersect(other.values))
  override def removeAll(other: Options) = new Options(values -- other.values)
}

object Options {
  def fromIterable(opts: Iterable[Opt]): Options =
    new Options(opts.toSet)

  val empty = new Options(Set.empty)
  implicit val decodes: Decoder[Options] = Decoder[JsonList[String]].map(list => Options.parse(list.values, maybeRelativize = None))
  implicit val encodes: Encoder[Options] = Encoder.instance {
    case opts if opts.isEmpty => Json.Null
    case opts                 => Json.fromString(opts.render.mkString(" "))
  }

  def parse(strings: List[String], maybeRelativize: Option[Replacements]): Options = {
    val splitStrings = strings.flatMap(_.split("\\s+")).filter(_.trim.nonEmpty)

    val relativeStrings = maybeRelativize match {
      case Some(relativize) => splitStrings.map(relativize.templatize.string)
      case None             => splitStrings
    }

    val opts = relativeStrings.foldLeft(List.empty[Options.Opt]) {
      case (current :: rest, arg) if !arg.startsWith("-") => current.withArg(arg) :: rest
      case (acc, str) if str.startsWith("-")              => Opt.Flag(str) :: acc
      // revisit this
      case (_, nonMatching) => sys.error(s"unexpected ${nonMatching}")
    }

    fromIterable(opts)
  }

  sealed trait Opt {
    def render: List[String]
    def withArg(str: String): Opt.WithArgs
  }

  object Opt {
    implicit val ordering: Ordering[Opt] = Ordering.by(_.render.mkString(" "))

    case class Flag(name: String) extends Opt {
      override def render: List[String] = List(name)
      override def withArg(arg: String): WithArgs = WithArgs(name, List(arg))
    }

    case class WithArgs(name: String, args: List[String]) extends Opt {
      override def render: List[String] = name :: args
      override def withArg(arg: String): WithArgs = WithArgs(name, args :+ arg)
    }
  }
}
