package bleep

import bleep.Options.Opt
import bleep.internal.SetLike
import io.circe.{Decoder, Encoder}

/** A description of javac/scalac options.
  *
  * Because some options have arguments we need to take some care when operating on them as sets
  */
case class Options(values: List[Options.Opt]) extends SetLike[Options] {
  def render: List[String] = values.flatMap(_.render)
  def sorted = new Options(values.sorted)
  def isEmpty: Boolean = values.isEmpty

  override def union(other: Options) = new Options((values ::: other.values).distinct)
  override def intersect(other: Options) = new Options(values.intersect(other.values))
  override def removeAll(other: Options) = new Options(values.filterNot(other.values.contains))

  def nameFilter(pred: String => Boolean): Options =
    new Options(values.filter {
      case Opt.Flag(name)        => pred(name)
      case Opt.WithArgs(name, _) => pred(name)
    })
}

object Options {
  val empty = new Options(Nil)
  implicit val decodes: Decoder[Options] = Decoder[JsonList[String]].map(list => Options.parse(list.values))
  implicit val encodes: Encoder[Options] = Encoder[JsonList[String]].contramap(opts => JsonList(opts.render))

  def parse(strings: List[String]): Options = {
    val opts = strings.foldLeft(List.empty[Options.Opt]) {
      case (current :: rest, arg) if !arg.startsWith("-") => current.withArg(arg) :: rest
      case (acc, str) if str.startsWith("-")              => Opt.Flag(str) :: acc
      // revisit this
      case (_, nonMatching) => sys.error(s"unexpected ${nonMatching}")
    }
    new Options(opts)
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
