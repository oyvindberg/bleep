package bleep.bsp

/** A reference to an object that another classloader's `scala-library` built.
  *
  * A `TestAdapter` that `CompilerResolver` resolves brings its own `scala-library`. The adapter's methods take and return that library's `List`, `Map`, and
  * `Option`. Those three classes differ from bleep's own three classes of the matching names.
  *
  * Each subtype of AlienValue keeps the alien object next to the classloader that built the alien object.
  */
sealed trait AlienValue {

  /** The alien object. Reflection accepts this object. A constructor that [[loader]] supplied accepts this object. Nothing else accepts it.
    *
    * The accessor is `private[bsp]` to keep raw reflection inside the BSP server. Code that reaches for it belongs beside a test adapter, such as
    * `ScalaJsTestRunner.JsTestAdapter`. Every other caller uses a named method on the subtype.
    */
  private[bsp] def underlying: AnyRef

  /** The classloader that owns [[underlying]]. */
  def loader: ClassLoader
}

/** A `scala.collection.immutable.List` that [[loader]] built. */
final case class AlienList(private[bsp] val underlying: AnyRef, loader: ClassLoader) extends AlienValue {

  /** Every element of this list. [[loader]] still owns each element. */
  def elements: List[AnyRef] = {
    val builder = List.newBuilder[AnyRef]
    val nilObj = AlienList.nilIn(loader)
    var current = underlying
    while (current != nilObj) {
      // Every cell of a `List` is a `$colon$colon`. The final cell is `Nil$`. `$colon$colon` and `Nil$` each declare `head` and `tail`. A `Method` that
      // `$colon$colon` declares rejects a `Nil$` receiver. This lookup therefore repeats for every cell.
      builder += current.getClass.getMethod("head").invoke(current)
      current = current.getClass.getMethod("tail").invoke(current)
    }
    builder.result()
  }

  /** Every element cast to an `A`.
    *
    * The cast succeeds only for a class that [[loader]] and bleep's own classloader resolve alike. The parent classloader supplies `sbt.testing.Framework` to
    * both classloaders. `sbt.testing.Framework` is the only class this method casts to today.
    */
  def as[A]: List[A] = elements.map(_.asInstanceOf[A])
}

object AlienList {

  /** Builds a list inside `loader`, cell by cell.
    *
    * @param elements
    *   the elements, each already belonging to `loader` or to a class that `loader` and bleep's own classloader resolve alike
    * @param loader
    *   the classloader that builds the list
    */
  def of(elements: List[AnyRef], loader: ClassLoader): AlienList = {
    val consConstructor = loader
      .loadClass("scala.collection.immutable.$colon$colon")
      .getConstructor(classOf[Object], loader.loadClass("scala.collection.immutable.List"))
    AlienList(elements.foldRight(nilIn(loader))((element, acc) => consConstructor.newInstance(element, acc)), loader)
  }

  private def nilIn(loader: ClassLoader): AnyRef =
    loader.loadClass("scala.collection.immutable.Nil$").getField("MODULE$").get(null)
}

/** A `scala.collection.immutable.Map` that [[loader]] built. */
final case class AlienMap(private[bsp] val underlying: AnyRef, loader: ClassLoader) extends AlienValue

object AlienMap {

  /** Builds a map inside `loader`, one entry at a time. */
  def of(entries: Map[String, String], loader: ClassLoader): AlienMap = {
    val mapCompanion = loader.loadClass("scala.collection.immutable.Map$")
    val mapObj = mapCompanion.getField("MODULE$").get(null)
    var result = mapCompanion.getMethod("empty").invoke(mapObj)
    entries.foreach { case (k, v) =>
      // Each added entry produces a map of a different class. The classes run `Map$Map1` through `Map$Map4`, then `HashMap`. `getMethod` returns a
      // `Method` that the queried class declares. A `Method` that `Map$EmptyMap$` declares rejects a `Map$Map1` receiver. This lookup repeats for every
      // entry.
      val updatedMethod = result.getClass.getMethod("updated", classOf[Object], classOf[Object])
      result = updatedMethod.invoke(result, k, v)
    }
    AlienMap(result, loader)
  }
}

/** A `scala.Option` that [[loader]] built. */
final case class AlienOption(private[bsp] val underlying: AnyRef, loader: ClassLoader) extends AlienValue {

  /** The value cast to an `A`. [[AlienList.as]] states when that cast succeeds. */
  def as[A]: Option[A] = {
    val noneObj = loader.loadClass("scala.None$").getField("MODULE$").get(null)
    if (underlying == noneObj) None
    else Option(underlying.getClass.getMethod("get").invoke(underlying).asInstanceOf[A])
  }
}
