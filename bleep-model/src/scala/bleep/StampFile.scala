package bleep

/** Where a project's stamps live — the one definition shared by the path model, the code that writes them, and anyone reading them back.
  *
  * On disk the stamps directory is a resource ROOT under `generated-resources/`, and it contains a namespace directory, so what lands in the jar is
  * `bleep-stamp/<project>.properties`:
  * {{{
  * .bleep/projects/dlab-version/generated-resources/bleep-stamps/   <- the root; this is what goes on the runtime classpath
  *   └── bleep-stamp/
  *         └── dlab-version.properties                             <- bleep-stamp/dlab-version.properties in the jar
  * }}}
  */
object StampFile {

  /** The root folder under `generated-resources/`. Hyphenated because sourcegen output sits beside it in folders named after the generator's main class, and a
    * hyphen cannot appear in a class name — so no generator can ever claim this folder.
    */
  val rootFolder: String = "bleep-stamps"

  /** The namespace directory inside the root, and therefore inside the jar. Hyphenated so it can never be a Java package, so it can never collide with
    * anybody's classes; and not `META-INF/`, which is exactly what proguard and shading tools carry filter rules for.
    */
  val resourceDir: String = "bleep-stamp"

  /** The resource path a project's stamps are read from at runtime, named by the cross project (`lib@jvm3`) so that every stamped project on one classpath can
    * coexist.
    */
  def resourcePath(crossName: model.CrossProjectName): String = s"$resourceDir/${crossName.fileSafeValue}.properties"
}
