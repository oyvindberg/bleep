package bleep.model

/** A project that must be built before another, without reaching its classpath: the other project uses its *output* for something — to run it, to compile with
  * it, to read it. All of them are scheduled alike (the project and its whole `dependsOn` closure compile first); only what uses the output differs.
  */
case class IndirectDependency(project: CrossProjectName, reason: IndirectDependency.Reason)

object IndirectDependency {
  sealed trait Reason
  object Reason {

    /** Runs `main` to generate sources, before compiling. */
    case class Sourcegen(main: String) extends Reason

    /** Its output is read by the sourcegen script `main`. */
    case class SourcegenInput(main: String) extends Reason

    /** Its runtime classpath is the Scala compiler, its own classes the zinc bridge. */
    case object ScalaCompiler extends Reason

    /** Runs `main` to rewrite the compiled classes. */
    case class PostCompileScript(main: String) extends Reason

    /** Its output is read by the post-compile script. */
    case object PostCompileInput extends Reason
  }
}
