package bleep
package internal

import java.nio.charset.StandardCharsets
import java.nio.file.Path
import scala.util.control.NonFatal

/** Run a read-only `git` command in `buildDir` and return its stdout **verbatim**.
  *
  * Deliberately not `scala.sys.process`'s `!!`. That reads the child's stdout through a `BufferedReader`, splits it into lines and rejoins them with `'\n'`,
  * which appends a newline that was never in the output and rewrites any `'\r'` it passes. Every caller here asks git for `-z` — NUL-separated records —
  * precisely so that no byte inside a path is reinterpreted, and a reader that reinterprets bytes defeats that: the phantom trailing newline arrives as an
  * extra, malformed record, and `'\r'` is a legal character in a git path.
  *
  * stderr is discarded. Each caller treats a non-zero exit as "git has no answer here" — no repo, no `HEAD` — and handles it; printing git's complaint at the
  * user on every digest of a fresh repository is noise about a case that is already handled.
  *
  * @throws BleepException.Text
  *   if git exits non-zero.
  */
object gitOutput {
  def apply(buildDir: Path, args: List[String]): String = {
    val process = new ProcessBuilder(args*)
      .directory(buildDir.toFile)
      .redirectError(ProcessBuilder.Redirect.DISCARD)
      .start()
    // Read before waiting: a full pipe buffer deadlocks a child we are waiting on.
    val bytes = process.getInputStream.readAllBytes()
    val exitCode = process.waitFor()
    if (exitCode != 0)
      throw new BleepException.Text(s"`${args.mkString(" ")}` in $buildDir exited with $exitCode")
    new String(bytes, StandardCharsets.UTF_8)
  }

  /** [[apply]], but `None` when git exits non-zero — "git has no answer here": no repository, or no `HEAD` because nothing is committed yet.
    *
    * Only the *process* failure is turned into `None`. Parsing git's output is the caller's job and happens outside this, so a record we cannot read throws
    * instead of being mistaken for an absent repository — the difference between "no fast path available" and "the fast path is broken", which otherwise looks
    * identical from the outside and stays hidden for as long as the slow path happens to be correct.
    */
  def attempt(buildDir: Path, args: List[String]): Option[String] =
    try Some(apply(buildDir, args))
    catch { case NonFatal(_) => None }
}
