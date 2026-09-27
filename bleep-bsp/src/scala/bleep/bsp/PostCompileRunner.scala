package bleep.bsp

import bleep.*
import bleep.bsp.Outcome.RunOutcome
import bleep.analysis.TransformedClass
import bleep.bsp.protocol.KillReason
import bleep.internal.{jvmRunCommand, FileUtils}
import bleep.model.CrossProjectName
import cats.effect.{Deferred, IO}

import java.io.File
import java.nio.file.{Files, Path, StandardCopyOption}
import java.util.Arrays
import scala.jdk.CollectionConverters.*

/** Runs a project's `postCompile` script: the compiler's output in, the project's classes out.
  *
  * The compiler writes [[ProjectPaths.compilerOutput]], which only this step reads. The script is forked on its project's runtime classpath with
  *
  * {{{
  *   --from <compiler output>                 read-only
  *   --to <empty directory>                   to be filled with the project's COMPLETE output
  *   --classpath <the project's compile classpath, path-separated>
  *   --input <cross project name>=<its classes>   once per declared input
  * }}}
  *
  * and only a successful run reaches [[ProjectPaths.classes]]: synced file by file (unchanged bytes keep their timestamps, files the script no longer produces
  * are deleted), its [[PostCompileAbi]] delta recorded for consumers, then stamped with the fingerprint of what produced it. Until the stamp is written,
  * `classes` is not vouched for, so a failed, killed or crashed run can never be mistaken for an up-to-date one.
  *
  * The fingerprint covers everything the script can read: the compiler output, the script's classpath and main, the inputs' classes, the compile classpath's
  * contents — an enhancer may read annotation metadata from a jar this project's sources never mention — and the JVM it runs on.
  */
object PostCompileRunner {

  private case class Plan(
      postCompile: model.PostCompile,
      paths: ProjectPaths,
      scriptClasspath: List[Path],
      inputs: List[(CrossProjectName, Path)],
      compileClasspath: List[Path],
      javaBin: Path
  ) {
    lazy val fingerprint: String =
      PathFingerprint.of(
        names = javaBin.toString :: postCompile.main :: inputs.map(_._1.value),
        paths = paths.compilerOutput :: scriptClasspath ::: inputs.map(_._2) ::: compileClasspath
      )
  }

  private def plan(started: Started, project: CrossProjectName): Plan = {
    val postCompile = started.build
      .explodedProjects(project)
      .postCompile
      .getOrElse(throw new BleepException.Text(project, "asked to run a post-compile step, but the project declares no `postCompile`"))
    Plan(
      postCompile = postCompile,
      paths = started.projectPaths(project),
      scriptClasspath = fixedClasspath(started.resolvedProject(postCompile.project)),
      inputs = postCompile.inputs.values.toList.map(input => input -> started.projectPaths(input).classes),
      compileClasspath = started.resolvedProject(project).classpath(Usage.Compile),
      javaBin = started.resolvedJvm.forceGet.javaBin
    )
  }

  /** Whether the project's classes are the script's output for the current inputs. */
  def upToDate(started: Started, project: CrossProjectName): Boolean =
    upToDateFor(plan(started, project))

  /** Run the script if the classes are not already its output for the current inputs. Caller holds the project's exclusive lock.
    *
    * @return
    *   None on success, the reason otherwise
    */
  def run(started: Started, project: CrossProjectName, killSignal: Deferred[IO, KillReason], onLog: String => Unit): IO[Option[String]] =
    IO.blocking(plan(started, project)).flatMap { p =>
      if (upToDateFor(p)) IO.pure(None)
      else {
        val what = s"Post-compile ${p.postCompile.main} for ${project.value}"
        val scratch = p.paths.targetDir.resolve("post-compile-tmp")
        val prepare = IO.blocking {
          // Whatever is in `classes` stops being vouched for the moment we start replacing it.
          Files.deleteIfExists(p.paths.postCompileStamp)
          Files.deleteIfExists(p.paths.postCompileAbi)
          if (Files.exists(scratch)) FileUtils.deleteDirectory(scratch)
          Files.createDirectories(scratch)
        }
        val args =
          List("--from", p.paths.compilerOutput.toString, "--to", scratch.toString, "--classpath", p.compileClasspath.mkString(File.pathSeparator)) ++
            p.inputs.flatMap { case (name, dir) => List("--input", s"${name.value}=$dir") }
        val jvmOptions = List(s"-Xmx${MachineResources.forkHeapMb(started.config.bspServerConfigOrDefault.sourcegenMaxMemory)}m")
        val cmd = jvmRunCommand.cmd(started.resolvedJvm.forceGet, jvmOptions, p.scriptClasspath, p.postCompile.main, args)
        val pb = new ProcessBuilder(cmd*)
        pb.directory(started.buildPaths.buildDir.toFile)

        (prepare >> ProcessRunner.runWithOutput(pb, killSignal).flatMap {
          case RunOutcome.Completed(0, stdout, _) =>
            IO.blocking {
              if (stdout.nonEmpty) stdout.linesIterator.foreach(onLog)
              sync(scratch, p.paths.classes)
              Files.writeString(p.paths.postCompileAbi, TransformedClass.write(PostCompileAbi.delta(p.paths.compilerOutput, p.paths.classes)))
              Files.writeString(p.paths.postCompileStamp, p.fingerprint)
              None
            }
          case RunOutcome.Completed(exitCode, _, stderr) =>
            IO.pure(Some(SourceGenRunner.forkFailureMessage(what, s"exit code $exitCode", stderr)))
          case RunOutcome.Crashed(signal, _, _, stderr) =>
            IO.pure(Some(SourceGenRunner.forkFailureMessage(what, SourceGenRunner.describeSignal(signal), stderr)))
          case RunOutcome.Killed(reason, _, _) =>
            IO.pure(Some(s"$what killed: $reason"))
        }).guarantee(IO.blocking(if (Files.exists(scratch)) FileUtils.deleteDirectory(scratch)))
      }
    }

  private def upToDateFor(p: Plan): Boolean =
    Files.isDirectory(p.paths.classes) && Files.isRegularFile(p.paths.postCompileAbi) && Files.isRegularFile(p.paths.postCompileStamp) &&
      Files.readString(p.paths.postCompileStamp) == p.fingerprint

  /** Make `to` hold exactly what `from` holds. Files whose bytes are unchanged are left alone, so their timestamps — and everything keyed on them downstream —
    * stay put.
    */
  private def sync(from: Path, to: Path): Unit = {
    Files.createDirectories(to)
    val produced: Set[Path] = walkFiles(from).map(from.relativize).toSet
    produced.foreach { rel =>
      val source = from.resolve(rel)
      val target = to.resolve(rel)
      val unchanged =
        Files.isRegularFile(target) && Files.size(target) == Files.size(source) && Arrays.equals(Files.readAllBytes(target), Files.readAllBytes(source))
      if (!unchanged) {
        Files.createDirectories(target.getParent)
        Files.copy(source, target, StandardCopyOption.REPLACE_EXISTING): Unit
      }
    }
    walkFiles(to).foreach { f =>
      if (!produced.contains(to.relativize(f))) Files.delete(f)
    }
    // Directories emptied by the deletes above
    val dirs = {
      val stream = Files.walk(to)
      try stream.iterator().asScala.filter(d => Files.isDirectory(d) && d != to).toList
      finally stream.close()
    }
    dirs.sortBy(-_.getNameCount).foreach { d =>
      val stream = Files.list(d)
      val empty = try !stream.iterator().hasNext
      finally stream.close()
      if (empty) Files.delete(d)
    }
  }

  private def walkFiles(dir: Path): List[Path] = {
    val stream = Files.walk(dir)
    try stream.iterator().asScala.filter(Files.isRegularFile(_)).toList
    finally stream.close()
  }
}
