package bleep.bsp

import bleep.*
import bleep.analysis.{OutputDeterminants, TransformedClass}
import bleep.model.CrossProjectName

import java.nio.file.Files

/** A project's [[OutputDeterminants]], as of now. Called when its compile task runs, so everything it reads — the compiler project's digest, the classes each
  * post-compiled dependency's transform changed — describes the builds this compile will use: the DAG has finished them.
  */
object OutputDeterminantsOf {
  def apply(started: Started, project: CrossProjectName, projectDigest: CrossProjectName => String): OutputDeterminants = {
    val compiler = started.build.explodedProjects(project).scala.flatMap(_.compilerProject).map(cp => s"${cp.value}:${projectDigest(cp)}")
    val transformed = started.build.transitiveDependenciesFor(project).keys.toList.sorted.flatMap { dep =>
      val paths = started.projectPaths(dep)
      if (!paths.hasPostCompile) None
      else if (!Files.isRegularFile(paths.postCompileAbi))
        throw new BleepException.Text(
          project,
          s"compiles against ${dep.value}, whose post-compile step has not recorded its ABI (${paths.postCompileAbi}). It runs as part of compiling ${dep.value}, which must finish first."
        )
      else TransformedClass.read(Files.readString(paths.postCompileAbi))
    }
    OutputDeterminants(compiler = compiler, transformedClasses = transformed.map(c => c.binaryName -> c).toMap)
  }
}
