package bleep.analysis

import java.io.{InputStream, OutputStream, Writer}
import java.nio.charset.Charset
import java.util.Locale
import javax.lang.model.SourceVersion
import javax.tools.*

/** javac, as the compile server runs it, with annotation processors and javac plugins (`-Xplugin`) loaded apart from the server's own classes.
  *
  * javac loads them in a class loader whose parent is the loader which loaded javac: in the server, that is the one with the whole server on it. A class the
  * server has too then comes from the server: error prone got bleep's guava 30 instead of its own, and failed with a `NoSuchMethodError`. Here the parent is
  * the platform class loader, which has the JDK, `jdk.compiler` included, and nothing else, like javac run on its own.
  */
final class IsolatedProcessorsJavaCompiler(delegate: JavaCompiler) extends JavaCompiler {
  override def getTask(
      out: Writer,
      fileManager: JavaFileManager,
      diagnosticListener: DiagnosticListener[? >: JavaFileObject],
      options: java.lang.Iterable[String],
      classes: java.lang.Iterable[String],
      compilationUnits: java.lang.Iterable[? <: JavaFileObject]
  ): JavaCompiler.CompilationTask =
    delegate.getTask(out, fileManager, diagnosticListener, options, classes, compilationUnits)

  override def getStandardFileManager(
      diagnosticListener: DiagnosticListener[? >: JavaFileObject],
      locale: Locale,
      charset: Charset
  ): StandardJavaFileManager =
    new IsolatedProcessorsFileManager(delegate.getStandardFileManager(diagnosticListener, locale, charset))

  override def isSupportedOption(option: String): Int = delegate.isSupportedOption(option)
  override def run(in: InputStream, out: OutputStream, err: OutputStream, arguments: String*): Int = delegate.run(in, out, err, arguments*)
  override def getSourceVersions: java.util.Set[SourceVersion] = delegate.getSourceVersions
}
