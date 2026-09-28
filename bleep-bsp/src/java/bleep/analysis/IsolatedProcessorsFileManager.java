package bleep.analysis;

import java.io.File;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import javax.tools.FileObject;
import javax.tools.ForwardingJavaFileManager;
import javax.tools.JavaFileObject;
import javax.tools.StandardJavaFileManager;
import javax.tools.StandardLocation;

/**
 * A file manager which loads annotation processors and javac plugins apart from the classes of
 * whatever runs javac. See {@code IsolatedProcessorsJavaCompiler}. In Java because Scala cannot
 * override the three {@code getJavaFileObjects} varargs overloads, which erase alike.
 */
public final class IsolatedProcessorsFileManager
    extends ForwardingJavaFileManager<StandardJavaFileManager> implements StandardJavaFileManager {

  public IsolatedProcessorsFileManager(StandardJavaFileManager delegate) {
    super(delegate);
  }

  /**
   * Where javac loads processors and plugins from: the processor path, or the class path when there
   * is none. The parent is the platform class loader, which has the JDK, jdk.compiler included, and
   * nothing else. javac closes the loader when it is done with the processors.
   */
  @Override
  public ClassLoader getClassLoader(Location location) {
    if (location == StandardLocation.ANNOTATION_PROCESSOR_PATH
        || location == StandardLocation.CLASS_PATH) {
      List<URL> urls = new ArrayList<>();
      Iterable<? extends Path> paths = fileManager.getLocationAsPaths(location);
      if (paths != null) {
        for (Path path : paths) {
          try {
            urls.add(path.toUri().toURL());
          } catch (MalformedURLException e) {
            throw new UncheckedIOException(e);
          }
        }
      }
      return new URLClassLoader(urls.toArray(new URL[0]), ClassLoader.getPlatformClassLoader());
    }
    return super.getClassLoader(location);
  }

  @Override
  public Iterable<? extends JavaFileObject> getJavaFileObjectsFromFiles(
      Iterable<? extends File> files) {
    return fileManager.getJavaFileObjectsFromFiles(files);
  }

  @Override
  public Iterable<? extends JavaFileObject> getJavaFileObjectsFromPaths(
      Collection<? extends Path> paths) {
    return fileManager.getJavaFileObjectsFromPaths(paths);
  }

  @Override
  public Iterable<? extends JavaFileObject> getJavaFileObjects(File... files) {
    return fileManager.getJavaFileObjects(files);
  }

  @Override
  public Iterable<? extends JavaFileObject> getJavaFileObjects(Path... paths) {
    return fileManager.getJavaFileObjects(paths);
  }

  @Override
  public Iterable<? extends JavaFileObject> getJavaFileObjectsFromStrings(Iterable<String> names) {
    return fileManager.getJavaFileObjectsFromStrings(names);
  }

  @Override
  public Iterable<? extends JavaFileObject> getJavaFileObjects(String... names) {
    return fileManager.getJavaFileObjects(names);
  }

  @Override
  public void setLocation(Location location, Iterable<? extends File> files) throws IOException {
    fileManager.setLocation(location, files);
  }

  @Override
  public void setLocationFromPaths(Location location, Collection<? extends Path> paths)
      throws IOException {
    fileManager.setLocationFromPaths(location, paths);
  }

  @Override
  public void setLocationForModule(
      Location location, String moduleName, Collection<? extends Path> paths) throws IOException {
    fileManager.setLocationForModule(location, moduleName, paths);
  }

  @Override
  public Iterable<? extends File> getLocation(Location location) {
    return fileManager.getLocation(location);
  }

  @Override
  public Iterable<? extends Path> getLocationAsPaths(Location location) {
    return fileManager.getLocationAsPaths(location);
  }

  @Override
  public Path asPath(FileObject file) {
    return fileManager.asPath(file);
  }

  @Override
  public void setPathFactory(PathFactory f) {
    fileManager.setPathFactory(f);
  }
}
