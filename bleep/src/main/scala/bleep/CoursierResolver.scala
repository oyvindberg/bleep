package bleep

import coursier.cache.{ArtifactError, FileCache}
import coursier.error.{FetchError, ResolutionError}
import coursier.util.Task
import coursier.{Classifier, Dependency, Fetch, MavenRepository}

import java.net.URI
import scala.collection.immutable.SortedSet
import scala.concurrent.{ExecutionContext, Future}

class CoursierResolver(ec: ExecutionContext, downloadSources: Boolean) {
  val fileCache = FileCache[Task]()

  def apply(deps: SortedSet[Dependency], repositories: Seq[URI]): Future[Fetch.Result] = {
    def go(remainingAttempts: Int): Future[Fetch.Result] = {
      val newClassifiers = if (downloadSources) List(Classifier.sources) else Nil

      Fetch[Task](fileCache)
        .withDependencies(deps.toList)
        .addRepositories(repositories.map(uri => MavenRepository(uri.toString)): _*)
        .withMainArtifacts(true)
        .addClassifiers(newClassifiers: _*)
        .ioResult
        .future()(ec)
        .recoverWith {
          case x: ResolutionError.CantDownloadModule if remainingAttempts > 0 && x.perRepositoryErrors.exists(_.contains("concurrent download")) =>
            go(remainingAttempts - 1)
          case x: FetchError.DownloadingArtifacts if remainingAttempts > 0 && x.errors.exists { case (_, artifactError) =>
                artifactError.isInstanceOf[ArtifactError.Recoverable]
              } =>
            go(remainingAttempts - 1)
        }(ec)
    }

    go(remainingAttempts = 3)
  }

}
