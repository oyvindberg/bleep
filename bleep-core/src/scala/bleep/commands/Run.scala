package bleep
package commands

import bleep.bsp.BspCommandFailed
import bleep.internal.{bleepLoggers, jvmRunCommand, DoSourceGen, TransitiveProjects}
import bloop.rifle.BuildServer
import ch.epfl.scala.bsp4j
import org.eclipse.lsp4j.jsonrpc.ResponseErrorException
import ryddig.Throwables

import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try}

/** @param raw
  *   use raw stdin and stdout and avoid the logger
  */
case class Run(
    project: model.CrossProjectName,
    maybeOverriddenMain: Option[String],
    args: List[String],
    raw: Boolean,
    watch: Boolean
) extends BleepCommandRemote(watch) {
  override def watchableProjects(started: Started): TransitiveProjects =
    TransitiveProjects(started.build, Array(project))

  override def runWithServer(started: Started, bloop: BuildServer): Either[BleepException, Unit] = {
    val maybeSpecifiedMain: Option[String] =
      maybeOverriddenMain.orElse(started.build.explodedProjects(project).platform.flatMap(_.mainClass))

    val maybeMain: Either[BleepException, String] =
      maybeSpecifiedMain match {
        case Some(mainClass) => Right(mainClass)
        case None =>
          started.logger.info("No main class specified in build or command line. discovering...")
          discoverMain(started.logger, bloop, BleepCommandRemote.buildTarget(started.buildPaths, project))
      }

    maybeMain.flatMap { main =>
      started.build.explodedProjects(project).platform.flatMap(_.name) match {
        // we could definitely run js/native projects in "raw" mode as well, it just needs to be implemented
        case Some(model.PlatformId.Jvm) if raw => rawRun(started, bloop, main)
        case _                                 => bspRun(started, bloop, main)
      }
    }
  }

  def rawRun(started: Started, bloop: BuildServer, main: String): Either[BleepException, Unit] =
    Compile(watch = false, Array(project)).runWithServer(started, bloop).map { case () =>
      cli(
        "run",
        started.pre.buildPaths.cwd,
        jvmRunCommand(started.bloopProject(project), started.resolvedJvm, project, Some(main), args).orThrow,
        logger = started.logger,
        out = cli.Out.Raw,
        in = cli.In.Attach,
        env = sys.env.toList
      ).discard()
    }

  def bspRun(started: Started, bloop: BuildServer, main: String): Either[BleepException, Unit] =
    DoSourceGen(started, bloop, watchableProjects(started)).flatMap { case () =>
      val params = new bsp4j.RunParams(BleepCommandRemote.buildTarget(started.buildPaths, project))
      val mainClass = new bsp4j.ScalaMainClass(main, args.asJava, List(s"-Duser.dir=${started.pre.buildPaths.cwd}").asJava)

      val env = sys.env.updated(bleepLoggers.CallerProcessAcceptsJsonEvents, "true")
      mainClass.setEnvironmentVariables(env.map { case (k, v) => s"$k=$v" }.toList.sorted.asJava)
      params.setData(mainClass)
      params.setDataKind("scala-main-class")
      started.logger.debug(params.toString)

      def failed(reason: BspCommandFailed.Reason) =
        Left(new BspCommandFailed("Run", Array(project), reason))

      Try(bloop.buildTargetRun(params).get().getStatusCode) match {
        case Success(bsp4j.StatusCode.OK) => Right(started.logger.info(s"Run $main succeeded"))
        case Success(errorCode)           => failed(BspCommandFailed.StatusCode(errorCode))
        case Failure(th) =>
          Throwables.tryExtract(classOf[ResponseErrorException])(th) match {
            case Some(roe) => failed(BspCommandFailed.FoundResponseError(roe.getResponseError))
            case None      => failed(BspCommandFailed.FailedWithException(th))
          }
      }
    }
}
