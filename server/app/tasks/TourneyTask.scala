package tasks

import javax.inject.Inject
import javax.inject.Named

import play.api.inject.{SimpleModule, _}
import play.api.{ Configuration, Logging }
import play.api.i18n._

import akka.actor.ActorRef
import akka.actor.ActorSystem

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

import tourn.services.TourneyService
import tourn.utils.Helper._


class TourneyTask @Inject()(tsv: TourneyService, actorSystem: ActorSystem)
                           (implicit ec: ExecutionContext, config: Configuration)
extends Logging {

  val tourneyInterval = Duration(getConfig("server.save.intervall", 900L), SECONDS) 
  logger.info(s"Starting with interval: ${tourneyInterval}")

  actorSystem.scheduler.schedule(initialDelay = 1.minutes, interval = tourneyInterval) {
    process(tsv)
  }

  def process(tournSvc: TourneyService): Unit = {
    tournSvc.clean()
  }
}

// enabled through application config
// play.modules.enabled += "tasks.TourneyTaskModule"
class TourneyTaskModule extends SimpleModule(bind[TourneyTask].toSelf.eagerly())
