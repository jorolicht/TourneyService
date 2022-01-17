package tasks

import javax.inject.Inject
import javax.inject.Named

import play.api.inject.{SimpleModule, _}

import akka.actor.ActorRef
import akka.actor.ActorSystem

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

import com.mohiva.play.silhouette.api.util.Clock
import models.services.AuthTokenService
import play.api.Logging

class AuthTokenCleanerTask @Inject()(service: AuthTokenService, clock: Clock, actorSystem: ActorSystem)
  (implicit ec: ExecutionContext) extends Logging {
  actorSystem.scheduler.schedule(initialDelay = 10.minutes, interval = 8.hours) {
    process()
  }

  def process(): Unit = {
    val start = clock.now.getMillis
    val msg   = new StringBuffer("\n")
    msg.append("=================================\n")
    msg.append("Start to cleanup auth tokens\n")
    msg.append("=================================\n")
    service.clean.map { deleted =>
        val seconds = (clock.now.getMillis - start) / 1000
        msg.append("Total of %s auth tokens(s) were deleted in %s seconds".format(deleted.length, seconds))
        msg.append("\n")
        msg.append("=================================\n")
        logger.info(msg.toString)
      }
      .recover { case e => {
          msg.append("Couldn't cleanup auth tokens because of unexpected error\n")
          msg.append("=================================\n")
          logger.error(msg.toString, e)
      }}
  }
}

class AuthTokenCleanerTaskModule extends SimpleModule(bind[AuthTokenCleanerTask].toSelf.eagerly())