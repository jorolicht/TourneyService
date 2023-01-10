package addon.test

import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs._

import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"
import org.scalajs.dom.ext._             // import stmt sequence is important
import org.scalajs.dom                   // from "org.scala-js" %%% "scalajs-dom" % "0.9.3"

import org.scalajs.dom.raw.{ Event, HTMLInputElement } // for ScalaJs bindings

import upickle.default._
import upickle.default.{ReadWriter => RW, macroRW}


//import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.concurrent._
import scala.util.{Success, Failure }
import scala.util.matching

// tourney service imports
import shared.utils._
import shared.utils.Routines._

import shared.model._
import shared.utils.Constants._

import scalajs.usecase.component.BasicHtml._
import scalajs.usecase.component._
import scalajs.service._
import scalajs.{ App, AppEnv }

object AddonCmds extends UseCase("AddonCmds") 
  with TourneySvc with LicenseSvc with AuthenticateSvc with WrapperSvc
{
  def render(testCase:String = "", testOption:String = "", reload:Boolean = false) = {}

  def save() = {
    val toId = App.tourney.id
    if (toId == 0) {
      info("save", s"ERROR: saving tourney ${toId} not possible")
    } else {
      saveTourney(toId) map {
        case Left(err)  => info("save", s"ERROR: saving tourney ${toId} failed with: ${err.msgCode}")
        case Right(res) => info("save", s"SUCCESS: tourney toId: ${toId} saved")
      }  
    }
  }

  def sync() = {
    val toId = App.tourney.id
    if (toId == 0) {
      info("sync", s"ERROR: sync tourney ${toId} not possible")
    } else {
      syncTourney(toId) map {
        case Left(err)  => error("sync", s"sync tourney ${toId} failed with: ${err.msgCode}")
        case Right(res) => info("save", s"SUCCESS: tourney toId: ${toId} synced")
      }  
    }
  }

  def load(toIdStr: String) = {
    import cats.data.EitherT
    import cats.implicits._ 
    val toId = toIdStr.toLongOption.getOrElse(0L)

    if (toId == 0) {
      error("load", s"loading tourney ${toId} not possible")
    } else {
      (for {
        pw        <- EitherT(authReset("", "ttcdemo/FED89BFA1BF899D590B5", true ))
        coValid   <- EitherT(authBasicContext("","ttcdemo/FED89BFA1BF899D590B5", pw))
        result    <- EitherT(App.loadRemoteTourney(toId))
      } yield { (result, pw) }).value.map {
        case Left(err)    => error("load", s"error message: ${err}")
        case Right(res)   => {
          println(s"TOURNEY: \n ${App.tourney.toString()}")
        } 
      } 
    }
  }

  def showTourney() = {
    println(App.tourney.toString)
  }


  def showCompPhase() = {    
    val (coId, coPhId) = dom.window.prompt("Enter coId and coPhId separated by comma:") match {
      case s"${coId},${coPhId}" => (coId.toLongOption.getOrElse(0L), coPhId.toIntOption.getOrElse(0))
      case _                    => (0L,0)
    }
    if (coId != 0) println(App.tourney.cophs((coId, coPhId)).toString) else println("Input error")
  }

}