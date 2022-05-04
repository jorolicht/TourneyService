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
import shared.model.tabletennis._

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

}