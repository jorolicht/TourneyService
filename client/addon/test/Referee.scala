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
import shared.model.CompPhase._
import shared.utils.Constants._

import scalajs.usecase.component._
import scalajs.service._
import scalajs.{ App, AppEnv }


object AddonReferee extends UseCase("AddonReferee") 
  with TourneySvc with LicenseSvc with AuthenticateSvc with WrapperSvc
{
  def render(testCase:String = "", testOption:String = "", reload:Boolean = false) = {}

  def execTest(number: Int, toId: Long, coId: Long, phase: Int, param: String)= {
    number match {
      case 0 => test_0(toId, coId, phase, param)
      // case 1 => test_1(param)
      // case 2 => test_2(param)
      // case 3 => test_3(param)
    }
  }

  // Test 0 - schiri zettel / referee notes
  def test_0(toId: Long, coId: Long, phase: Int, param: String): Future[Boolean]  = {
    AddonMain.setOutput(s"START Test Schiri/Referee Notes: toId->${toId} coId->${coId} phase->${phase} param->${param}")
    AddonMain.setLoginLoad(toId).map {
      case false => false
      case true  => {
        App.execUseCase("OrganizeCompetitionReferee", "", "")
        AddonMain.addOutput(s"SUCCESS Test Schiri/Referee Notes: ")
        true
      }
    }
  }  




}