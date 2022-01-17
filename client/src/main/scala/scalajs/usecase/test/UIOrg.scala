package scalajs.usecase.home

// Start TestCases
// --------------------------------------------------------------------------------------------------
// DlgCardPlayer:       http://localhost:9000/start?ucName=TestMain&ucParam=DlgCardPlayer&ucInfo=show
// OrganizeCompetition: http://localhost:9000/start?ucName=TestMain&ucParam=OrganizeCompetition

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
import scalajs.usecase.dialog._
import scalajs.usecase.component._
import scalajs.service._
import scalajs.{ App, AppEnv }


object UIOrg extends UseCase("UIOrg") 
  with TourneySvc with LicenseSvc with AuthenticateSvc with WrapperSvc
{

  def render(testCase: String = "", testOption: String = "", reload: Boolean=false) = {} 

  // testCompetiton
  def testComp(testCase: String, testOption: String="") = {
    val toId = testOption.toLongOption.getOrElse(161L)
    authBasic("robert.lichtengger@icloud.com", "", "Si5d4H").map {
      case Left(err)  => error("authBasic", s"${err.encode}")
      case Right(sessionCtx) => {
        info("authBasic", s"successfull ctx: ${sessionCtx}")
        AppEnv.initContext(sessionCtx)
        App.loadRemoteTourney(toId).map {
          case Left(err)     => error("loadRemoteTourney", s"${err.encode}")
          case Right(result) => {
            info("loadRemoteTourney", App.tourney.toString)
            App.execUseCase("OrganizeCompetition", "", "")
          } 
        }
      }  
    }
  }


  // testCompDraw
  def testCompDraw(testCase: String, testOption: String) = {
    val toId = testOption.toLongOption.getOrElse(161L)
    authBasic("robert.lichtengger@icloud.com", "", "Si5d4H").map {
      case Left(err)  => error("authBasic", s"${err.encode}")
      case Right(sessionCtx) => {
        info("authBasic", s"successfull ctx: ${sessionCtx}")
        AppEnv.initContext(sessionCtx)
        App.loadRemoteTourney(toId) map {
          case Left(err)     => error("loadRemoteTourney", s"${err.encode}")
          case Right(result) => {
            info("loadRemoteTourney", App.tourney.toString)
            val (id, preId, winId, looId, name, coId, winSets) = (1, 0, 0, 0, "Vorrunde", 1L, 3)
 
            App.execUseCase("OrganizeCompetitionDraw", "", "")
          } 
        }
      }  
    }
  }


  // testCompView
  def testCompView(testCase: String, testOption: String) = {
    authBasic("robert.lichtengger@icloud.com", "", "Si5d4H").map {
      case Left(err)  => error("authBasic", s"${err.encode}")
      case Right(sessionCtx) => {
        info("authBasic", s"successfull ctx: ${sessionCtx}")
        AppEnv.initContext(sessionCtx)
        App.loadRemoteTourney(159) map {
          case Left(err)     => error("loadRemoteTourney", s"${err.encode}")
          case Right(result) => {
            info("loadRemoteTourney", App.tourney.toString)
            App.execUseCase("OrganizeCompetitionView", "", "")
          } 
        }
      }  
    }
  }

  // testCompInput
  def testCompInput(testCase: String, testOption: String) = {
    authBasic("robert.lichtengger@icloud.com", "", "Si5d4H").map {
      case Left(err)  => error("authBasic", s"${err.encode}")
      case Right(sessionCtx) => {
        info("authBasic", s"successfull ctx: ${sessionCtx}")
        AppEnv.initContext(sessionCtx)
        App.loadRemoteTourney(159) map {
          case Left(err)     => error("loadRemoteTourney", s"${err.encode}")
          case Right(result) => {
            info("loadRemoteTourney", App.tourney.toString)
            App.execUseCase("OrganizeCompetitionInput", "", "")
          } 
        }
      }  
    }
  }


  // testTrny - OrganizeTourney
  def testTrny(testCase: String, testOption: String) = {
    authBasic("robert.lichtengger@icloud.com", "", "Si5d4H").map {
      case Left(err)  => error("authBasic", s"${err.encode}")
      case Right(sessionCtx) => {
        info("authBasic", s"successfull ctx: ${sessionCtx}")
        AppEnv.initContext(sessionCtx)
        App.execUseCase("OrganizeTourney", "", "")
      }  
    }
  }

}
