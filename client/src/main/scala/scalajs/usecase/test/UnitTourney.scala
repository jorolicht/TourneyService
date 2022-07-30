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

import scalajs.usecase.component.BasicHtml._
import scalajs.usecase.dialog._
import scalajs.usecase.component._
import scalajs.service._
import scalajs.{ App, AppEnv }


object UnitTourney extends UseCase("UnitTourney") 
  with TourneySvc with LicenseSvc with AuthenticateSvc with WrapperSvc
{

  def render(testCase: String = "", testOption: String = "", reload: Boolean=false) = {} 

  // testAddTournCTT
  // Start with Homepage http://localhost:9000/ Test.start("addTournCTT", "20221007")
  // def testAddTournCTT(testCase: String, testOption: String) = {
  //   authBasic("robert.lichtengger@icloud.com", "", "Si5d4H").map {
  //     case Left(err)  => error("authBasic", s"${err.encode}")
  //     case Right(sessionCtx) => {
  //       info("authBasic", s"successfull ctx: ${sessionCtx}")
  //       AppEnv.initContext(sessionCtx)
  //       addTournCTT(UnitCTT.getDemo(testOption.toInt), 0, 0).map {
  //         case Left(err)     => error("addTournCTT", s"${err.encode}")
  //         case Right(trny) => {
  //           info("addTournCTT", s"${trny.toString}")
  //           App.setLocalTourney(trny)
  //           App.execUseCase("OrganizeCompetition", "", "")
  //         }  
  //       }
  //     }  
  //   }
  // }

  // testAddTournCTT
  // Start with Homepage http://localhost:9000/ Test.start("addTourneyCTT", "20220107")
  def testAddTournCTT(testCase: String, testOption: String) = {
    import cats.data.EitherT
    import cats.implicits._    
    (for {
      result  <- EitherT(authBasicContext("robert.lichtengger@icloud.com", "", "Si5d4H"))
      tourney <- EitherT(addTournCTT(UnitCTT.getDemo(testOption.toInt), 0, 0))
    } yield { tourney }).value.map {
      case Left(err)   => error("addTournCTT", s"Error: ${err.encode}")
      case Right(trny) => {
        info("addTournCTT", s"${trny.toString}")
        App.setLocalTourney(trny)
        App.execUseCase("OrganizeCompetition", "", "")      
      }
    }  
  } 

  // def testAddTournCTT(testCase: String, testOption: String) = {
  //   addTournCTT(UnitCTT.getDemo(testOption.toInt), 0, 0).map {
  //     case Left(err)   => error("addTournCTT", s"Error: ${err.encode}")
  //     case Right(trny) => {
  //       info("addTournCTT", s"${trny.toString}")
  //       App.setLocalTourney(trny)
  //       App.execUseCase("OrganizeCompetition", "", "")      
  //     }
  //   }  
  // } 





}