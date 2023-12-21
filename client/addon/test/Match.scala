package addon.test

import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs._

import cats.data.EitherT
import cats.implicits._ 

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


object AddonMatch extends UseCase("AddonMatch") 
  with TourneySvc with LicenseSvc with AuthenticateSvc with WrapperSvc
{
  def render(testCase:String = "", testOption:String = "", reload:Boolean = false) = {}

  def execTest(number: Int, toId: Long, coId: Long, phase: Int, game: Int, param: String)= {
    number match {
      case 0 => test_0(toId, coId, phase, game, param)
      case 1 => test_1(toId, coId, phase, game, param)
      case _ => println(s"Invalid match test number ${number}"); Future(false)
    }
  }

  // Test 0 - resetMatch
  def test_0(toId: Long, coId: Long, phase: Int, game: Int, param: String): Future[Boolean]  = {
    
    AddonMain.setOutput(s"START Test Reset Match: toId->${toId} coId->${coId} phase->${phase} game->${game} param->${param}")
    
    (for {
      valid   <- EitherT(AddonMain.setLoginLoad(toId))
      result  <- EitherT(resetMatch(coId, phase, game))
    } yield { (valid, result) }).value.map {
      case Left(err)   => AddonMain.addOutput(s"ERROR Test Reset Match ${err.toString}"); false
      case Right(res)  => {
        AddonMain.addOutput(s"Affected games: ${res._2.mkString(":")}") 
        AddonMain.addOutput(s"SUCCESS Test Delete Match")
        true
      }
    }
  }  

  // Test 1 - getMatch
  def test_1(toId: Long, coId: Long, phase: Int, game: Int, param: String): Future[Boolean]  = {
    
    AddonMain.setOutput(s"START Test getMatch: toId->${toId} coId->${coId} phase->${phase} game->${game} param->${param}")
    
    (for {
      valid   <- EitherT(AddonMain.setLoginLoad(toId))
      result  <- EitherT(getMatch(toId, coId, phase, game))
    } yield { (valid, result) }).value.map {
      case Left(err)   => AddonMain.addOutput(s"ERROR Test getMatch ${err.toString}"); false
      case Right(res)  => {
        AddonMain.addOutput(s"RESULT: ${res._2.toString}") 
        AddonMain.addOutput(s"SUCCESS Test getMatch")
        true
      }
    }
  } 



}