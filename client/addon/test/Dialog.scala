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

import scalajs.usecase.component._
import scalajs.service._
import scalajs.{ App, AppEnv }

object AddonDialog extends UseCase("AddonDialog") 
  with TourneySvc with AuthenticateSvc with WrapperSvc
{
  
  def render(testCase:String = "", testOption:String = "", reload:Boolean = false) = {}

  def execTest(number: Int, toId: Long, plId: Long, param: String): Future[Boolean] = {

    number match {
      case 0 => test_0(toId, param)
      case 1 => test_1(toId, plId).flatMap(identity)
      case 2 => test_2(param)
      case _ => AddonMain.setOutput(s"ERROR invalid test dialog number ${number}"); Future(false)
    }
  }


  // Test 0 - input dialog
  def test_0(toId: Long, param: String): Future[Boolean]  = {
    import scalajs.usecase.dialog.DlgInputTxt

    def check(input: String): Either[String, Boolean] = {
      if      (input == "")    Left("kein Input") 
      else if (input == "XXX") Left("Triple XXX not allowed")
      else                     Right(true)
    }

    AddonMain.setOutput(s"START Test Dialog 0: param->${param}")
    DlgInputTxt.show(param, "TestBTN", "e.g. Vorrunde", "CCLOSE", "SSTART", check) map { 
      case Left(err)    => AddonMain.addOutput(s"ERROR Test Dialog 0: no input ${err}"); false
      case Right(value) => AddonMain.addOutput(s"SUCCESS Test Dialog 0: input->${value}"); true
    }
  }   

  // Test 1 - input dialog
  def test_1(toId: Long, plId: Long): Future[Future[Boolean]]  = {
    import scalajs.usecase.dialog.DlgCardPlayer
    import cats.data.EitherT
    import cats.implicits._ 
    
    AddonMain.setOutput(s"START Test Dialog 1 player: plId->${plId}")
    (for {
      pw        <- EitherT(authReset("", "ttcdemo/FED89BFA1BF899D590B5", true ))
      coValid   <- EitherT(authBasicContext("","ttcdemo/FED89BFA1BF899D590B5", pw._2))
      result    <- EitherT(App.loadRemoteTourney(toId))
      player    <- EitherT(DlgCardPlayer.show(plId, App.tourney))
    } yield { (result, pw, player) }).value.map {
      case Left(err)    => AddonMain.addOutput(s"ERROR Test Dialog 1: tourney ${toId} failed with: ${err.msgCode}"); Future(false)
      case Right(res)   => {
        AddonMain.addOutput(s"SUCCESS Test Dialog 1: player->${res._3}")
        Future(false)
      }
    }
  }  

  // Test 2 - File Input Dialog Test
  // http://localhost:9000/start?ucName=HomeMain&ucParam=Debug&ucInfo=test%20%2Ds%20dialog%20%2Dn%202%20%2D%2Dparam%20gen 
  def test_2(param: String):Future[Boolean] = {
    import scalajs.usecase.dialog.DlgBoxFile

    val test = s"START Test Dialog 2 Input File -> ${param}"
    AddonMain.setOutput(test)

    DlgBoxFile.show("DlgBoxFile_Header", "DlgBoxFile_Body", true).map { 
      case Left(err) => {}
      case Right(content) => println(s"CONTENT: ${content}") 
    } 

    Future(true)
  }



}