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

object AddonDialog extends UseCase("AddonDialog") 
  with TourneySvc with AuthenticateSvc with WrapperSvc
{
  
  def render(testCase:String = "", testOption:String = "", reload:Boolean = false) = {}

  def execTest(number: Int, param: String)= {
    number match {
      case 0 => test_0(param)
      case _ => println("Invalid DIALOG test option")
    }
  }

  // Test 0 - input dialog
  def test_0(param: String="Dialog Input") = {
    import scalajs.usecase.dialog.DlgInputTxt

    def check(input: String): Either[String, Boolean] = {
      if      (input == "")    Left("kein Input") 
      else if (input == "XXX") Left("Triple XXX not allowed")
      else                     Right(true)
    }    
    
    
    DlgInputTxt.show(param, "TestBTN", "e.g. Vorrunde", "CCLOSE", "SSTART", check) map { 
      case Left(err)    => println(s"NO INPUT: ${err}")
      case Right(value) => println(s"INPUT: ${value}")
    }
  }    

}