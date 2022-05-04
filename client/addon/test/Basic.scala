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

object AddonBasic extends UseCase("AddonBasic") 
  with TourneySvc with LicenseSvc with AuthenticateSvc with WrapperSvc
{
  
  def render(testCase:String = "", testOption:String = "", reload:Boolean = false) = {}

  def testHello(text: String) = s"Hello ${text}!"

  def testDate(testDate: String): String = {
    import java.time._
    import java.time.format.DateTimeFormatter

    val sDate = testDate.toInt

    val year  = sDate / 10000
    val month = (sDate / 100) % 100
    val day   = sDate % 100

    val sourceDate = LocalDate.of(year, month, day)            // Source Date
    val newDate    = sourceDate.plusDays(10)                   // Adding 10 days to source date.
    val formatter  = DateTimeFormatter.ofPattern("yyyy-MM-dd") // Setting date format
    newDate.format(formatter)
  }

}