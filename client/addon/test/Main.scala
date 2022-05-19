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


@JSExportTopLevel("Start")
object AddonMain extends UseCase("AddonMain") 
  with TourneySvc with LicenseSvc with AuthenticateSvc with WrapperSvc
{
  
  def render(testCase:String = "", testOption:String = "", reload:Boolean = false) = {}

  // Commands
  @JSExport def save()                            = AddonCmds.save()
  @JSExport def sync()                            = AddonCmds.sync()
  @JSExport def load(toId: String)                = AddonCmds.load(toId)

  // Basic tests
  @JSExport def testBasicHello(text: String)      = AddonBasic.testHello(text)
  @JSExport def testBasicDate(testDate: String)   = AddonBasic.testDate(testDate)
  

  // Competition tests
  @JSExport def testCompEncode(toId: String)      = AddonComp.testEncode(toId)
  
  // Tourney tests
  @JSExport def testTourneyLoad(toId: Long)       = AddonTourney.testLoad(toId)

  // set debug Level
  @JSExport def setDebug(level: String) = AppEnv.setDebugLevel(level)
  
  // get debug Level
  @JSExport def getDebug() = println(s"Debug Level: ${AppEnv.getDebugLevel.getOrElse("not set")}") 
  



}

