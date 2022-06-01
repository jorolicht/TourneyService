package scalajs.usecase.organize

// Start TestCases
// http://localhost:9000/start?ucName=TestMain&ucParam=OrganizeCompetitionView

import scala.concurrent._
import scala.util.{Success, Failure }
import scala.util.matching

import scala.scalajs._
import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"
import org.scalajs.dom                   // from "org.scala-js" %%% "scalajs-dom" % "0.9.3"
import upickle.default._

import shared.model._
import shared.utils.Constants._ 
import shared.utils.Validation._ 
import shared.utils.Routines._
import shared.utils._

import scalajs.usecase.dialog.{ DlgBox, DlgSpinner, DlgInfo }
import scalajs.usecase.component._
import scalajs.service._
import scalajs._


// ***
// Organize Competition Draw
// ***
@JSExportTopLevel("OrganizeCompetitionView")
object OrganizeCompetitionView extends UseCase("OrganizeCompetitionView")  
  with TourneySvc
{

  def render(param: String = "", ucInfo: String = "", reload: Boolean=false) = {
    OrganizeCompetitionTab.render("View")
  }

  // setFrame for a competition, coId != 0 and coPhId != 0
  def setFrame(coId: Long, coPhId: Int, reload: Boolean)(implicit trny: Tourney): Unit = {
  }

  // set content for competition phase for competition
  def setContent(coId: Long, coPhId: Int)(implicit trny: Tourney) = {
  }  
  
}