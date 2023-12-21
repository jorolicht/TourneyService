package scalajs.usecase.home

import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs._

import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"
import org.scalajs.dom.ext._             // import stmt sequence is important
import org.scalajs.dom                   // from "org.scala-js" %%% "scalajs-dom" % "0.9.3"

import org.scalajs.dom.raw.{ Event, HTMLInputElement } // for ScalaJs bindings

//import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.concurrent._
import scala.util.{Success, Failure}

import shared.model.{ TournBase, Player }
import shared.utils._

import scalajs.usecase.component._
import scalajs.service._
import scalajs._

@JSExportTopLevel("HomeSearch")
object HomeSearch 
  extends UseCase("HomeSearch") with TourneySvc  
{
  def render(ucParam: String = "", ucInfo: String = "", reload: Boolean=false) = {
    val date = new js.Date
    val year = date.getFullYear().toInt
    setMainContent(clientviews.home.html.Search(year-1, year+2, year, AppEnv.msgs).toString)     
  }


  @JSExport
  def onsubmit(): Unit = doSearch(getInput(gE(uc("string"))), getInput(gE(uc("typ")), 0), getInput(gE(uc("year")), 1970))
  
  @JSExport
  def onchange(): Unit = doSearch(getInput(gE(uc("string"))), getInput(gE(uc("typ")), 0), getInput(gE(uc("year")), 1970))

  /** doSearch - searches for tourneys (searchs tring) lists results
   *  
   */
  def doSearch(sString: String, sTyp: Int, sYear: Int): Unit = { 
    import scalajs.usecase.dialog._ 
    findTournBases(sString, sTyp, sYear).map { 
      case Left(err)    => DlgInfo.show(getMsg("dlg.hdg"), getError(err), "danger")
      case Right(trnys) => setHtml(gUE("Result"), clientviews.home.html.ListTourney(trnys, AppEnv.msgs).toString)      
    } 
  }

  // --- 
  // click on one entry of tourney list
  // --- 
  @JSExport
  def onclickSelect(elem: dom.raw.HTMLElement, id: String): Unit = {
    App.loadRemoteTourney(id.toLong).map { toi => App.execUseCase("InfoSchedule") }
  } 

}  