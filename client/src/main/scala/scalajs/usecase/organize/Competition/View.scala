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

import org.scalajs.dom                   // from "org.scala-js" %%% "scalajs-dom" % "0.9.3"
import org.scalajs.dom.raw.HTMLElement

import shared.model._
import shared.model.CompPhase._
import shared.utils._

import scalajs.usecase.component._
import scalajs.service._

import scalajs._

// ***
// Organize Competition Draw
// ***
@JSExportTopLevel("OrganizeCompetitionView")
object OrganizeCompetitionView extends UseCase("OrganizeCompetitionView")  
  with TourneySvc with ViewServices
{

  def render(param: String = "", ucInfo: String = "", update: Boolean=false) = {
    OrganizeCompetitionTab.render("View")
  }

  // set view page for a competition phase(round), coId != 0 and coPhId != 0
  def setPage(coph: CompPhase): Unit = {
    val contentElement = gE(s"ViewContent_${coph.coId}_${coph.coPhId}")
    if (contentElement.innerHTML == "") {
      debug("setPage", s"View init: coId: ${coph.coId} coPhId: ${coph.coPhId}")
      coph.getTyp match {
        case CompPhaseTyp.GR => setHtml(contentElement, clientviews.component.html.ViewGroups(coph.coId, coph.coPhId, coph.groups))
        case CompPhaseTyp.RR => setHtml(contentElement, clientviews.component.html.ViewRR(coph.coId, coph.coPhId, coph.groups(0)))
        case CompPhaseTyp.KO => setHtml(contentElement, clientviews.component.html.ViewKo(coph.coId, coph.coPhId, coph.ko))
        case CompPhaseTyp.SW => ???
        case _               => error("setPage", s"View init invalid typ, coId: ${coph.coId} coPhId: ${coph.coPhId}")
      }
    }  

    debug("setPage", s"View: coId: ${coph.coId} coPhId: ${coph.coPhId}")
    // update view page
    coph.getTyp match {
      case CompPhaseTyp.KO => showKoResult(coph.coId, coph.coPhId, coph.ko)      
      case CompPhaseTyp.GR => for (grp <- coph.groups ) showGrResult(coph.coId, coph.coPhId, grp)
      case CompPhaseTyp.RR => showRrResult(coph.coId, coph.coPhId, coph.groups(0))
      case CompPhaseTyp.SW => ???
      case _               => error("setPage", s"View update invalid typ, coId: ${coph.coId} coPhId: ${coph.coPhId}")
    }
  }


}
