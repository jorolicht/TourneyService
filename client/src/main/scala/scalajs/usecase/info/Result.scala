package scalajs.usecase.info

import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs._

import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"
import org.scalajs.dom.ext._             // import stmt sequence is important
import org.scalajs.dom                   // from "org.scala-js" %%% "scalajs-dom" % "0.9.3"

import org.scalajs.dom.raw.{ Event, HTMLInputElement } // for ScalaJs bindings

import upickle.default._

//import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

// tourney service client imports
import shared.model._
import shared.model.CompPhase._
import shared.utils._
import shared.utils.Constants._ 

import scalajs.usecase.component._
import scalajs.service._
import scalajs.App

// ***
// User Info View Competitions
// ***    
@JSExportTopLevel("InfoResult")
object InfoResult extends UseCase("InfoResult")
  with TourneySvc with ViewServices
{

  def render(param: String = "", ucInfo: String = "", reload: Boolean=false) = {
    showTourneyResults(App.tourney)
  }
  
  override def update(param: String = "", upd: UpdateTrigger = UpdateTrigger("", 0L)) = {
    val coId   = upd.coId
    val coPhId = upd.coPhId
    val grId   = upd.grId

    debug("update",s"for ${coId} ${coPhId} ${grId}")
    val cphase = App.tourney.cophs((coId, coPhId))
    cphase.getTyp match {
      case CompPhaseTyp.KO => showKoResult(cphase.coId, cphase.coPhId, cphase.ko) 
      case CompPhaseTyp.GR => showGrResult(cphase.coId, cphase.coPhId, cphase.groups(grId-1))
      case CompPhaseTyp.RR => showRrResult(cphase.coId, cphase.coPhId, cphase.groups(0))
      case _      => debug("update",s"error unknown competition type")
    }
  }


  // show results of tourney with all competitions
  def showTourneyResults(tourney: Tourney) = {
    // list of all configured competition phases
    debug("showTourneyResults", s"start")
    val coPhMapSeq = tourney.cophs.values.groupBy(x => x.coId).transform((key, value) => value.toList.sortBy(_.coPhId).toSeq)
    val co2NaSt = (for { (key, comp) <- tourney.comps } yield {
      key -> (comp.name, gMTyp(comp.status))
    }).toMap


    val coPhMapListSorted = Map(coPhMapSeq.toSeq.sortBy(_._1):_*)
    setMainContent(clientviews.info.html.ResultTmpl(co2NaSt, coPhMapListSorted).toString)
    debug("showTourneyResults", s"phases")
    for (coPhList <- coPhMapSeq.values; coPhase  <- coPhList) {
      // show results of every phase of every competition
      if (coPhase.getTyp == CompPhaseTyp.KO) showKoResult(coPhase.coId, coPhase.coPhId, coPhase.ko)
      if (coPhase.getTyp == CompPhaseTyp.GR) for (grp <- coPhase.groups ) showGrResult(coPhase.coId, coPhase.coPhId, grp)
      if (coPhase.getTyp == CompPhaseTyp.RR) showGrResult(coPhase.coId, coPhase.coPhId, coPhase.groups(0))
    }
  } 
}