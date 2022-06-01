package scalajs.usecase.organize

// Start TestCases in Javascript Console
// Start.testOrgCompInput("182")

import scala.concurrent._
//import scala.collection.mutable.ListBuffer
import scala.util.{Success, Failure }
import scala.util.matching


import scala.scalajs._
import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

//import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"
import org.scalajs.dom                   // from "org.scala-js" %%% "scalajs-dom" % "0.9.3"
import org.scalajs.dom.raw.HTMLElement
import org.scalajs.dom.raw.HTMLTableElement

import upickle.default._

import shared.model._
import shared.model.CompPhase._
import shared.utils._

import scalajs.usecase.component._
import scalajs.service._

import scalajs._



// ***
// Organize Competition Inaput
// ***
@JSExportTopLevel("OrganizeCompetitionInput")
object OrganizeCompetitionInput extends UseCase("OrganizeCompetitionInput")  
  with TourneySvc with DrawSvc
{

  def render(param: String = "", ucInfo: String = "", reload: Boolean=false) = {
    OrganizeCompetitionTab.render("Input")
  }


  // setInputFrame for a competition, coId != 0
  def setFrame(coId: Long, coPhId: Int, reload: Boolean)(implicit trny: Tourney): Unit = {
    debug("setFrame", s"coId: ${coId} coPhId: ${coPhId}")
    if (!exists(s"Input_${coId}_${coPhId}") | reload) {
      val elem    = getElemById_(s"InputContent_${coId}").querySelector(s"[data-coPhId='${coPhId}']")
      val size    = trny.cophs(coId, coPhId).size
      val coPhTyp = trny.cophs(coId, coPhId).coPhTyp
      debug("setFrame", s"step 1")
      coPhTyp match {
        case CPT_GR  => {
          elem.innerHTML = "input for groups"
        }  
        case CPT_KO => {
          
          val maxRnd = genKORnds(size)
          debug("setFrame", s"step 2")
          elem.innerHTML = clientviews.organize.competition.input.html.KoCard(coId, coPhId, size, maxRnd).toString
        }  
        case CPT_SW => elem.innerHTML = "input for switz-system"
        case _      => elem.innerHTML = showAlert(getMsg("invalidSection"))
      }
    }
  }


  def setContent(coId: Long, coPhId: Int) (implicit trny: Tourney) = {
    debug("setContent", s"coId: ${coId} coPhId: ${coPhId}")
    val elem    = getElemById_(s"InputContent_${coId}").querySelector(s"[data-coPhId='${coPhId}']")
    trny.cophs(coId, coPhId).coPhTyp match {
      case CPT_GR => elem.innerHTML = "input for Group-System"
      case CPT_KO => {
        val maxRnd = genKORnds(trny.cophs(coId, coPhId).size)  
        val matchMap = trny.cophs(coId,coPhId).matches.groupBy(mEntry=>mEntry.round)
        for (rnd <- maxRnd to 0 by -1) {
          val tableElem = s"InputRound_${coId}_${coPhId}_${rnd}" 
          val matches = matchMap(rnd).sortBy(mEntry => mEntry.gameNo)
          for (m <- matches) {
            val rowElem = getElemById(tableElem).asInstanceOf[HTMLTableElement].insertRow(-1)
            rowElem.innerHTML = clientviews.organize.competition.input.html.KoMatchEntry(
              SNO(m.stNoA).getName(trny.comps(coId).typ, getMsg("bye")), 
              SNO(m.stNoB).getName(trny.comps(coId).typ, getMsg("bye")), 
              m.gameNo, m.info, m.getPlayfield, m.getBallArr, 
              m.printSets, trny.cophs(coId,coPhId).noWinSets).toString
          }
        }
      }
      case CPT_SW => elem.innerHTML = "input for Switz-System"
      case _      => elem.innerHTML = showAlert(getMsg("invalidSection"))
    }
  }

  
}
