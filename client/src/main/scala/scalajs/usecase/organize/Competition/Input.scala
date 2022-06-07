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

import shared.model._
import shared.model.CompPhase._
import shared.model.tabletennis._

import shared.utils._

import scalajs.usecase.component._
import scalajs.service._

import scalajs._

// Possible match result status
// - edit     (no result)   - activ: saveButten, inactiv: deleteButton, balls,sets unlocked
// - valid    (with result) - inactiv: saveButton, activ: deleteButton, balls,sets locked (grey)
// - fix      (with result) - not shown saveButton, deleteButton, balls,sets locked (grey)

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

@JSExport 
  override def actionEvent(key: String, elem: dom.raw.HTMLElement, event: dom.Event) = {
    import shared.utils.Routines._ 
    import org.scalajs.dom.document
    
    // import org.scalajs.dom.raw.HTMLCollection
    // import org.scalajs.dom.raw.NodeList
    //debug("actionEvent", s"key: ${key} event: ${event.`type`}")
    debug("actionEvent", s"key: ${key}")
    key match {

      case "SaveMatchResult"   => {
        val game = getData(elem, "game", 0L)
        debug("actionEvent", s"save: ${game}")
      }
      
      case "DeleteMatchResult"   => {
        val game = getData(elem, "game", 0L)
        debug("actionEvent", s"delete: ${game}")      
      }

      case _                     => { 
        debug("actionEvent(error)", s"unknown key: ${key}") 
        debug("actionEvent(error)", s"event: ${event.`type`}") 
      }

    }
  }        

  // setInputFrame for a competition, coId != 0
  def setFrame(coId: Long, coPhId: Int, reload: Boolean)(implicit trny: Tourney): Unit = {
    //debug("setFrame", s"coId: ${coId} coPhId: ${coPhId}")
    if (!exists(s"Input_${coId}_${coPhId}") | reload) {
      val elem    = getElemById_(s"InputContent_${coId}").querySelector(s"[data-coPhId='${coPhId}']").asInstanceOf[HTMLElement]
      val size    = trny.cophs(coId, coPhId).size
      val coPhTyp = trny.cophs(coId, coPhId).coPhTyp
      val maxRnd  = trny.cophs(coId, coPhId).getMaxRnds
      coPhTyp match {
        case CPT_GR => setHtml(elem, clientviews.organize.competition.input.html.GroupCard(coId, coPhId, maxRnd))
        case CPT_KO => {
          setHtml(elem, clientviews.organize.competition.input.html.KoCard(coId, coPhId, maxRnd))
          var gameNo = 0
          for (rnd <- maxRnd to 0 by -1) {
            val cnt = scala.math.pow(2, rnd-1).toInt.max(1) 
            val tableElem = s"InputRound_${coId}_${coPhId}_${rnd}"
            setHtml(tableElem, "")
            for (j<-1 to cnt) {
              gameNo = gameNo + 1
              val rowElem = getElemById(tableElem).asInstanceOf[HTMLTableElement].insertRow(-1)
              setHtml(rowElem, clientviews.organize.competition.input.html.KoMatchEntry(gameNo, trny.cophs(coId,coPhId).noWinSets))
            }
          }
        }  
        case CPT_SW => elem.innerHTML = "input for switz-system"
        case _      => elem.innerHTML = showAlert(getMsg("invalidSection"))
      }
    }
  }


  def setContent(coId: Long, coPhId: Int) (implicit trny: Tourney) = {
    trny.cophs(coId, coPhId).coPhTyp match {
      case CPT_GR => {
        val matchMap = trny.cophs(coId,coPhId).matches.groupBy(mEntry=>mEntry.round)
        val maxRnd = trny.cophs(coId, coPhId).getMaxRnds
        for (rnd <- 1 to maxRnd) {
          val tableElem = s"InputRound_${coId}_${coPhId}_${rnd}"
          setHtml(tableElem, "")
          for (m <- matchMap(rnd).sortBy(mEntry => mEntry.gameNo)) {
            val rowElem = getElemById(tableElem).asInstanceOf[HTMLTableElement].insertRow(-1)
            setGrMatch(coId, coPhId, rowElem, m.asInstanceOf[MEntryGr])(trny)
          }
        } 
      }  
      case CPT_KO => { 
        for (m <- trny.cophs(coId, coPhId).matches) {
          setKoMatch(coId, coPhId, m.asInstanceOf[MEntryKo])(trny)
        }
      }
      case _ =>  {
        val elem   = getElemById_(s"InputContent_${coId}").querySelector(s"[data-coPhId='${coPhId}']") 
        elem.innerHTML = showAlert(getMsg("invalidSection")) 
      }  
    }
  }

  def setGrMatch(coId: Long, coPhId: Int, elem: HTMLElement, m: MEntryGr)(implicit trny: Tourney) = {
    val (grpName, wgw) = (getGroupName(m.grId), s"${m.wgw._1}-${m.wgw._2}")
    elem.innerHTML = clientviews.organize.competition.input.html.MatchEntry(
      grpName, wgw,
      SNO(m.stNoA).getName(trny.comps(coId).typ, getMsg("bye")), 
      SNO(m.stNoB).getName(trny.comps(coId).typ, getMsg("bye")), 
      m.gameNo, m.info, m.getPlayfield, m.result.split('·'), 
      m.sets, trny.cophs(coId,coPhId).noWinSets).toString
  }

  def setKoMatch(coId: Long, coPhId: Int, m: MEntryKo)(implicit trny: Tourney)  = {
    debug("setKoMatch", s"coId: ${coId} coPhId: ${coPhId} round: ${m.round}")
    val tableElem = getElemById(s"InputRound_${coId}_${coPhId}_${m.round}")
    if (tableElem == null) debug("setKoMatch", "tableElem NULL")
    val nameA = tableElem.querySelector(s"[data-game_${m.gameNo}='nameA']").asInstanceOf[HTMLElement]
    val nameB = tableElem.querySelector(s"[data-game_${m.gameNo}='nameB']").asInstanceOf[HTMLElement]
    if (nameA == null) debug("setKoMatch", "NULL")
    setHtml(nameA, SNO(m.stNoA).getName(trny.comps(coId).typ))
    setHtml(nameB, SNO(m.stNoB).getName(trny.comps(coId).typ))
  }

}
