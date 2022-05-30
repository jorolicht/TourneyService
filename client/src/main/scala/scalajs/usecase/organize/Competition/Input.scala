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

    // generate cards for every competition phase if possible,
    // e.g. a competition is selected and it has competition phases/sections
    val coId = AppEnv.getCoId
    if (coId > 0) {
      // generate list of tuple (phase name, phase Id )
      val coPhNameIds = App.tourney.cophs.filter(x => x._1._1 == coId).values.map(x => (x.name, x.coPhId)).toList
      if (coPhNameIds.length == 0) {
        setMainContent(showAlert(getMsg("noSection"))) 
      } else {
        setMainContent(clientviews.organize.competition.html.InputCard( coPhNameIds ).toString)
        coPhNameIds.map { case (name, id) => 
          val elem    = getElemById("Content").querySelector(s"[data-navId='${id}']")
          val size    = Trny.cophs(coId, id).size
          val coPhTyp = Trny.cophs(coId, id).coPhTyp
          // generate input frame
          coPhTyp match {
            case CPT_GR  => {
             elem.innerHTML = "input for groups"
            }  
            case CPT_KO => {
              val maxRnd = genKORnds(size)
              elem.innerHTML = clientviews.organize.competition.input.html.KoCard(coId, id, size, maxRnd).toString
              setInputContent(coId, id, maxRnd)(App.tourney)
            }  
            case CPT_SW => elem.innerHTML = "input for switz-system"
            case _      => elem.innerHTML = showAlert(getMsg("invalidSection"))
          }
          // generate draw input
          // setDrawContent(coId, id)(App.tourney)
        }  
      }
    } else {
      setMainContent(showAlert(getMsg("noSelection"))) 
    }
  }


@JSExport 
  override def actionEvent(key: String, elem: dom.raw.HTMLElement, event: dom.Event) = {
    debug("actionEvent", s"key: ${key}")
    key match {

      case "ClickNavElement" => clickNavigation(elem.getAttribute("data-navId"))

      case _                 => { debug("actionEvent(error)", s"unknown key: ${key} event: ${event.`type`}") }
    }
  }

  
  // control register 
  def clickNavigation(navId: String) = {
    val aNodes = getElemById("Links").getElementsByTagName("a")
    for( i <- 0 to aNodes.length-1) {
      if (aNodes.item(i).asInstanceOf[HTMLElement].getAttribute("data-navId") == navId) {
        aNodes.item(i).asInstanceOf[HTMLElement].classList.add("active")
      } else {
        aNodes.item(i).asInstanceOf[HTMLElement].classList.remove("active")
      }
    }  

    val contentNodes = getElemById("Content").getElementsByTagName("section")
    for( i <- 0 to contentNodes.length-1) {
      val elem = contentNodes.item(i).asInstanceOf[HTMLElement]
      elem.style.display = if (elem.getAttribute("data-navId") == navId) "block" else "none"
    }
  }

  //set view for draw, input player list with (pos, SNO, Name, Club, TTR)
  def setInputContent(coId: Long, coPhId: Int, maxRnd: Int)(implicit trny: Tourney) = {
    val matchMap = trny.cophs(coId,coPhId).matches.groupBy(mEntry=>mEntry.round)
    for (rnd <- maxRnd to 0 by -1) {
      val tableElem = s"InputRound_${coId}_${coPhId}_${rnd}" 
      val matches = matchMap(rnd).sortBy(mEntry => mEntry.gameNo)
      for (m <- matches) {
        val pantA = SNO(m.stNoA).getInfo(trny.comps(coId).typ)
        val pantB = SNO(m.stNoB).getInfo(trny.comps(coId).typ)
        val rowElem = getElemById(tableElem).asInstanceOf[HTMLTableElement].insertRow(-1)
        rowElem.innerHTML = clientviews.organize.competition.input.html.KoMatchEntry(
          pantA._2, pantB._2, m.gameNo, m.info, m.playfield, m.getBallArr, m.printSets, 3).toString
        // rowElem.innerHTML = clientviews.organize.competition.input.html.KoMatchEntry(
        //   "SpielerA", "SpielerB", 3, "", "X", Array("3","4","5"), "3:0", 3).toString
        //rowElem.innerHTML = "xxx"

      }
    }
  }  

  
}
