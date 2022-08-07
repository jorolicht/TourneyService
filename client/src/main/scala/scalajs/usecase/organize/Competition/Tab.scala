package scalajs.usecase.organize

// Start TestCases in Javascript Console
// Start.testOrgCompDraw("<toId>")

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
import shared.model.CompPhase._ 
import shared.utils.Validation._ 
import shared.utils.Routines._
import shared.utils._

import scalajs.usecase.component._
import scalajs.service._
import scalajs._


// ***
// Organize Competition Tab/Register
// ***
@JSExportTopLevel("OrganizeCompetitionTab")
object OrganizeCompetitionTab extends UseCase("OrganizeCompetitionTab")  
  with TourneySvc with DrawSvc
{
  import org.scalajs.dom.raw.HTMLElement
  import org.scalajs.dom.raw.HTMLTableElement
  import scala.collection.mutable.ListBuffer

  var tabMode: String = ""

  def render(param: String = "", ucInfo: String = "", update: Boolean=false) = {
    tabMode = param
    
    val coId = AppEnv.getCoId
    val coPhNameIds = App.tourney.cophs.filter(x => x._1._1 == coId).values.map(x => (x.name, x.coPhId)).toList
    debug("render", s"coId: ${coId} param: ${param}")

    // generate cards for competition phase if a competition is selected
    // and competition phases are available
    if (coId > 0 & coPhNameIds.length > 0) { 
      if (!exists(s"Content")) setMainContent(clientviews.organize.competition.html.TabCard(coId, coPhNameIds))
      // at least one competition phase is configured
      var coPhId = AppEnv.coPhIdMap.getOrElse(coId, coPhNameIds.head._2)
      selectCoPh(coId, coPhId, tabMode, update)
    } else {
      setMainContent(showAlert(getMsg("noSelection"))) 
    }
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

      case "DrawRefresh"   => {
        val coId  = elem.getAttribute("data-coId")
        val coPhId = elem.getAttribute("data-coPhId")

        val inputNodes = getElemById(s"Draw_${coId}_${coPhId}").querySelectorAll("small[data-drawPos]")
        val result = for( i <- 0 to inputNodes.length-1) yield {
          val elem   = inputNodes.item(i).asInstanceOf[HTMLElement]
          val posOld = elem.getAttribute("data-drawPos").toIntOption.getOrElse(-1)
          val sno    = elem.getAttribute("data-sno")
          val posNew = elem.innerHTML.toIntOption.getOrElse(-2)
          debug("actionEvent", s"DrawRefresh: posOld: ${posOld} posNew: ${posNew}  sno: ${sno}  ")
          (posOld, posNew, (posOld -> posNew))
        } 
        val orgList = result.map(_._1)
        val newList = result.map(_._2)
        val reassign = result.map(_._3).toMap

        val xx = orgList.filterNot(newList.contains)
        if (xx.length > 0) error("DrawRefresh", s"keine Zuordnung: ${xx}")
        debug("DrawRefresh", s"orgList: ${orgList}  newList: ${newList} reassign: ${reassign}")
      } 

      case "SelectCoPh"  => selectCoPh(getData(elem, "coId", 0L), getData(elem, "coPhId", 0), tabMode)

      case _             => { debug("actionEvent(error)", s"unknown key: ${key} event: ${event.`type`}") }
    }
  }


  // select a competition phase e.g. click on tab  
  def selectCoPh(coId: Long, coPhId: Int, tabMode: String, update:Boolean=false) = {
    val aNodes = getElemById("Links").getElementsByTagName("a")
    
    // set register/tab active
    for( i <- 0 to aNodes.length-1) {
      if (aNodes.item(i).asInstanceOf[HTMLElement].getAttribute("data-coPhId") == coPhId.toString) {
        aNodes.item(i).asInstanceOf[HTMLElement].classList.add("active")
      } else {
        aNodes.item(i).asInstanceOf[HTMLElement].classList.remove("active")
      }
    }  

    // remember selection
    AppEnv.coPhIdMap(coId) = coPhId
    implicit val trny = App.tourney
    tabMode match {
      case "Draw"  => if (update) OrganizeCompetitionDraw.update(coId, coPhId) else OrganizeCompetitionDraw.init(coId, coPhId)
      case "Input" => if (update) OrganizeCompetitionInput.update(coId, coPhId) else OrganizeCompetitionInput.init(coId, coPhId)
      case "View"  => if (update) OrganizeCompetitionView.update(coId, coPhId) else OrganizeCompetitionView.init(coId, coPhId)
    }  

    // set relevant section visible
    val contentNodes = getElemById("Content").getElementsByTagName("section")
    for( i <- 0 to contentNodes.length-1) {
      val elem = contentNodes.item(i).asInstanceOf[HTMLElement]
      elem.style.display = if (getData(elem, "coPhId", 0) == coPhId & 
                               getData(elem, "tabMode", "") == tabMode) "block" else "none"
    }
  }
  
}