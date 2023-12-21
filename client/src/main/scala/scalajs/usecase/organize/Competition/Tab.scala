package scalajs.usecase.organize

// Start TestCases in Javascript Console
// test -s compphase -n 1

import scala.concurrent._
import scala.util.{Success, Failure }
import scala.util.matching

import scala.scalajs._
import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"
import org.scalajs.dom                   // from "org.scala-js" %%% "scalajs-dom" % "0.9.3"

import shared.model._
import shared.model.CompPhase._ 
import shared.utils.Validation._ 
import shared.utils.Routines._
import shared.utils._

import scalajs.usecase.component._
import scalajs.usecase.dialog.{ DlgInfo, DlgInputTxt }

import scalajs.service._
import scalajs._


// ***
// Organize Competition Tab/Register
// ***
@JSExportTopLevel("OrganizeCompetitionTab")
object OrganizeCompetitionTab extends UseCase("OrganizeCompetitionTab") with TourneySvc
{
  import org.scalajs.dom.raw.HTMLElement
  import org.scalajs.dom.raw.HTMLTableElement
  import scala.collection.mutable.ListBuffer

  var selSection: String = ""

  def render(section: String = "", ucInfo: String = "", update: Boolean=false) = {
    selSection = section
    
    val coId = App.tourney.getCurCoId

    // get available competition phases and selected phase otherwise 0 (competition not yet started)
    // val coPhNameIds = App.tourney.cophs.filter(x => x._1._1 == coId).values.map(x => (x.name, x.coPhId)).toList
    var coPhId =  App.tourney.getCurCoPhId
    
    debug("render", s"coId: ${coId} coPhId: ${coPhId} section: ${selSection}")
    App.tourney.getCoPh(coId, coPhId) match {
      case Left(err)   => setMainContent(clientviews.organize.competition.html.TabCard(coId, List()))
      case Right(coph) => selFrame(coph, section)
    }
  } 

  def checkCompPhaseName(name: String): Either[String, Boolean] = {
    if (name.length < 2) Left("XXX Länge mindestens 2") else Right(true)
  }


  def initFrame(coId: Long, coPhId: Int, section: String):Unit = {
    val coPhNameIds = App.tourney.cophs.filter(x => x._1._1 == coId).values.map(x => (x.name, x.coPhId)).toList
    // check whether element exists
    if (gE(s"${section}Content_${coId}_${coPhId}") == null) setMainContent(clientviews.organize.competition.html.TabCard(coId, coPhNameIds))
  }


  @JSExport 
  override def actionEvent(key: String, elem: dom.raw.HTMLElement, event: dom.Event) = {
    import shared.utils.Routines._ 
    import org.scalajs.dom.document
    
    val (coph, coId, coPhId) = getCompEnv(elem) 
    debug("actionEvent", s"key: ${key} coId: ${coId} coPhId: ${coPhId}")
    key match {

      case "DrawRefresh"   => {
        val inputNodes = gEqSA(s"Draw_${coId}_${coPhId}", "small[data-drawPos]")
        val result = for( i <- 0 to inputNodes.length-1) yield {
          val elem   = inputNodes.item(i).asInstanceOf[HTMLElement]
          val posOld = elem.getAttribute("data-drawPos").toIntOption.getOrElse(-1)
          val sno    = elem.getAttribute("data-sno")
          val posNew = elem.innerHTML.toIntOption.getOrElse(-2)
          debug("DrawRefresh", s"posOld: ${posOld} posNew: ${posNew}  sno: ${sno}  ")
          (posOld, posNew, (posOld -> posNew))
        } 
        val orgList = result.map(_._1)
        val newList = result.map(_._2)
        val reassign = result.map(_._3).toMap

        val xx = orgList.filterNot(newList.contains)
        if (xx.length > 0) error("DrawRefresh", s"keine Zuordnung: ${xx}")
        debug("DrawRefresh", s"orgList: ${orgList}  newList: ${newList} reassign: ${reassign}")
      }      

      case "SelectCoPh"  => selFrame(coph, selSection)

      case _             => debug("actionEvent(error)", s"unknown key: ${key} event: ${event.`type`}")
    }
  }



  // select a competition phase e.g. click on tab  
  // constraint: coId, coPhId <> 0
  
  def selFrame(coph: CompPhase, section: String) = {
    initFrame(coph.coId, coph.coPhId, section)

    val aNodes = gUE("Links").getElementsByTagName("a")  
    // set register/tab active
    for( i <- 0 to aNodes.length-1) {
      val elem = aNodes.item(i).asInstanceOf[HTMLElement]
      setClass(elem, getData(elem, "coPhId", 0)==coph.coPhId, "active" )
    }  

    // remember selection
    App.tourney.comps(coph.coId).setCurCoPhId(coph.coPhId)
    
    // set relevant tab content visible
    val contentNodes = gEqSA(s"TabContent_${coph.coId}", s"[data-cophContent]")
    for( i <- 0 to contentNodes.length-1) {
      val elem = contentNodes.item(i).asInstanceOf[HTMLElement]
      elem.style.display = if (elem.id == s"${section}Content_${coph.coId}_${coph.coPhId}") "block" else "none"
    }

    if (List(CompPhaseStatus.CFG, CompPhaseStatus.UNKN).contains(coph.status)) 
      OrganizeCompetitionError.setPage(coph)
    else section match {
      case "Draw"    => OrganizeCompetitionDraw.setPage(coph)
      case "Input"   => OrganizeCompetitionInput.setPage(coph)
      case "View"    => OrganizeCompetitionView.setPage(coph)
      case "Referee" => OrganizeCompetitionReferee.setPage(coph)
      case _         => OrganizeCompetitionError.setPage(coph)
    }
  }  

}
