package scalajs.usecase.organize

import scala.collection.mutable.{ ArrayBuffer, ListBuffer }

import scala.concurrent._
import scala.util.{Success, Failure }
import scala.util.matching

import scala.scalajs._
import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"
import org.scalajs.dom                   // from "org.scala-js" %%% "scalajs-dom" % "0.9.3"
import org.scalajs.dom.raw.HTMLElement
import upickle.default._

import shared.model._
import shared.model.CompPhase._ 
import shared.utils.Validation._ 
import shared.utils.Routines._
import shared.utils.Constants._
import shared.utils._

import scalajs.usecase.dialog.{ DlgBox }
import scalajs.usecase.component._
import scalajs.service._
import scalajs._

// ***
// Organize Competition Draw
// ***
@JSExportTopLevel("OrganizeCompetitionDraw")
object OrganizeCompetitionDraw extends UseCase("OrganizeCompetitionDraw")  
  with TourneySvc
{

  def render(param: String = "", ucInfo: String = "", reload: Boolean=false) = {
    OrganizeCompetitionTab.render("Draw")
  }

  @JSExport 
  override def actionEvent(key: String, elem: dom.raw.HTMLElement, event: dom.Event) = {
    val (coPhase, coId, coPhId) = getCompEnv(elem) 
    //debug("actionEvent", s"key: ${key} coId: ${coId} coPhId: ${coPhId}")
    key match {
      case "DrawRefresh"   => {
        val drawElements = gE(s"DrawContent_${coId}_${coPhId}").querySelectorAll("td[data-drawPos]")
        val asign = (for ( i <- 0 until drawElements.length) yield {
          val elem = drawElements.item(i).asInstanceOf[HTMLElement]
          (getData(elem, "drawPos", 0), elem.innerText.toIntOption.getOrElse(0))
        }).unzip
        //debug("DrawRefresh, s"${asign._1.mkString(",")} versus ${asign._2.mkString(",")}")
        val diff = asign._1.toSet.diff(asign._2.toSet)
        if      (diff.size == 0) reassignDraw(App.tourney.cophs(coId, coPhId), asign._1.zip(asign._2).toMap)
        else if (diff.size == 1) DlgBox.standard(getMsg("change.hdr"), getMsg("change.msg", diff.head.toString), Seq("ok"))
        else                     DlgBox.standard(getMsg("change.hdr"), getMsg("changex.msg", diff.mkString(",")), Seq("ok"))
      } 

      case "Start"   => updateCompPhaseStatus(coId, coPhId, CompPhaseStatus.EIN) map {
        case Left(err)   => error("StartInputCoPh", s"${err}") 
        case Right(res)  => {
          setVisible(gE(s"DrawStartBtn_${coId}_${coPhId}"), false)
          App.execUseCase("OrganizeCompetitionInput", "", "")
        }    
      }



    }
  }

  // set draw page for a competition phase (round), coId != 0 and coPhId != 0
  def setPage(coph: CompPhase): Unit = {
    val contentElement = gE(s"DrawContent_${coph.coId}_${coph.coPhId}")
    if (contentElement.innerHTML == "") {
      val compTyp = App.tourney.getCompTyp(coph.coId)
      debug("setPage", s"Draw init: coId: ${coph.coId} coPhId: ${coph.coPhId}")
      coph.getTyp match {
        case CompPhaseTyp.GR => setHtml(contentElement, clientviews.organize.competition.draw.html.GroupCard(coph, compTyp))
        case CompPhaseTyp.KO => setHtml(contentElement, clientviews.organize.competition.draw.html.KOCard(coph, compTyp))
        case CompPhaseTyp.RR => setHtml(contentElement, clientviews.organize.competition.draw.html.RRCard(coph, compTyp))        
        case CompPhaseTyp.SW => {
          val mtchRnds = coph.matches.toList.groupBy(_.round)
          setHtml(contentElement, clientviews.organize.competition.draw.html.SwissCard(coph, mtchRnds))
        }  
        case _               => setHtml(contentElement, showAlert(getMsg("invalidSection")))
      }      
    } else {
      //debug("setPage", s"Draw update: coId: ${coph.coId} coPhId: ${coph.coPhId}")
      val base = gE(s"DrawContent_${coph.coId}_${coph.coPhId}")
      coph.getTyp match {
        case CompPhaseTyp.GR  => updateGrView(base, coph.groups, App.tourney.comps(coph.coId).typ)
        case CompPhaseTyp.RR  => updateRrView(base, coph.groups(0), App.tourney.comps(coph.coId).typ)
        case CompPhaseTyp.KO  => updateKoView(base, coph.ko, App.tourney.comps(coph.coId).typ)
        case CompPhaseTyp.SW  => ???
        case _                => {}
      }       
    }

    setVisible(gE(s"DrawStartBtn_${coph.coId}_${coph.coPhId}"), (coph.status == CompPhaseStatus.AUS))
    setVisibleDataAttr("drawSelectField", (coph.status == CompPhaseStatus.AUS))
  }

  def setDrawPosition(elem: HTMLElement, pant: Pant, coTyp: CompTyp.Value, pantPos: String="") = try {
    setData(elem, "sno", pant.sno)
    setHtml(elem.querySelector(s"[data-name]").asInstanceOf[HTMLElement], pant.getName(getMsg("bye")))
    setHtml(elem.querySelector(s"[data-club]").asInstanceOf[HTMLElement], pant.club)
    setHtml(elem.querySelector(s"[data-rating]").asInstanceOf[HTMLElement], pant.getRatingInfo)
    setHtml(elem.querySelector(s"[data-qInfo]").asInstanceOf[HTMLElement], pant.qInfo)

    // reset drawpos to original value    
    val drawPosElem = elem.querySelector(s"[data-drawPos]").asInstanceOf[HTMLElement]
    setHtml(drawPosElem, getData(drawPosElem, "drawPos", ""))
  } catch { case _: Throwable => error("setDrawPosition ", s"Pos: ${pantPos} Pant: ${pant.sno} ${pant.name} [${pant.club}]") }

  def updateGrView(base: HTMLElement, groups: ArrayBuffer[Group], coTyp: CompTyp.Value) =
    for (g <- groups) {
      g.pants.zipWithIndex.foreach { case (pant, index) => {
        val pantBase = base.querySelector(s"[data-pantPos='${g.grId}_${index}']").asInstanceOf[HTMLElement]
        setDrawPosition(pantBase, pant, coTyp, s"${g.grId}_${index}")
      }}
    } 

  def updateRrView(base: HTMLElement, grp: Group, coTyp: CompTyp.Value) =
    grp.pants.zipWithIndex.foreach { case (pant, index) => {
      val pantBase = base.querySelector(s"[data-pantPos='${grp.grId}_${index}']").asInstanceOf[HTMLElement]
      setDrawPosition(pantBase, pant, coTyp, s"${grp.grId}_${index}")
    }}
         

  def updateKoView(base: HTMLElement, ko: KoRound, coTyp: CompTyp.Value) =
    ko.pants.zipWithIndex.foreach { case (pant, index) => {
      val pantBase = base.querySelector(s"[data-pantPos='${index+1}']").asInstanceOf[HTMLElement]
      setDrawPosition(pantBase, pant, coTyp, s"${index+1}")
    }}


  // reassingDraw set new draw
  def reassignDraw(coph: CompPhase, reassign: Map[Int,Int])= {
    val base = gE(s"DrawContent_${coph.coId}_${coph.coPhId}")   
    coph.reassignDraw(reassign, App.tourney.comps(coph.coId).typ)
    coph.getTyp match {
      case CompPhaseTyp.GR => updateGrView(base, coph.groups, App.tourney.comps(coph.coId).typ)
      case CompPhaseTyp.RR => updateRrView(base, coph.groups(0), App.tourney.comps(coph.coId).typ)  
      case CompPhaseTyp.KO => updateKoView(base, coph.ko, App.tourney.comps(coph.coId).typ) 
      case _      => {}
    }
  }
}