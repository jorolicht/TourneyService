package scalajs.usecase.organize

// Start TestCases in Javascript Console
// Start.testOrgCompDraw("<toId>")

import scala.collection.mutable.{ ArrayBuffer }
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
  import org.scalajs.dom.raw.HTMLElement

  import scala.collection.mutable.ListBuffer

  def render(param: String = "", ucInfo: String = "", reload: Boolean=false) = {
    OrganizeCompetitionTab.render("Draw")
  }


  @JSExport 
  override def actionEvent(key: String, elem: dom.raw.HTMLElement, event: dom.Event) = {
    val (coPhase, coId, coPhId) = getCompEnv(elem) 
    debug("actionEvent", s"key: ${key} coId: ${coId} coPhId: ${coPhId}")
    key match {
      case "DrawRefresh"   => {
        val drawElements = getElemById_(s"Draw_${coId}_${coPhId}").querySelectorAll("td[data-drawPos]")
        val asign = (for ( i <- 0 to drawElements.length-1) yield {
          val elem = drawElements.item(i).asInstanceOf[HTMLElement]
          (elem.getAttribute("data-drawPos").toInt, elem.innerText.toIntOption.getOrElse(0))
        }).unzip
        println(s"${asign._1.mkString(",")} versus ${asign._2.mkString(", ")}")
        val diff = asign._1.toSet.diff(asign._2.toSet)
        if      (diff.size == 0) reassignDraw(App.tourney.cophs(coId, coPhId), asign._1.zip(asign._2).toMap)
        else if (diff.size == 1) DlgBox.showStd(getMsg("change.hdr"), getMsg("change.msg", diff.head.toString), Seq("ok"))
        else                     DlgBox.showStd(getMsg("change.hdr"), getMsg("changex.msg", diff.mkString(",")), Seq("ok"))
      } 
      case "Start"   => {
        App.tourney.cophs((coId,coPhId)).setStatus(CPS_EIN)
        App.execUseCase("OrganizeCompetitionInput", "", "")        
      }
    }
  }

  // set draw page for a competition phase (round), coId != 0 and coPhId != 0
  def setPage(coId: Long, coPhId: Int)(implicit coPhase: CompPhase): Unit = {
    debug("init", s"coId: ${coId} coPhId: ${coPhId}")
    if (!exists_(s"Draw_${coId}_${coPhId}")) {
      // init view
      val elem    = getElemById_(s"DrawContent_${coId}").querySelector(s"[data-coPhId='${coPhId}']").asInstanceOf[HTMLElement]
      val size    = coPhase.size
      val coPhTyp = coPhase.coPhTyp
      // generate draw frame
      coPhTyp match {
        case CPT_GR => setHtml(elem, clientviews.organize.competition.draw.html.GroupCard(coPhase))
        case CPT_KO => setHtml(elem, clientviews.organize.competition.draw.html.KOCard(coPhase))
        case CPT_SW => setHtml(elem, clientviews.organize.competition.draw.html.SwitzCard(coPhase))
        case _      => setHtml(elem, showAlert(getMsg("invalidSection")))
      }
    } else { 
      // update view
      val base = getElemById_(s"Draw_${coId}_${coPhId}").asInstanceOf[HTMLElement]
      coPhase.coPhTyp match {
        case CPT_GR => updateGrView(base, coPhase.groups)
        case CPT_KO => updateKoView(base, coPhase.ko)
        case CPT_SW => {}
        case _      => {}
      }    
    }

    setVisible(gE(s"DrawStartBtn_${coId}_${coPhId}"), (coPhase.status == CompPhase.CPS_AUS))
    setVisibleDataAttr("drawSelectField", (coPhase.status == CompPhase.CPS_AUS))
    //setVisibleDataAttr("drawSelectField", false)
  }

  def setDrawPosition(elem: HTMLElement, pant: ParticipantEntry, pantPos: String="") = try {
    setData(elem, "sno", pant.sno)
    elem.querySelector(s"[data-name]").asInstanceOf[HTMLElement].innerHTML = pant.name
    elem.querySelector(s"[data-club]").asInstanceOf[HTMLElement].innerHTML = pant.club
    elem.querySelector(s"[data-rating]").asInstanceOf[HTMLElement].innerHTML = pant.getRating

    // reset drawpos to original value    
    val drawPosElem = elem.querySelector(s"[data-drawPos]").asInstanceOf[HTMLElement]
    drawPosElem.innerHTML = getData(drawPosElem, "drawPos", "")
  } catch { case _: Throwable => error("setDrawPosition ", s"Pos: ${pantPos} Pant: ${pant.sno} ${pant.name} [${pant.club}]") }

  def setDrawInfo(elem: HTMLElement, drawInfo: (String,Int,Int,Int)) = try {
    val info = if (drawInfo._1 != "") s"${drawInfo._1}[${drawInfo._3}]" else ""
    elem.querySelector(s"[data-drawInfo]").asInstanceOf[HTMLElement].innerHTML = info

    // reset drawpos to original value    
    val drawPosElem = elem.querySelector(s"[data-drawPos]").asInstanceOf[HTMLElement]
    drawPosElem.innerHTML = getData(drawPosElem, "drawPos", "")
  } catch { case _: Throwable => error("setDrawInfo ", s"Pos:") }



  def updateGrView(base: HTMLElement, groups: ArrayBuffer[Group]) =
    for (g <- groups) {
      g.pants.zipWithIndex.foreach { case (pant, index) => {
        val pantBase = base.querySelector(s"[data-pantPos='${g.grId}_${index}']").asInstanceOf[HTMLElement]
        setDrawPosition(pantBase, pant, s"${g.grId}_${index}")
      }}
    } 

  def updateKoView(base: HTMLElement, ko: KoRound) =
    ko.pants.zipWithIndex.foreach { case (pant, index) => {
      val pantBase = base.querySelector(s"[data-pantPos='${index+1}']").asInstanceOf[HTMLElement]
      setDrawPosition(pantBase, pant, s"${index+1}")
      setDrawInfo(pantBase, ko.drawInfo(index))
    }}


  // reassingDraw set new draw
  def reassignDraw(coph: CompPhase, reassign: Map[Int,Int])= {
    val pants = Array.fill[ParticipantEntry](coph.size)(ParticipantEntry("0", "", "", 0, (0,0)))
    val base = getElemById_(s"Draw_${coph.coId}_${coph.coPhId}").asInstanceOf[HTMLElement]   
    coph.coPhTyp match {
      case CPT_GR => {
        coph.groups.foreach { g => 
          g.pants.zipWithIndex.foreach { case (pant, index) => pants(reassign(g.drawPos + index) - 1) = pant }
        }     
        coph.groups.foreach { g => pants.slice(g.drawPos - 1, g.drawPos + g.size - 1).copyToArray(g.pants) }
        updateGrView(base, coph.groups) 
        //pants.zipWithIndex.foreach { case (pant, index) => println(s"[${index}] ${pant.name} ${pant.club} ${pant.getRating}") }
      }
      case CPT_KO => {
        val pantsNew    = ArrayBuffer.fill[ParticipantEntry](coph.size) (ParticipantEntry("0", "", "", 0, (0,0)))
        var drawInfoNew = ArrayBuffer.fill[(String, Int, Int, Int)](coph.size) (("",0,0,0))
        coph.ko.pants.zipWithIndex.foreach { case (pant, index) =>
          pantsNew(reassign(index+1)-1) = pant 
          drawInfoNew(reassign(index+1)-1) = coph.ko.drawInfo(index) 
        } 
        coph.ko.setDraw(pantsNew, drawInfoNew) match {
          case Left(err)   => println("Error setDraw")
          case Right(res)  => updateKoView(base, coph.ko)
        }
      } 
      case _      => {}
    }
  }

}