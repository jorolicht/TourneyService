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
// Organize Competition Draw
// ***
@JSExportTopLevel("OrganizeCompetitionDraw")
object OrganizeCompetitionDraw extends UseCase("OrganizeCompetitionDraw")  
  with TourneySvc with DrawSvc
{
  import org.scalajs.dom.raw.HTMLElement
  import scala.collection.mutable.ListBuffer

  def render(param: String = "", ucInfo: String = "", reload: Boolean=false) = {
    OrganizeCompetitionTab.render("Draw")
  }


  //set view for draw, input player list with (pos, SNO, Name, Club, TTR)
  def setContent(coId: Long, coPhId: Int)(implicit trny: Tourney) = {
    // first get the base element identified by coId and coPhId
    val elemBase = getElemById(s"Draw_${coId}_${coPhId}")
    
    //println(s"setDrawContent pants: ${trny.cophs(coId, coPhId).pants.toString}")
    val pants = trny.cophs(coId, coPhId).pants.map( _.getInfo(trny.comps(coId).typ))
    //println(s"setDrawContent pants: ${pants.toString}")

    // first set the start numbers
    val inputElements = elemBase.querySelectorAll("small[data-drawPos]")
    for( i <- 0 to inputElements.length-1) {
      val elem = inputElements.item(i).asInstanceOf[HTMLElement]
      val pos  = elem.getAttribute("data-drawPos").toInt
      elem.setAttribute("data-sno", pants(pos-1)._1)
    }
    val nameElements = elemBase.querySelectorAll("small[data-drawName]")
    for( i <- 0 to nameElements.length-1) {
      val elem = nameElements.item(i).asInstanceOf[HTMLElement]
      val pos  = elem.getAttribute("data-drawName").toInt
      elem.innerHTML = pants(pos-1)._2
    }
    val clubElements = elemBase.querySelectorAll("small[data-drawClub]")
    for( i <- 0 to clubElements.length-1) {
      val elem = clubElements.item(i).asInstanceOf[HTMLElement]
      val pos  = elem.getAttribute("data-drawClub").toInt
      elem.innerHTML = pants(pos-1)._3
    }

    val ttrElements = elemBase.querySelectorAll("small[data-drawTTR]")
    for( i <- 0 to ttrElements.length-1) {
      val elem = ttrElements.item(i).asInstanceOf[HTMLElement]
      val pos  = elem.getAttribute("data-drawTTR").toInt
      elem.innerHTML = pants(pos-1)._4.toString
    }
    // val koRowDown = elemBase.querySelectorAll("td[data-koRowDown]")
    // for( i <- 0 to koRowDown.length-1) {
    //   val elem = koRowDown.item(i).asInstanceOf[HTMLElement]
    //   val pos  = elem.getAttribute("data-koRowDown").toInt
    //   if (pos % 2 == 1) {
    //     elem.classList.add("border-top")
    //     elem.classList.add("border-right")
    //   } 
    //   // elem.innerHTML = i.toString
    //   // elem.nextElementSibling.innerHTML = "x"
    //   // elem.nextElementSibling.nextElementSibling.innerHTML = "y"
    // } 
  }

  // setFrame for a competition, coId != 0 and coPhId != 0
  def setFrame(coId: Long, coPhId: Int, reload: Boolean)(implicit trny: Tourney): Unit = {
    if (!exists(s"Draw_${coId}_${coPhId}") | reload) {
      val elem    = getElemById_(s"DrawContent_${coId}").querySelector(s"[data-coPhId='${coPhId}']")
      val size    = trny.cophs(coId, coPhId).size
      val coPhTyp = trny.cophs(coId, coPhId).coPhTyp
      // generate draw frame
      coPhTyp match {
        case CPT_GR  => {
          // generate group configuration list: (grName: String, grId: Int, grSize: Int, pos: Int)
          var pos = 1
          var grpCfg = new ListBuffer[(String,Int,Int,Int)]()
          for (grp <- trny.cophs((coId, coPhId)).groups) {
            grpCfg += ((grp.name, grp.grId, grp.size, pos))
            pos = pos + grp.size 
          }
          elem.innerHTML = clientviews.organize.competition.draw.html.GroupCard(coId, coPhId, grpCfg.toList).toString
        }  
        case CPT_KO => elem.innerHTML = clientviews.organize.competition.draw.html.KOCard(coId, coPhId, size).toString
        case CPT_SW => elem.innerHTML = clientviews.organize.competition.draw.html.SwitzCard(coId, coPhId, size).toString
        case _      => elem.innerHTML = showAlert(getMsg("invalidSection"))
      }
    }
  }  
  
}