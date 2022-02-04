package scalajs.usecase.organize

// Start TestCases
// http://localhost:9000/start?ucName=TestMain&ucParam=OrganizeCompetitionDraw

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
import shared.utils.Constants._ 
import shared.utils.Validation._ 
import shared.utils.Routines._
import shared.utils._

import scalajs.usecase.dialog.{ DlgBox, DlgSpinner, DlgInfo }
import scalajs.usecase.component._
import scalajs.service._
import scalajs._


// ***
// Organize Competition Draw
// ***
@JSExportTopLevel("OrganizeCompetitionDraw")
object OrganizeCompetitionDraw extends UseCase("OrganizeCompetitionDraw")  
  with TourneySvc with DrawSvc with UIComponentServices
{
  import org.scalajs.dom.raw.HTMLElement

  def render(param: String = "", ucInfo: String = "", reload: Boolean=false) = {
    
    //setMainContent("OrganizeCompetitionDraw")
    // generate cards for every competition section if a competition is selected
    val coId = AppEnv.getCoId
    if (coId > 0) {
      // generate list of tuple (section name, section Id )
      val compSects = App.tourney.coSects.filter(x => x._1._1 == coId).values.map(x => (x.name, x.id)).toList
      if (compSects.length == 0) {
        showAlert(getMsg("noSection"))
      } else {
        setMainContent(clientviews.organize.competition.html.DrawCard( compSects ).toString)
        compSects.map { case (name, secId) => 
          val elem = getElemById("Content").querySelector(s"[data-navId='${secId}']")
          val size   = Trny.coSects(coId, secId).size
          val secTyp = Trny.coSects(coId, secId).secTyp
          // generate draw frame
          secTyp match {
            case CST_GRPS3 | CST_GRPS4 | CST_GRPS45 | CST_GRPS5 | CST_GRPS56 | CST_GRPS6  => {
              elem.innerHTML = clientviews.organize.competition.draw.html.GroupCard(coId, secId, genGrpConfig(secTyp, size)).toString
            }  
            case CST_KO => elem.innerHTML = clientviews.organize.competition.draw.html.KOCard(coId, secId, size).toString
            case CST_SW => elem.innerHTML = clientviews.organize.competition.draw.html.SwitzCard(coId, secId, size).toString
            case _      => elem.innerHTML = showAlert(getMsg("invalidSection"))
          }
          // generate draw frame

        }  
      }
    } else {
      showAlert(getMsg("noSelection")) 
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
        val secId = elem.getAttribute("data-secId")

        val inputNodes = getElemById(s"Draw_${coId}_${secId}").querySelectorAll("small[data-drawPos]")
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

      case "ClickRegister"  => clickNavigation(elem.getAttribute("data-navId"))

      case _                => { debug("actionEvent(error)", s"unknown key: ${key} event: ${event.`type`}") }
    }
  }



  //set view for draw, input player list with (pos, SNO, Name, Club, TTR)
  def setDrawContent(coId: Long, secId: Int, size: Int)(implicit trny: Tourney) = {
    // first get the base element identified by coId and secId
    val elemBase = getElemById(s"Draw_${coId}_${secId}")

    val pants = trny.coSects(coId, secId).pants.map( _.getInfo(trny.comps(coId).typ))
    
    //.playerMap = player.map(x=> x._1 -> (x._2, x._3, x._4, x._5) ).toMap

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

  }


  
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
  
}