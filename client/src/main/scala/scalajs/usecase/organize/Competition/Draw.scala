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

  //implicit val trny = App.tourney

  def render(param: String = "", ucInfo: String = "", reload: Boolean=false) = {
    
    // setMainContent(s"OrganizeCompetitionDraw coId: ${AppEnv.getCoId}")
    // generate cards for every competition section if a competition is selected
    val coId = AppEnv.getCoId
    if (coId > 0) {
      // old: generate list of tuple (section name, section Id )
      // generate list of tuple (phase name, phase Id )
      val coPhNameIds = App.tourney.cophs.filter(x => x._1._1 == coId).values.map(x => (x.name, x.coPhId)).toList
      //val compSects = App.tourney.coSects.filter(x => x._1._1 == coId).values.map(x => (x.name, x.id)).toList
      if (coPhNameIds.length == 0) {
        setMainContent(showAlert(getMsg("noSection"))) 
      } else {
        setMainContent(clientviews.organize.competition.html.DrawCard( coPhNameIds ).toString)
        coPhNameIds.map { case (name, id) => 
          val elem    = getElemById("Content").querySelector(s"[data-navId='${id}']")
          val size    = Trny.cophs(coId, id).size
          val coPhTyp = Trny.cophs(coId, id).coPhTyp
          // generate draw frame
          coPhTyp match {
            case CPT_GR  => {
             // generate group configuration list: (grName: String, grId: Int, grSize: Int, pos: Int)
             var pos = 1
             var grpCfg = new ListBuffer[(String,Int,Int,Int)]()
             for (grp <- App.tourney.cophs((coId, id)).groups) {
               grpCfg += ((grp.name, grp.grId, grp.size, pos))
               pos = pos + grp.size 
             }
             elem.innerHTML = clientviews.organize.competition.draw.html.GroupCard(coId, id, grpCfg.toList).toString
            }  
            case CPT_KO => elem.innerHTML = clientviews.organize.competition.draw.html.KOCard(coId, id, size).toString
            case CPT_SW => elem.innerHTML = clientviews.organize.competition.draw.html.SwitzCard(coId, id, size).toString
            case _      => elem.innerHTML = showAlert(getMsg("invalidSection"))
          }
          // generate draw content
          setDrawContent(coId, id)(App.tourney)
        }  
      }
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

      case "ClickRegister"  => clickNavigation(elem.getAttribute("data-navId"))

      case _                => { debug("actionEvent(error)", s"unknown key: ${key} event: ${event.`type`}") }
    }
  }



  //set view for draw, input player list with (pos, SNO, Name, Club, TTR)
  def setDrawContent(coId: Long, coPhId: Int)(implicit trny: Tourney) = {
    // first get the base element identified by coId and coPhId
    val elemBase = getElemById(s"Draw_${coId}_${coPhId}")
    
    //println(s"setDrawContent pants: ${trny.cophs(coId, coPhId).pants.toString}")
    val pants = trny.cophs(coId, coPhId).pants.map( _.getInfo(trny.comps(coId).typ))
    println(s"setDrawContent pants: ${pants.toString}")

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


    // val koRowUp = elemBase.querySelectorAll("td[data-koRowUp]")
    // for( i <- 0 to koRowUp.length-1) {
    //   val elem = koRowUp.item(i).asInstanceOf[HTMLElement]
    //   val pos  = elem.getAttribute("data-koRowUp").toInt
    //   if (pos % 2 == 0) {
    //     elem.classList.add("border-bottom")
    //     elem.classList.add("border-right")
    //   }
    //   // elem.innerHTML = i.toString
    //   // elem.nextElementSibling.innerHTML = "x"
    //   // elem.nextElementSibling.nextElementSibling.innerHTML = "y"
    // }  

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